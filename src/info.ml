type location = string * int * int
let pp_location ppf (file, line, col) =
  Format.fprintf ppf "%s: %i,%i" file line col

type size = int64

type kind =
  | Value
  | Function
  | Module
  | Functor
  | Primitive
  | Unknown

let to_string = function
  | Value -> "value"
  | Function -> "function"
  | Module -> "module"
  | Functor -> "functor"
  | Primitive -> "primitive"
  | Unknown -> "unknown"

type data = {
  size : size option;
  location : location option ;
  kind : kind ;
  v : Int64.t option ;
}
let mk ?v ?size ?location kind = { v ; size ; location ; kind }
let coalesce data1 data2 =
  let open CCOption.Infix in
  let size = Int64.add <$> data1.size <*> data2.size in
  let location = data1.location <+> data2.location in
  (* assert (data1.kind = data2.kind); *)
  let kind = data1.kind in
  { v = data1.v ; size ; location ; kind }

let pp_data ppf d =
  Format.fprintf ppf "%s %a %a v %a"
    (to_string d.kind)
    CCFormat.(opt int64) d.size
    (CCFormat.opt pp_location) d.location
    CCFormat.(opt int64) d.v

type name = string
type lid = name list


module SMap = CCMap.Make(String)
module T = struct

  type t = T of node SMap.t
  and node =
    { value : data ; children : t }

  let rec pp ppf (T t) =
    let pp_node ppf { value ; children } =
      Format.fprintf ppf "%a, children: %a@."
        pp_data value pp children
    in
    SMap.iter (fun k v ->
        Format.fprintf ppf "key %s node %a@." k pp_node v)
      t

  let empty = T SMap.empty
  
  let rec insert (T t) l x = match l with
    | [] -> assert false
    | name::rest ->
      T (match SMap.find_opt name t with
      | None ->
        SMap.add name (singleton rest x) t
      | Some node ->
        SMap.add name (insert_node node rest x) t)

  and insert_node node l x = match l with
    | [] ->
      {node with value = coalesce node.value x}
    | l ->
      let children = insert node.children l x in
      {node with children}
  
  and singleton l v = match l with
    | [] -> { value = v ; children = empty }
    | name::rest ->
      let value = mk Module in
      {value ; children = T (SMap.singleton name (singleton rest v)) }

  let of_iter l : t =
    Iter.fold (fun t (k,v) -> insert t k v) empty l

  let rec union (T t1) (T t2) =
    T (SMap.union union_node t1 t2)
  and union_node _ v1 v2 =
    Some {
      value = coalesce v1.value v2.value ;
      children = union v1.children v2.children ;
    }
end

type t = T.t

let import = T.of_iter

let adjust_size ~total ~children_size =
  match total with
  | None -> children_size, None
  | Some s ->
    assert (s >= children_size);
    let size = Int64.sub s children_size in
    s, Some size

let rec diff_size_tree ((T.T t) : t) =
  let aux v tree (total_size, trees) =
    let size, tree = diff_size_node v tree in
    let t = match tree with None -> trees | Some t -> SMap.add v t trees in
    Int64.add size total_size, t
  in
  let total_size, trees = SMap.fold aux t (0L,SMap.empty) in
  total_size, T.T trees
and diff_size_node v T.{ value; children } =
  match value.kind with
  | Module ->
    begin
      let (T child_nodes) = children in
      match
        SMap.find_opt "code_begin" child_nodes, SMap.find_opt "code_end" child_nodes,
        SMap.find_opt "data_begin" child_nodes, SMap.find_opt "data_end" child_nodes
      with
      | Some cb, Some ce, Some db, Some de ->
        Printf.printf "hit module %s\n" v;
        let code_size =
          Int64.sub (Option.get ce.value.v) (Option.get cb.value.v)
        and data_size =
          Int64.sub (Option.get de.value.v) (Option.get db.value.v)
        in
        let code_size = match SMap.find_opt "primitives" child_nodes with
          | None -> code_size
          | Some x ->
            let s = Option.get x.value.size in
            Printf.printf "%Lu bytes primitives\n" s;
            Int64.add code_size s
        in
        let code_value = { cb.value with size = Some code_size }
        and data_value = { db.value with size = Some data_size }
        in
        let c_size, T.T more_children = diff_size_tree children in
        let children =
          SMap.add "code" { T.value = code_value ; children = T.empty }
            (SMap.singleton "data" { T.value = data_value ; children = T.empty })
        in
        let children = SMap.union (fun _ a _ -> Some a) children more_children in
        let children = T.T children in
        let size = Int64.(add c_size (add code_size data_size)) in
        let value = { value with v = None ; size = Some size } in
        size, Some T.{ value ; children }
      | _ ->
        let children_size, children = diff_size_tree children in
        let total_size, size = adjust_size ~total:value.size ~children_size in
        let value = {value with size} in
        total_size, Some T.{ value; children }
    end
  | Value -> 0L, None
  | k ->
    Printf.printf "skipping kind %s (%s)\n" (to_string k) v;
    0L, None

let diff_size t =
  snd @@ diff_size_tree t

(* let rec find_ranges ranges ((T.T t) : t) =
 *   let aux name tree ranges = find_range_node ranges name tree in
 *   SMap.fold aux t ranges
 * and find_range_node ranges _name T.{ value ; children } =
 *   let ranges = find_ranges ranges children in
 *   match value.v, value.size with
 *   | Some our_start, Some size when size > 0L ->
 *     let our_stop = Int64.add our_start size in
 *     let added, ranges = 
 *       List.fold_left (fun (added, acc) (start, stop) ->
 *           if start <= our_start && our_stop <= stop then begin
 *             true, (start, stop) :: acc
 *           end else if our_stop = start then
 *             true, (our_start, stop) :: acc
 *           else if stop = our_start then
 *             true, (start, our_stop) :: acc
 *           else if start <= our_stop && our_stop <= stop then begin
 *             assert (our_start <= start);
 *             true, (our_start, stop) :: acc
 *           end else if start <= our_start && our_start <= stop then begin
 *             assert (our_stop >= stop);
 *             true, (start, our_stop) :: acc
 *           end else if our_start <= start && our_stop >= stop then begin
 *             true, if added then acc else (our_start, our_stop) :: acc
 *           end else
 *             added, (start, stop) :: acc)
 *         (false, []) ranges
 *     in
 *     if added then ranges else (our_start, our_stop) :: ranges
 *   | _ -> ranges *)

let find_ranges t =
  let rec find_range acc ((T.T t) : t) =
    let aux name tree ranges = find_range_node ranges name tree in
    SMap.fold aux t acc
  and find_range_node acc name T.{ value ; children } =
    let acc = find_range acc children in
    match value.v, value.size with
    | Some our_start, Some size when size > 0L ->
      let our_stop = Int64.add our_start size in
      (our_start, our_stop, name) :: acc
    | _ -> acc
  in
  let triplets = find_range [] t in
  let sorted_triplets = List.sort (fun (a, a', _) (b, b', _) ->
      match compare a b with 0 -> compare b' a' | x -> x)
      triplets
  in
  let space = 16L in
  let rec merge_consequtive = function
    | (start, stop, n as t0) :: (start', stop', _n' as t1) :: tail ->
      if Int64.add stop space >= start' && start <= start' then
        let t = (start, max stop stop', n) in
        merge_consequtive (t :: tail)
      else if Int64.add start space <= stop' && stop >= stop' then
        let t = (min start start', stop, n) in
        merge_consequtive (t :: tail)
      else
        t0 :: merge_consequtive (t1 :: tail)
    | [] | _ :: [] as l -> l
  in
  merge_consequtive sorted_triplets

(*
    start..stop
         ^^ here  --> skip it, DONE (1 in the conditional)
^-^^           ^^---^^
   here          here --> extend the start..stop range (2nd and 3rd)
 ^^------^^ here (start <= our_stop <= stop) (4th)
            ^^------^^ here (start <= our_start <= stop) (5th)
 ^^--------------^^ here (our_start <= start && our_stop >= stop) (6th)
^^               ^^
here             here          --> don't do anything
*)

let prefix_filename ((T.T t) : t) =
  let add map prefix data =
    SMap.update prefix (function
        | None -> Some data
        | Some data' -> T.union_node prefix data data'
      ) map
  in
  let f prefix data map =
    match data.T.value with
    | {location = None ; _ }  ->
      let i = "<unknown>" in
      let value = mk Unknown in
      let data = T.{value ; children = T (SMap.singleton prefix data)} in
      add map i data
    | {location = Some (file, _,_) ; _} ->
      let modname =
        Fpath.(filename @@ v file)
      in
      let value = mk ~location:(file,-1,-1) Module in
      let data = T.{value ; children = T (SMap.singleton prefix data)} in
      add map modname data
  in
  T.T (SMap.fold f t SMap.empty)



let rec compute_area ?(size=0L) ((T.T t) : t) =
  let aux _ T.{ value; children } x =
    let s = compute_area ?size:value.size children in
    Int64.add x s
  in
  SMap.fold aux t size

let rec cut_tree n ((T.T t) : t) =
  let aux tree = cut_node n tree in
  T.T (SMap.map aux t)
and cut_node n T.{ value; children } =
  if n > 0 then
    let children = cut_tree (n-1) children in
    T.{ value; children }
  else
    let size = compute_area ?size:value.size children in
    let value = {value with size = Some size} in
    T.{ value; children = T.T SMap.empty }

let cut n t = cut_tree n t
