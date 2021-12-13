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

let rec diff_size_tree ?(n = "") ((T.T t) : t) =
  let aux v tree (total_size, trees) =
    let size, tree = diff_size_node (n ^ ":" ^ v) tree in
    let t = match tree with None -> trees | Some t -> SMap.add v t trees in
    Int64.add size total_size, t
  in
  let total_size, trees = SMap.fold aux t (0L,SMap.empty) in
  total_size, T.T trees
and diff_size_node v T.{ value; children } =
  match value.kind with
  | Module ->
    Printf.printf "module %s\n" v;
    begin
      let (T child_nodes) = children in
      match SMap.find_opt "code" child_nodes with
      | Some code ->
        (* Printf.printf "hit module %s\n" v; *)
        let data = SMap.find_opt "data" child_nodes in
        let prims = SMap.find_opt "primitives" child_nodes in
        let others =
          SMap.remove "data"
            (SMap.remove "code"
               (SMap.remove "primitives" child_nodes))
        in
        let c_size, T.T more_children = diff_size_tree ~n:v (T.T others) in
        let children = SMap.singleton "code" code in
        let children =
          Option.fold
            ~none:children
            ~some:(fun p -> SMap.add "data" p children)
            data
        in
        let children =
          Option.fold
            ~none:children
            ~some:(fun p -> SMap.add "primitives" p children)
            prims
        in
        let children = SMap.union (fun _ a _ -> Some a) children more_children in
        let children = T.T children in
        let size = Option.get code.value.size in
        let size = Option.fold ~none:size ~some:(fun p -> Int64.add (Option.get p.T.value.size) size) data in
        let size = Option.fold ~none:size ~some:(fun p -> Int64.add (Option.get p.T.value.size) size) prims in
        let size = Int64.add size c_size in
        let value = { value with v = None } in
        size, Some T.{ value ; children }
      | None ->
        match SMap.find_opt "primitives" child_nodes with
        | Some p ->
          let others = SMap.remove "primitives" child_nodes in
          let c_size, T.T more_children = diff_size_tree ~n:v (T.T others) in
          let children = SMap.singleton "primitives" p in
          let children = SMap.union (fun _ a _ -> Some a) children more_children in
          let children = T.T children in
          let size = Option.get p.value.size in
          let size = Int64.add size c_size in
          let value = { value with v = None } in
          size, Some T.{ value ; children }
        | None ->
          let children_size, children = diff_size_tree children in
          let total_size, size = adjust_size ~total:value.size ~children_size in
          let value = {value with size} in
          total_size, Some T.{ value; children }
    end
  | Value ->
    Printf.printf "value (%s)\n" v;
    0L, None
  | k ->
    Printf.printf "skipping kind %s (%s)\n" (to_string k) v;
    0L, None

let diff_size t =
  snd @@ diff_size_tree t

let find_ranges t =
  let rec find_range acc name ((T.T t) : t) =
    let aux name' tree ranges = find_range_node ranges (name ^ ":" ^ name') tree in
    SMap.fold aux t acc
  and find_range_node acc name T.{ value ; children } =
    let acc = find_range acc name children in
    match value.v, value.size with
    | Some our_start, Some size when size > 0L ->
      let our_stop = Int64.add our_start size in
      (our_start, our_stop, name) :: acc
    | _ -> acc
  in
  let triplets = find_range [] "" t in
  let sorted_triplets = List.sort (fun (a, a', _) (b, b', _) ->
      match compare a b with 0 -> compare b' a' | x -> x)
      triplets
  in
  sorted_triplets
(*  let space = 0L in
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
    merge_consequtive sorted_triplets *)

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
