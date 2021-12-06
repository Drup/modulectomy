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
    let size, tree = diff_size_node tree in
    Int64.add size total_size, SMap.add v tree trees
  in
  let total_size, trees = SMap.fold aux t (0L,SMap.empty) in
  total_size, T.T trees
and diff_size_node T.{ value; children } =
  let (T child_nodes) = children in
  match
    SMap.find_opt "code_begin" child_nodes, SMap.find_opt "code_end" child_nodes,
    SMap.find_opt "data_begin" child_nodes, SMap.find_opt "data_end" child_nodes
  with
  | Some cb, Some ce, Some db, Some de ->
    let size =
      Int64.add
        (Int64.sub (Option.get ce.value.v) (Option.get cb.value.v))
        (Int64.sub (Option.get de.value.v) (Option.get db.value.v))
    in
    let size = match SMap.find_opt "primitives" child_nodes with
      | None -> size
      | Some x -> Int64.add size (Option.get x.value.size)
    in
    let value = { value with size = Some size } in
    size, T.{ value ; children = T.empty }
  | _ ->
    let children_size, children = diff_size_tree children in
    let total_size, size = adjust_size ~total:value.size ~children_size in
    let value = {value with size} in
    total_size, T.{ value; children }

let diff_size t =
  snd @@ diff_size_tree t


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
