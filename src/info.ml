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
  id : int option ;
  location : location option ;
  kind : kind ;
}
let pp_data ppf d =
  Format.fprintf ppf "%s %a %a"
    (to_string d.kind)
    CCFormat.(opt int64) d.size
    (CCFormat.opt pp_location) d.location

type name = string
type lid = name list


module SMap = CCMap.Make(String)
module T = struct

  type 'a t = T of 'a node SMap.t
  and 'a node =
    { value : 'a ; children : 'a t }

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
      { node with value = x :: node.value }
    | l ->
      let children = insert node.children l x in
      {node with children}
  
  and singleton l v = match l with
    | [] -> { value = [v] ; children = empty }
    | name::rest ->
      {value = [] ; children = T (SMap.singleton name (singleton rest v)) }

  let of_iter l : _ t =
    Iter.fold (fun t (k,v) -> insert t k v) empty l

  let rec union (T t1) (T t2) =
    T (SMap.union union_node t1 t2)
  and union_node _ v1 v2 =
    Some {
      value = v1.value @ v2.value ;
      children = union v1.children v2.children ;
    }  
end


let prefix_filename (T.T t) =
  let add map prefix data =
    SMap.update prefix (function
        | None -> Some data
        | Some data' -> T.union_node prefix data data'
      ) map
  in
  let f prefix data map =
    match data.T.value with
    | [] 
    | {location = None ; _ } :: _ ->
      let i = "<unknown>" in
      let v = { kind = Module; location = Some (i,-1,-1); size = None; id = None}
      in
      let data = T.{value = [v] ; children = T (SMap.singleton prefix data)} in
      add map i data
    | {location = Some (file, _,_) ; _} :: _ ->
      let i = Fpath.(filename @@ v file) in
      let v = { kind = Module; location = Some (i,-1,-1); size = None; id = None}
      in
      let data = T.{value = [v] ; children = T (SMap.singleton prefix data)} in
      add map i data
  in
  T.T (SMap.fold f t SMap.empty)
