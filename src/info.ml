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
  | Value -> "v"
  | Function -> "f"
  | Module -> "M"
  | Functor -> "F"
  | Primitive -> "P"
  | Unknown -> "U"

type data = {
  size : size ;
  id : int option ;
  location : location option ;
  kind : kind ;
}
let pp_data ppf d =
  Format.fprintf ppf "%s %LiB %a"
    (to_string d.kind) d.size
    (CCFormat.opt pp_location) d.location

type name = string
type lid = name list

module T = CCTrie.MakeList(String)

type t = data T.t
let of_iter k =
  let f m k v = T.add k v m in
  Iter.fold2 f T.empty k

let pp ppf x =
  let ktree = T.to_tree x in
  let f = function
    | `Char c -> `Text c
    | `Switch -> `Empty
    | `Val v -> `Text (Format.asprintf "%a" pp_data v)
  in
  let pb = PrintBox.Simple.(to_box @@ map_ktree f ktree) in
  PrintBox_text.pp ppf pb

(* type ('a, 'b) tree =
 *   | Node of 'a * ('a, 'b) tree SMap.t
 *   | Leaf of 'b
 * 
 * let rec insert t l x = match l, t with
 *   | _, Leaf _ ->
 *     invalid_arg "Conflicting name"
 *   | [], _ -> assert false
 *   | name::rest, Node ((), m) ->
 *     let m'= match SMap.find_opt name m with
 *       | None ->
 *         SMap.add name (singleton rest x) m
 *       | Some t' ->
 *         SMap.add name (insert t' rest x) m
 *     in
 *     Node ((), m')
 * 
 * and singleton l x = match l with
 *   | [s] -> Leaf (s, x)
 *   | l -> insert (Node ((), SMap.empty)) l x
 * 
 * let rec map acc f g = function
 *   | Node (x, m) ->
 *     let m = SMap.mapi (fun name -> map (name::acc) f g) m in
 *     Node (f (List.rev acc) x, m)
 *   | Leaf v -> f (List.rev (name::acc)
 * let map_node f t = map_node [] f t *)
