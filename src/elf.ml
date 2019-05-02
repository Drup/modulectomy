open Owee_elf
module Symbol = Symbol_table.Symbol
open CCOpt.Infix

let re_classify_caml =
  let open Tyre in
  let mk_id s =
    shortest @@ regex @@ Re.Posix.re s
  in
  let mid = mk_id "[A-Z][a-zA-Z0-9_$]*" in
  let id = mk_id  "[a-zA-Z0-9_$]+" in
  let final_id =
    mid
    <|> (id <&> opt (str"_" *> pos_int))
  in
  let caml_lid =
    str "caml" *> terminated_list ~sep:(str"__") mid <&> final_id
  in
  let runtime_id = str "caml_" *> id in 
  (* let unknown_caml_id = str "caml" *> pcre ".*" in *)

  let (-->) re f = whole_string re --> f in
  route [
    runtime_id --> (fun s -> ([s], None, Info.Primitive));
    caml_lid --> (fun (l,s) -> match s with
        | `Left s -> l@[s], None, Info.Module
        | `Right (s, id) -> l@[s], id, Info.Value
      );
    (* unknown_caml_id --> (fun s -> ([s], Info.Unknown)); *)
  ]
  
let annot_kind k ty = match k, ty with
  | Info.Value, Symbol.Func -> Info.Function
  | Module, Symbol.Func -> Functor
  | _ -> k

let classify_symb ~tbl symb =
  Symbol.name symb tbl >>= fun name ->
  let id = CCResult.to_opt @@ Tyre.exec re_classify_caml name in
  match id with
  | Some (s, id, k) -> Some ("OCaml"::s, id, annot_kind k @@ Symbol.type_attribute symb)
  | None -> Some ([name], None, Info.Unknown)



module AddrMap = Map.Make(Int64)
module AddrTbl = CCHashtbl.Make(CCInt64)

let mk_location_tbl buffer sections =
  Owee_elf.find_section sections ".debug_line" >|= fun section ->
  let body = Owee_buf.cursor (Owee_elf.section_body buffer section) in
  let rec aux tbl =
    match Owee_debug_line.read_chunk body with
    | None -> tbl
    | Some (header, chunk) ->
      let check header state tbl =
        let open Owee_debug_line in
        if state.end_sequence then
          tbl
        else
          let filename = CCOpt.get_or ~default:"" @@ get_filename header state in
          AddrMap.add
            (Int64.of_int state.address)
            (filename, state.line, state.col)
            tbl
      in
      let tbl = Owee_debug_line.fold_rows (header, chunk) check tbl in
      aux tbl
  in
  aux AddrMap.empty

let mk_info_tbl buffer sections =
  mk_location_tbl buffer sections >>= fun loctbl ->
  Owee_elf.find_symbol_table buffer sections >>= fun symtbl ->
  Owee_elf.find_string_table buffer sections >|= fun tbl ->
  let h = AddrTbl.create 17 in
  let f addr =
    let v = Symbol.value addr in
    let size = Some (Symbol.size_in_bytes addr) in
    let location =
      AddrMap.find_opt v loctbl
    in
    match classify_symb ~tbl addr with
    | None -> ()
    | Some (name, id, kind) ->
      AddrTbl.add h v (name, {Info. id; kind; size; location})
  in
  Symbol_table.iter symtbl ~f ;
  h
  
let mk_buffer path = 
  let fd = Unix.openfile path [Unix.O_RDONLY] 0 in
  let len = Unix.lseek fd 0 Unix.SEEK_END in
  let map =
    Unix.map_file fd
      Bigarray.int8_unsigned Bigarray.c_layout false [|len|]
    |> Bigarray.array1_of_genarray
  in
  Unix.close fd;
  map

let get path =
  let buffer = mk_buffer path in
  let _header, sections = Owee_elf.read_elf buffer in
  match mk_info_tbl buffer sections with
  | None -> Error `Invalid_file
  | Some h -> Ok (fun k -> AddrTbl.iter (fun _ x -> k x) h)
