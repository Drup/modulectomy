open Owee_elf
module Symbol = Symbol_table.Symbol
open CCOption.Infix

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
          let filename = CCOption.get_or ~default:"" @@ get_filename header state in
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
    match Symbol.type_attribute addr with
    | Symbol.File -> ()
    | _ ->
      match Symbol.name addr tbl with
      | Some name when
          (String.length name >= 9 &&
           (String.(equal (sub name 0 8) "cpu_irq_") ||
            String.(equal (sub name 0 9) "cpu_trap_")) ||
           (String.length name >= 18 &&
            String.(equal (sub name 0 18) "domain_field_caml_")))
        -> ()
      | _ ->
        let v = Symbol.value addr in
        let size = Some (Symbol.size_in_bytes addr) in
        let location =
          AddrMap.find_opt v loctbl
        in
        match classify_symb ~tbl addr with
        | None -> ()
        | Some (name, _id, kind) ->
          AddrTbl.add h v (name, Info.mk ~v ?size ?location kind)
  in
  Symbol_table.iter symtbl ~f ;
  (* look for caml_startup_code_begin / code_end / data_begin / data_end  (and caml_system) *)
  (* remove all address that match the range begin..end *)
  let find_begin_end modname prefix =
    let data = ref (None, None) in
    let f symbol =
      match classify_symb ~tbl symbol with
      | Some ([ "OCaml" ; name ], _, _)
        when String.equal name (modname ^ "__" ^ prefix ^ "_begin") ->
        data := (Some symbol, snd !data)
      | Some ([ "OCaml" ; name ], _, _)
        when String.equal name (modname ^ "__" ^ prefix ^ "_end") ->
        data := (fst !data, Some symbol)
      | _ -> ()
    in
    Symbol_table.iter symtbl ~f;
    Symbol.value (Option.get (fst !data)), Symbol.value (Option.get (snd !data))
  in
  let startup_code = find_begin_end "startup" "code"
  and startup_data = find_begin_end "startup" "data"
  and system_code = find_begin_end "system" "code"
  in
  (* remove symbols with addresses between start..end *)
  let in_range addr =
    (addr >= fst startup_code && addr <= snd startup_code) ||
    (addr >= fst startup_data && addr <= snd startup_data) ||
    (addr >= fst system_code && addr <= snd system_code)
  in
  AddrTbl.filter_map_inplace
    (fun addr v -> if in_range addr then None else Some v)
    h;
  (* remove the code_begin/code_end/data_begin/data_end symbols *)
  AddrTbl.remove h (fst startup_code); AddrTbl.remove h (snd startup_code);
  AddrTbl.remove h (fst startup_data); AddrTbl.remove h (fst startup_data);
  AddrTbl.remove h (fst system_code); AddrTbl.remove h (snd system_code);
  (* finally, add one symbol for startup and one for system *)
  let startup_size =
    Int64.add
      (Int64.sub (snd startup_code) (fst startup_code))
      (Int64.sub (snd startup_data) (fst startup_data))
  and system_size =
    Int64.sub (snd system_code) (fst system_code)
  in
  AddrTbl.add h (fst startup_code)
    (["OCaml" ; "startup"], Info.mk ~size:startup_size Info.Module);
  AddrTbl.add h (fst system_code)
    (["OCaml" ; "system"], Info.mk ~size:system_size Info.Module);
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


