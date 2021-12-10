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
  Printf.printf "classifying %s\n" name;
  let id = CCResult.to_opt @@ Tyre.exec re_classify_caml name in
  match id with
  | Some (s, id, k) ->
    Printf.printf "%s classified as %s (id %d)\n"
      (Info.to_string k)
      (String.concat "::" s)
      (Option.fold ~none:0 ~some:Fun.id id);
    Some ("OCaml"::s, id, annot_kind k @@ Symbol.type_attribute symb)
  | None ->
    let k = match Symbol.type_attribute symb with
      | Symbol.Func -> Info.Function
      | Symbol.Object -> Info.Value
      | _ -> Info.Unknown
    in
    Printf.printf "not an ocaml symbol %s\n" (Info.to_string k);
    Some ([name], None, k)

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

let filtered_sym name =
  String.length name >= 9 &&
  (String.(equal (sub name 0 8) "cpu_irq_") ||
   String.(equal (sub name 0 9) "cpu_trap_")) ||
  (String.length name >= 18 &&
   String.(equal (sub name 0 18) "domain_field_caml_"))

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
      | Some name when filtered_sym name -> ()
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
  let other_syms = Hashtbl.create 7 in
  let find_syms names =
    let data = ref [] in
    let f symbol =
      match Symbol.name symbol tbl with
      | Some name when List.mem name names ->
        data := (name, symbol) :: !data
      | Some name ->
        Tables.categorize_symbol other_syms name symbol
      | _ -> ()
    in
    Symbol_table.iter symtbl ~f;
    !data
  in
  let syms = find_syms [
      "caml_startup__code_begin"; "caml_startup__code_end";
      "caml_startup__data_begin"; "caml_startup__data_end";
      "caml_system__code_begin"; "caml_system__code_end";
    ]
  in
  let startup_code =
    List.assoc "caml_startup__code_begin" syms |> Symbol.value,
    List.assoc "caml_startup__code_end" syms |> Symbol.value
  and startup_data =
    List.assoc "caml_startup__data_begin" syms |> Symbol.value,
    List.assoc "caml_startup__data_end" syms |> Symbol.value
  and system_code =
    List.assoc "caml_system__code_begin" syms |> Symbol.value,
    List.assoc "caml_system__code_end" syms |> Symbol.value
  in
  let ranges = [
    fst startup_code, snd startup_code;
    fst startup_data, snd startup_data;
    fst system_code, snd system_code;
  ] in
  (* remove symbols with addresses between start..end *)
  let in_range addr =
    List.exists (fun (start, stop) ->
        (addr >= start && addr <= stop))
      ranges
  in
  AddrTbl.filter_map_inplace
    (fun addr v -> if in_range addr then None else Some v) h;
  Hashtbl.iter (fun k (size, vs) ->
      List.iter (AddrTbl.remove h) vs;
      let first_addr = List.hd (List.sort compare vs) in
      AddrTbl.add h first_addr (["OCaml" ; k ; "primitives"], Info.mk ~v:first_addr ~size Info.Primitive))
    other_syms;
  (* remove the code_begin/code_end/data_begin/data_end symbols *)
  List.iter (fun (start, stop) ->
      AddrTbl.remove h start; AddrTbl.remove h stop)
    ranges;
  (* finally, add one symbol for startup and one for system *)
  let startup_code_size =
    Int64.sub (snd startup_code) (fst startup_code)
  and startup_data_size =
    Int64.sub (snd startup_data) (fst startup_data)
  and system_size =
    Int64.sub (snd system_code) (fst system_code)
  in
  let startup_loc = AddrMap.find_opt (fst startup_code) loctbl in
  Printf.printf "startup code %08Lx - %08Lx\nstartup data %08Lx - %08Lx\nsystem_size %08Lx - %08Lx\n"
    (fst startup_code) (Int64.add (fst startup_code) startup_code_size)
    (fst startup_data) (Int64.add (fst startup_data) startup_data_size)
    (fst system_code) (Int64.add (fst system_code) system_size);
  AddrTbl.add h (fst startup_code)
    (["OCaml" ; "startup" ; "code"], Info.mk ?location:startup_loc ~v:(fst startup_code) ~size:startup_code_size Info.Module);
  let startup_loc = AddrMap.find_opt (fst startup_data) loctbl in
  AddrTbl.add h (fst startup_data)
    (["OCaml" ; "startup" ; "data"], Info.mk ?location:startup_loc ~v:(fst startup_data) ~size:startup_data_size Info.Module);
  let system_loc = AddrMap.find_opt (fst system_code) loctbl in
  AddrTbl.add h (fst system_code)
    (["OCaml" ; "system"], Info.mk ?location:system_loc ~v:(fst system_code) ~size:system_size Info.Module);
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

let print_section s =
    let open Owee_elf in
    Printf.printf "0x%08Lx - 0x%08Lx (VM 0x%08Lx - 0x%08Lx) section %s\n"
      s.sh_offset (Int64.add s.sh_offset s.sh_size)
      s.sh_addr (Int64.add s.sh_addr s.sh_size)
      s.sh_name_str

let get path =
  let buffer = mk_buffer path in
  Printf.printf "bigarray size %d\n" (Bigarray.Array1.size_in_bytes buffer);
  let _header, sections = Owee_elf.read_elf buffer in
  (* let check_consistency acc_offset section =
   *   print_section section;
   *   if acc_offset <> section.sh_offset then
   *     Printf.printf "Offsets not consistent! section.sh_offset = %Lu, acc_offset = %Lu\n"
   *       section.sh_offset acc_offset;
   *   Int64.add section.sh_offset section.sh_size
   * in *)
  let compute_section_sizes size section =
    if section.sh_addr <> 0L then begin
      print_section section ;
      Int64.add size section.sh_size
    end else size
  in (* 35000 bytes more in section_size than in the binary hvt *)
  (* Array.fold_left check_consistency 0L sections |> ignore; *)
  let section_size = Array.fold_left compute_section_sizes 0L sections in
  Printf.printf "accounted section size %Lu\n" section_size;
  match mk_info_tbl buffer sections with
  | None -> Error `Invalid_file
  | Some h -> Ok (fun k -> AddrTbl.iter (fun _ x -> k x) h)


