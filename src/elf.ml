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
  | Some (s, id, k) ->
    Some ("OCaml"::s, id, annot_kind k @@ Symbol.type_attribute symb)
  | None ->
    let k = match Symbol.type_attribute symb with
      | Symbol.Func -> Info.Function
      | Symbol.Object -> Info.Value
      | _ -> Info.Unknown
    in
    Some ([name], None, k)

module AddrMap = Map.Make(Int64)
module AddrTbl = CCHashtbl.Make(CCInt64)

let mk_location_tbl buffer sections =
  Owee_elf.find_section sections ".debug_line" >|= fun section ->
  let pointers_to_other_sections = Owee_elf.debug_line_pointers buffer sections
  and body = Owee_buf.cursor (Owee_elf.section_body buffer section) in
  let rec aux tbl =
    match Owee_debug_line.read_chunk ~pointers_to_other_sections body with
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

type modul = {
  code_start : Symbol.t option ;
  code_end : Symbol.t option ;
  data_start : Symbol.t option ;
  data_end : Symbol.t option ;
}

let no_mod = { code_start = None ; code_end = None ; data_start = None ; data_end = None }

module S = Set.Make(String)

let mk_info_tbl buffer sections =
  mk_location_tbl buffer sections >>= fun loctbl ->
  Owee_elf.find_symbol_table buffer sections >>= fun symtbl ->
  Owee_elf.find_string_table buffer sections >|= fun tbl ->
  let h = AddrTbl.create 17 in
  let modules = Hashtbl.create 7 in
  let other_syms = Hashtbl.create 7 in
  let marker =
    let code_begin = "__code_begin" and code_end = "__code_end"
    and data_begin = "__data_begin" and data_end = "__data_end"
    in
    let lb = String.length code_begin and le = String.length code_end in
    fun name ->
      match name with
      | None -> Error ()
      | Some name ->
        let l = String.length name in
        if l >= le + 4 && String.(equal (sub name 0 4) "caml") then
          let modname suffix_len =
            (* camlMain__code_begin and caml_startup_code_begin *)
            let start = if String.get name 4 = '_' then 5 else 4 in
            if l > suffix_len + start then
              String.sub name start (l - suffix_len - start)
            else
              name
          in
          match
            l >= 4 + lb && String.(equal (sub name (l - lb) lb) code_begin),
            String.(equal (sub name (l - le) le) code_end),
            l >= 4 + lb && String.(equal (sub name (l - lb) lb) data_begin),
            String.(equal (sub name (l - le) le) data_end)
          with
          | true, false, false, false -> Ok (`Code_begin (modname lb))
          | false, true, false, false -> Ok (`Code_end (modname le))
          | false, false, true, false -> Ok (`Data_begin (modname lb))
          | false, false, false, true -> Ok (`Data_end (modname le))
          | false, false, false, false -> Error ()
          | _ -> assert false
        else
          Error ()
  in
  let update modname f =
    let v =
      match Hashtbl.find_opt modules modname with
      | None -> f no_mod
      | Some x -> f x
    in
    Hashtbl.replace modules modname v
  in
  let module SymRepr = struct
    type t = {
      value : int64;
      name : string option;
    }
    let of_symbol ~tbl s = {
      value = Symbol.value s;
      name = Symbol.name s tbl;
    }
    let compare = compare
  end in
  let module SymSet = Set.Make(SymRepr)
  in
  let symbols = ref SymSet.empty in
  let visited s = SymSet.mem (SymRepr.of_symbol ~tbl s) !symbols
  in
  let f symbol =
    if not (visited symbol) then
      begin
        symbols := SymSet.add (SymRepr.of_symbol ~tbl symbol) !symbols;
        match Symbol.type_attribute symbol with
        | Symbol.File -> ()
        | _ ->
          match Symbol.name symbol tbl with
          | Some name when filtered_sym name -> ()
          | name ->
            match marker name with
            | Ok `Code_begin modname ->
              update modname (fun x -> { x with code_start = Some symbol })
            | Ok `Code_end modname ->
              update modname (fun x -> { x with code_end = Some symbol })
            | Ok `Data_begin modname ->
              update modname (fun x -> { x with data_start = Some symbol })
            | Ok `Data_end modname ->
              update modname (fun x -> { x with data_end = Some symbol })
            | _ ->
              if not (Tables.categorize_symbol other_syms name symbol) then
                let v = Symbol.value symbol in
                let size = Some (Symbol.size_in_bytes symbol) in
                let location =
                  AddrMap.find_opt v loctbl
                in
                match classify_symb ~tbl symbol with
                | None -> ()
                | Some (name, _id, kind) ->
                  AddrTbl.add h v (name, Info.mk ~v ?size ?location kind)
      end
  in
  Symbol_table.iter symtbl ~f ;
  (* remove symbols with addresses between start..end *)
  let ranges =
    Hashtbl.fold (fun m { code_start ; code_end ; data_start ; data_end } acc ->
        let c =
          Symbol.value (Option.get code_start), Symbol.value (Option.get code_end)
        in
        match data_start, data_end with
        | None, None -> c :: acc
        | Some ds, Some de ->
          [ c ; Symbol.value ds, Symbol.value de ] @ acc
        | _ ->
          Printf.eprintf "only data_start or data_end present for %s\n" m;
          assert false)
      modules []
  in
  (* List.iter (fun (start, stop) ->
   *     Printf.eprintf "range: %08Lx - %08Lx\n" start stop) ranges; *)
  let in_range addr =
    List.exists (fun (start, stop) ->
        (addr >= start && addr <= stop))
      ranges
  in
  AddrTbl.filter_map_inplace
    (fun addr v -> if in_range addr then None else Some v) h;
  let to_mod ?post s =
    let eles = String.split_on_char '_' s in
    let rec go acc cur = function
      | [] -> List.rev (String.concat "_" (List.rev cur) :: acc)
      | "" :: rt -> go (String.concat "_" (List.rev cur) :: acc) [] rt
      | x :: xs -> go acc (x :: cur) xs
    in
    let m = go [] [] eles in
    "OCaml" :: m @ (match post with None -> [] | Some x -> [ x ])
  in
  Hashtbl.iter (fun k (size, vs) ->
      List.iter (AddrTbl.remove h) vs;
      let first_addr = List.hd (List.sort compare vs) in
      AddrTbl.add h first_addr (to_mod ~post:"primitives" k, Info.mk ~v:first_addr ~size Info.Value))
    other_syms;
  (* finally, add symbols for code and data *)
  Hashtbl.iter (fun m { code_start ; code_end ; data_start ; data_end } ->
      let code_addr = Symbol.value (Option.get code_start) in
      let code_size = Int64.sub (Symbol.value (Option.get code_end)) code_addr in
      let code_loc = AddrMap.find_opt code_addr loctbl in
      AddrTbl.add h code_addr
        (to_mod ~post:"code" m, Info.mk ?location:code_loc ~v:code_addr ~size:code_size Info.Value);
      match data_start, data_end with
      | None, None -> ()
      | Some ds, Some de ->
        let data_addr = Symbol.value ds in
        let data_size = Int64.sub (Symbol.value de) data_addr in
        let data_loc = AddrMap.find_opt data_addr loctbl in
        AddrTbl.add h data_addr
          (to_mod ~post:"data" m, Info.mk ?location:data_loc ~v:data_addr ~size:data_size Info.Value)
      | _ -> assert false)
    modules;
  let mnames =
    Hashtbl.fold (fun k _ acc -> S.add k acc) other_syms
      (Hashtbl.fold (fun k _ acc -> S.add k acc) modules S.empty)
  in
  S.iter (fun m -> AddrTbl.add h 0L (to_mod m, Info.mk Info.Module)) mnames;
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

(* let print_section s =
 *     let open Owee_elf in
 *     Printf.eprintf "0x%08Lx - 0x%08Lx (VM 0x%08Lx - 0x%08Lx) section %s\n"
 *       s.sh_offset (Int64.add s.sh_offset s.sh_size)
 *       s.sh_addr (Int64.add s.sh_addr s.sh_size)
 *       s.sh_name_str *)

let get path =
  let buffer = mk_buffer path in
  (* Printf.eprintf "bigarray size %d\n" (Bigarray.Array1.size_in_bytes buffer); *)
  let _header, sections = Owee_elf.read_elf buffer in
  (* let check_consistency acc_offset section =
   *   print_section section;
   *   if acc_offset <> section.sh_offset then
   *     Printf.eprintf "Offsets not consistent! section.sh_offset = %Lu, acc_offset = %Lu\n"
   *       section.sh_offset acc_offset;
   *   Int64.add section.sh_offset section.sh_size
   * in *)
  (* let compute_section_sizes size section =
   *   if section.sh_addr <> 0L then begin
   *     print_section section ;
   *     Int64.add size section.sh_size
   *   end else size
   * in *) (* 35000 bytes more in section_size than in the binary hvt *)
  (* Array.fold_left check_consistency 0L sections |> ignore; *)
  (* let section_size = Array.fold_left compute_section_sizes 0L sections in *)
  (* Printf.eprintf "accounted section size %Lu\n" section_size; *)
  match mk_info_tbl buffer sections with
  | None -> Error `Invalid_file
  | Some h -> Ok (fun k -> AddrTbl.iter (fun _ x -> k x) h)


