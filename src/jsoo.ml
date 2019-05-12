open Js_of_ocaml_compiler 

exception Invalid_sourcemap of string
exception Cant_find_sourcemap

(** Sourcemap *)

let get_sourcemap_from_comment =
  let r = Re.Posix.compile_pat "sourceMappingURL=(.*)$" in
  fun s ->
    let g = Re.exec_opt r s in
    match g with
    | None -> None
    | Some g ->
      let s = Re.Group.get g 1 in
      match Fpath.of_string s with
      | Ok s -> Some s
      | Error `Msg s -> raise @@ Invalid_sourcemap s

let find_sourcemap lexer =
  let iter k = Parse_js.lexer_fold (fun () tok -> k tok) () lexer in
  let p = function
    | Js_token.TComment (_, s)
    | Js_token.TCommentML (_,s)
      ->
      get_sourcemap_from_comment s
    | _ -> None
  in
  match Iter.find_map p iter with
  | Some s -> s
  | None -> raise Cant_find_sourcemap

let parse_sourcemap ~js_file ~sourcemap_file =
  let path, _ = Fpath.split_base js_file in
  let file = Fpath.(to_string @@ path // sourcemap_file) in
  Format.eprintf "Sourcemap: %s@." file ;
  Source_map_io.of_string @@ CCIO.(with_in file read_all)

(** Position handling *)

let make_linepos l =
  let f (linepos, _) tok =
    let lastpos = (Js_token.info_of_tok tok) in
    let linepos = match tok with
    | Js_token.TCommentNewline (pos, _) -> (pos.idx + 1) :: linepos
    | _ -> linepos
    in
    linepos, lastpos
  in
  let l, lastpos = Parse_js.lexer_fold f ([0], Parse_info.zero) l in
  (Array.of_list @@ List.rev l, lastpos)

let parse_js file =
  let lexer = Parse_js.lexer_from_file ~rm_comment:false file in
  let linepos = make_linepos lexer in
  let sourcemap_file = find_sourcemap lexer in
  let js = Parse_js.parse @@ Parse_js.strip_comment lexer in
  let sourcemap = parse_sourcemap ~js_file:(Fpath.v file) ~sourcemap_file in
  linepos, sourcemap, js

module PosMap = CCMap.Make(CCInt)
let make_posmap linepos (sourcemap : Source_map.t) =
  let get_source map =
    let l = List.length sourcemap.sources in
    let i = map.Source_map.ori_source in
    if i < 0 || i >= l then ""
    else List.nth sourcemap.sources map.Source_map.ori_source
  in 
  let add_binding posmap (map : Source_map.map) =
    let pos = linepos.(map.gen_line) + map.gen_col in
    let ori_source = get_source map in
    let ori_name = CCOpt.map (fun x -> List.nth sourcemap.names x) map.ori_name in
    let ori_pos = ori_source, map.ori_line, map.ori_col in
    PosMap.add pos (ori_name, ori_pos) posmap
  in
  List.fold_left add_binding PosMap.empty sourcemap.mappings

(** Javascript AST walking *)

let diff_loc loc1 loc2 = match loc1, loc2 with
  | Javascript.Pi pi1, Javascript.Pi pi2 ->
    Some (Int64.of_int @@ abs (pi1.idx - pi2.idx))
  | _ -> None
let pi_of_loc = function Javascript.Pi pi -> Some pi | _ -> None
let lookup_ident = function
  | Javascript.S id -> id.name, id.loc
  | V c -> Code.Var.to_string c, N
let get_name posmap id = 
  let open CCOpt.Infix in
  id >>= fun id ->
  let name, loc = lookup_ident id in
  let ori_name = 
    pi_of_loc loc >>= fun pi ->
    PosMap.find_opt pi.idx posmap >>= fun (name, _) ->
    name
  in
  ori_name <+> (Some name) 
  
let get_position posmap id =
  let open CCOpt.Infix in
  id >>= fun id ->
  let _, loc = lookup_ident id in
  pi_of_loc loc >>= fun pi ->
  (* Format.eprintf "%a: %i@." CCFormat.(opt string) pi.name (pi.idx+shift) ; *)
  PosMap.find_opt pi.idx posmap >>= fun (_, pos) ->
  Some pos

let is_position_in_js = function
  | Some (file,_,_) ->
    Fpath.(get_ext @@ v file) = ".js"
  | None -> false

let info_of_js endpos posmap js : _ Iter.t =
  let open Javascript in
  let mk scope ?id kind size k =
    match get_name posmap id with
    | None -> ()
    | Some name ->
      let pos = get_position posmap id in
      let kind = if is_position_in_js pos then Info.Primitive else kind in
      let data = Info.mk ?size ?location:pos kind in
      k (scope @ [name], data)
  in 
  let rec list f (scope, prevloc) l k = match l with
    | [] -> prevloc
    | h :: t ->
      let prevloc = list f (scope, prevloc) t k in
      let prevloc = f (scope, prevloc) h k in
      prevloc 
  in 
  let opt f (scope, prevloc) o k =
    CCOpt.map_or ~default:prevloc (fun i -> f (scope, prevloc) i k) o
  in
  let rec walk_source_elements a = list walk_source_element a
  and walk_source_element (scope, prevloc) (e, loc) k = match e with
    | Statement s -> walk_statement (scope, prevloc) (s, loc) k
    | Function_declaration d -> walk_function_declaration scope (d, loc) k
  and walk_statements a = list walk_statement a
  and walk_statement (scope, prevloc) (s, loc) k = match s with
    | Block l -> walk_statements (scope, prevloc) l k
    | Variable_statement l ->
      list walk_variable_decaration (scope, prevloc) l k
    (* Not rec *)
    | Empty_statement
    | Debugger_statement
    | Continue_statement _
    | Break_statement _
      -> loc
    (* Expr *)
    | Expression_statement e
    | Throw_statement e
      -> walk_expression (scope, prevloc) e k
    | Return_statement e
      -> opt walk_expression (scope, prevloc) e k
    (* rec *)
    | Do_while_statement (s, _)
    | While_statement (_, s)
    | For_statement (_, _, _, s)
    | ForIn_statement (_, _, s)
    | Labelled_statement (_, s)
      ->
      walk_statement (scope, prevloc) s k
    (* Multi rec *)
    | If_statement (_, s1, s2) ->
      begin match s2 with
        | None -> walk_statement (scope, prevloc) s1 k
        | Some s2 -> 
          let prevloc = walk_statement (scope, prevloc) s2 k in
          walk_statement (scope, prevloc) s1 k
      end
    (* TODO *)
    | Switch_statement (_, _, _, _) -> loc
    | Try_statement (_, _, _) -> loc
  and walk_expression (scope, endloc) e k = match e with
    | EFun (id, _, body, loc) ->
      let size = diff_loc loc endloc in
      mk scope Function size ?id k ;
      let scope = scope @ CCOpt.to_list @@ get_name posmap id in
      let _ = walk_source_elements (scope, endloc) body k in
      loc
    | ECall (e, args, _) ->
      let endloc = list walk_expression (scope, endloc) args k in
      walk_expression (scope, endloc) e k
    | _ -> endloc
  and walk_variable_decaration (scope, prevloc) (id, i) k =
    match i with
    | None -> prevloc
    | Some (e, loc) ->
      let prevloc = walk_expression (scope, prevloc) e k in
      let size = diff_loc loc prevloc in
      (* let scope = scope @ CCOpt.to_list @@ get_name 0 posmap loc ~id in *)
      mk scope Value size ~id k ;
      loc (* This sucks a bit *)
  and walk_function_declaration scope ((id, _, body, endloc), loc) k =
    let size = diff_loc loc endloc in
    mk scope Function size ~id k;
    let scope = scope @ CCOpt.to_list @@ get_name posmap (Some id) in
    let _ = walk_source_elements (scope, endloc) body k in
    loc
  in
  fun k -> ignore @@ walk_source_elements endpos js k

let get file =
  let (linepos, endpos), sourcemap, js = parse_js file in
  let posmap = make_posmap linepos sourcemap in
  (* Format.eprintf "@[<2>Posmal:@ %a@."
   *   (PosMap.pp CCFormat.int CCFormat.(pair (opt string) (fun _ _ -> ()) ))
   *   posmap; *)
  info_of_js ([], Pi endpos) posmap js 
