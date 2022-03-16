open CCResult.Infix
open Modulectomy

let debug = false

type ty =
  | Elf

let get_file (file, ty) = match ty with
  | Elf ->
    let f = function
      | `Invalid_file ->
        `Msg (Format.sprintf "The file %s is not a valid ELF binary." file)
    in
    CCResult.map_err f @@ Elf.get file

let print_debug ~size ~tree =
  Printf.eprintf "treemap size: %Ld \n" size;
  let ranges = Info.find_ranges tree in
  let compute_range_size acc (start, stop, _) =
    Int64.add acc (Int64.sub stop start)
  in
  let size = List.fold_left compute_range_size 0L ranges in
  Printf.eprintf "ranges size: %Lu\n" size;
  Printf.eprintf "ranges:\n";
  let x = ref 0L in
  List.iter (fun (start, stop, v) ->
    Printf.eprintf "0x%08Lx - 0x%08Lx %s\n" start stop v;
    if start > Int64.add !x 16L then
      Printf.eprintf "  GAP before: %Ld\n"  (Int64.sub start !x);
    x := stop)
    ranges;
  Printf.eprintf "\n"

let squarify infos =
  infos
  |> Info.import
  |> (fun info ->
      let size, tree = Info.diff_size_tree info in
      if debug then print_debug ~size ~tree;
      tree
    )
  (* |> Info.diff_size *)
  |> Info.prefix_filename
  |> Info.cut 2
  |> Treemap.of_tree
  |> Treemap.to_html
  |> Format.printf "%a@." (Tyxml.Html.pp ())

let guess file =
  match Fpath.get_ext @@ Fpath.v file with
  | _ -> Elf
  | exception _ -> Elf

let programs_arg =
  let open Cmdliner in
  let flatten x = Term.(const List.flatten $ x) in
  let annot f t =
    let g l = List.map (fun x -> (x, f x)) l in
    Term.(const g $ t) in
  let elf_args =
    let doc = "Native ELF (Linux) binaries. Requires the $(b,owee) library. \
               For better results, the binary file should have been compiled \
               with debug information." in
    let i = Arg.info ~doc ~docs:"FORMATS" ~docv:"BIN,..." ["elf"] in
    annot (fun _ -> Elf) @@ flatten Arg.(value & opt_all (list file) [] i)
  in
  let guess_args =
    let doc = "OCaml compiled files that need to be analyzed. Can be one of \
               formats described in $(b,FORMATS). By default, the format is \
               guessed."
    in
    let i = Arg.info ~doc ~docv:"FILE" [] in
    annot guess Arg.(value & pos_all file [] i)
  in
  let take_all elfs guesses =
    let l = elfs @ guesses in
    match l with
    | [] -> `Help (`Auto, None)
    | l -> `Ok l
  in
  Term.(ret (const take_all $ elf_args $ guess_args))

let squarify_files files =
  let rec get_all = function
    | [] -> Ok Iter.empty
    | h :: t ->
      get_file h >>= fun i ->
      get_all t >|= fun i' ->
      Iter.append i i'
  in
  get_all files >|= fun i ->
  squarify i

let main_term =
  let open Cmdliner in
  let doc = "Dissect OCaml compiled programs, and weight their content." in
  let info = Cmd.info ~doc "modulectomy" in
  let term = Term.(term_result (const squarify_files $ programs_arg)) in
  Cmd.v info term

let () =
  Cmdliner.Cmd.eval main_term
  |> exit
