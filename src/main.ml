open CCResult.Infix

type ty =
  | Elf
  | Js

let get_file (file, ty) = match ty with
  | Elf ->
    let f = function
      | `Invalid_file ->
        `Msg (Format.sprintf "The file %s is not a valid ELF binary." file)
    in
    CCResult.map_err f @@ Elf.get file
  | Js ->
    Ok (Jsoo.get file)

let squarify infos =
  infos
  |> Info.import
  |> Info.diff_size
  |> Info.prefix_filename
  |> Info.cut 2
  |> Treemap.of_tree
  |> Treemap.doc
  |> Format.printf "%a@." (Tyxml.Html.pp ())

let guess file =
  match Fpath.get_ext @@ Fpath.v file with
  | ".js" -> Js
  | _ -> Elf
  | exception _ -> Elf

let programs_arg =
  let open Cmdliner in
  let flatten x = Term.(pure List.flatten $ x) in
  let annot f t =
    let g l = List.map (fun x -> (x, f x)) l in
    Term.(pure g $ t) in
  let elf_args =
    let doc = "Native ELF (Linux) binaries. Requires the $(b,owee) library. For better results, the binary file should have been compiled with debug information." in
    let i = Arg.info ~doc ~docs:"FORMATS" ~docv:"BIN,..." ["elf"] in
    annot (fun _ -> Elf) @@ flatten Arg.(value & opt_all (list file) [] i)
  in
  let js_args =
    let doc = "Javascript filed compiled with $(b,js_of_ocaml). For better results, source maps should be available." in
    let i = Arg.info ~doc ~docs:"FORMATS" ~docv:"JS,..." ["js"] in
    annot (fun _ -> Js) @@ flatten Arg.(value & opt_all (list file) [] i)
  in
  let guess_args =
    let doc = "OCaml compiled files that need to be analyzed. Can be one of formats described in $(b,FORMATS). By default, the format is guessed."
    in
    let i = Arg.info ~doc ~docv:"FILE" [] in
    annot guess Arg.(value & pos_all file [] i)
  in
  let take_all js elfs guesses =
    let l = js @ elfs @ guesses in
    match l with
    | [] -> `Help (`Auto, None)
    | l -> `Ok l
  in
  Term.(ret (pure take_all $ js_args $ elf_args $ guess_args))

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
  let i = Term.info ~doc "modulectomy" in
  Term.(term_result (pure squarify_files $ programs_arg)), i

let () =
  Cmdliner.Term.(exit @@ eval main_term)
