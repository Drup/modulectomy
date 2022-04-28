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

module Robur_defaults = struct

  let css_overrides = "\
    .treemap-module {\
      fill: rgb(60, 60, 87);\
    }\
    .treemap-functor > text, .treemap-module > text {\
      fill: bisque;\
    }\
  "

  let filter_small = 0.004

  let with_scale () = failwith "You need to pass --with-scale=<ELF-SIZE>"
  
end

let squarify robur_defaults robur_css filter_small with_scale infos =
  let default_css_overrides =
    if robur_defaults || robur_css then
      Some Robur_defaults.css_overrides
    else None
  and default_filter_small = 
    if robur_defaults then
      Some Robur_defaults.filter_small
    else None
  and default_with_scale () = 
    if robur_defaults then
      Some (Robur_defaults.with_scale ())
      (*< todo can this param dependency be represented in Cmdliner DSL?*)
    else None
  in
  let filter_small = filter_small |> CCOption.or_ ~else_:default_filter_small
  and with_scale = with_scale |> CCOption.or_lazy ~else_:default_with_scale
  in
  let size, infos = 
    infos
    |> Info.import
    |> (fun info ->
      let size, tree = Info.diff_size_tree info in
      if debug then print_debug ~size ~tree;
      size, tree
    )
  in
  (*> Note: this heuristic fails if one has many subtrees of equal size*)
  let node_big_enough subtree =
    match filter_small, Info.(subtree.T.value.size) with
    | _, None | None, _ -> true 
    | Some min_pct, Some subtree_size ->
      let pct = Int64.(to_float subtree_size /. to_float size) in
      pct > min_pct 
  in
  let infos, excluded_minors =
    infos
    |> Info.prefix_filename
    |> Info.cut 2
    |> Info.partition_subtrees node_big_enough
  in
  let override_css = default_css_overrides in
  let treemap = Treemap.of_tree infos in
  let html = match with_scale with
    | None -> Treemap.to_html ?override_css treemap
    | Some elf_size ->
      let scale_chunks =
        let excluded_minors_size =
          excluded_minors
          |> List.map Info.compute_area
          |> List.fold_left Int64.add 0L
        in
        [ "Smaller excluded entries", excluded_minors_size ]
      in
      Treemap.to_html_with_scale
        ~binary_size:elf_size
        ~scale_chunks
        ?override_css
        treemap
  in
  Tyxml.Html.pp () Format.std_formatter html

let guess file =
  match Fpath.get_ext @@ Fpath.v file with
  | _ -> Elf
  | exception _ -> Elf

module Arg_aux = struct

  open Cmdliner 

  let programs_arg =
    let flatten x = Term.(const List.flatten $ x) in
    let annot f t =
      let g l = List.map (fun x -> (x, f x)) l in
      Term.(const g $ t) in
    let elf_args =
      let doc = "Native ELF binaries. Requires the $(b,owee) library. \
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

  let filter_small =
    let doc = "Remove subtrees that are smaller than PCT" in
    let docv = "PCT" in
    Arg.(value & opt (some float) None & info [ "filter-small" ] ~doc ~docv)

  let with_scale =
    let doc = "Include an additional scale-SVG in the HTML, \
               given the size in bytes of the non-debug ELF file." in
    let docv = "BYTES" in
    Arg.(value & opt (some int) None & info [ "with-scale" ] ~doc ~docv)

  let robur_css = 
    let doc = "Use Robur CSS styling in HTML" in
    Arg.(value & flag & info [ "robur-css" ] ~doc)
  
  let robur_defaults = 
    let doc = "Use Robur default values for every configuration option. \
               You need to pass --with-scale too." in
    Arg.(value & flag & info [ "robur-defaults" ] ~doc)

  let visualization_version_printer = 
    let doc = "Print the visualization version. Used to know when the output \
               has changed because of an update to settings or implementation." in
    let arg = Arg.(value & flag & info [ "visualization-version" ] ~doc) in
    let print_visualization_version flag = 
      if not flag then `Ok () else (
        Printf.printf "%d\n" Treemap.visualization_version;
        exit 0
        (* `Error (false, Printf.sprintf "%d" Treemap.visualization_version) *)
      )
    in
    Term.(ret (const print_visualization_version $ arg))

end

let squarify_files
    _visualization_version_printer
    robur_defaults
    robur_css
    filter_small
    with_scale
    files =
  let rec get_all = function
    | [] -> Ok Iter.empty
    | h :: t ->
      get_file h >>= fun i ->
      get_all t >|= fun i' ->
      Iter.append i i'
  in
  get_all files >|= fun i ->
  squarify robur_defaults robur_css filter_small with_scale i

let main_term =
  let open Cmdliner in
  let doc = "Dissect OCaml compiled programs, and weight their content." in
  let info = Cmd.info ~doc "modulectomy" in
  let term = Term.(term_result (
    const squarify_files
    $ Arg_aux.visualization_version_printer
    $ Arg_aux.robur_defaults
    $ Arg_aux.robur_css
    $ Arg_aux.filter_small
    $ Arg_aux.with_scale
    $ Arg_aux.programs_arg
  )) in
  Cmd.v info term

let () =
  Cmdliner.Cmd.eval main_term
  |> exit
