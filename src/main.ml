open CCResult.Infix

type ty =
  | Elf

let get_file (file, ty) = match ty with
  | Elf ->
    let f = function
      | `Invalid_file ->
        `Msg (Format.sprintf "The file %s is not a valid ELF binary." file)
    in
    CCResult.map_err f @@ Elf.get file 

let squarify infos =
  infos
  |> Iter.filter (fun (_,x) -> x.Info.size > 0L)
  |> Info.of_iter
  |> Format.printf "%a@." Info.pp

let programs_arg =
  let open Cmdliner in
  let annot f t = Term.(pure (List.map (fun x -> (x, f x))) $ t) in  
  let elf_args = 
    let i = Arg.info ["elf"] in
    annot (fun _ -> Elf) Arg.(value & opt_all file [] i)
  in
  let guess _ = Elf in
  let guess_args =
    let i = Arg.info [] in
    annot guess Arg.(value & pos_all file [] i)
  in
  let take_all elfs guesses = elfs @ guesses in
  Term.(pure take_all $ elf_args $ guess_args)

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
  let i = Term.info "modulectomy" in
  Term.(term_result (pure squarify_files $ programs_arg)), i

let () =
  Cmdliner.Term.(exit @@ eval main_term)
