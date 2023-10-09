(* as Aeron is not typically installable via pakcage installers, we
   have to assume here that the user has already installed it, and ask
   her to point us to its location using an environment variable. *)

let () =
  let var = "AERON_ROOT" in
  match Sys.getenv_opt var with
  | None ->
    Printf.printf
      "please set environment variable %s pointing to root of the Aeron \
       repository"
      var;
    exit 1
  | Some aeron_root ->
    let fc path_components =
      match List.rev path_components with
      | head :: tail ->
        List.fold_left
          (fun component path -> Filename.concat path component)
          head tail
      | _ -> assert false
    in
    (* expecting C include files in [$AERON_ROOT/aeron-client/src/main/c] *)
    let include_dir = fc [ aeron_root; "aeron-client"; "src"; "main"; "c" ] in
    if Sys.file_exists include_dir then (
      (* expecting shared object libraries under [$AERON_ROOT/build/lib].
         In terms of building the Aeron shared-object library, the following
         worked for the author:
         {[
            cd /path/to/aeron
            rm -rf build
            mkdir build
            cd build
            cmake ..
         ]}
      *)
      let lib_dir = fc [ aeron_root; "lib" ] in
      if Sys.file_exists lib_dir then (
        let module C = Configurator.V1 in
        C.Flags.write_sexp "c_flags.sexp" [ "-I" ^ include_dir ];
        let library_flags = [ "-L" ^ lib_dir; "-laeron" ] in
        C.Flags.write_sexp "c_library_flags.sexp" library_flags
      ) else (
        Printf.printf "directory %S under %s=%s does not exist!" lib_dir var
          aeron_root;
        exit 1
      )
    ) else (
      Printf.printf "directory %S under %s=%s does not exists!" include_dir var
        aeron_root;
      exit 1
    )
