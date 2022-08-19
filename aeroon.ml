open Ctypes
open Foreign

let _ =
  let lib_so_path = Sys.argv.(1) in
  let lib = Dl.(dlopen ~filename:lib_so_path ~flags:[RTLD_NOW]) in

  let context_t : [`Aeron_context] abstract typ =
    abstract ~name:"aeron_context_stct" ~size:1 ~alignment:1
  in

  let context_init =
    foreign "aeron_context_init" (ptr (ptr context_t) @-> returning int) ~from:lib
  in

  let context_get_dir =
    foreign "aeron_context_get_dir" ((ptr context_t) @-> returning string) ~from:lib
  in

  let p_context = allocate (ptr context_t) (from_voidp context_t null) in

  let err = context_init p_context in
  match err with
  | -1 -> failwith "context_init"
  | 0 ->
     let p = !@ p_context in
     let dir = context_get_dir p in
     Printf.printf "dir=%s\n" dir

  | _ -> assert false



