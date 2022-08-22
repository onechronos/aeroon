open Ctypes
open Foreign

module Context = struct
  let t : [`Context] abstract typ =
    abstract ~name:"aeron_context_stct" ~size:1 ~alignment:1

  let init =
    foreign "aeron_context_init" (ptr (ptr t) @-> returning int)

  let get_dir =
    foreign "aeron_context_get_dir" ((ptr t) @-> returning string)

  let alloc () = allocate (ptr t) (from_voidp t null)

end

let _ =
  let p_context = Context.alloc () in
  let err = Context.init p_context in
  match err with
  | -1 -> failwith "context_init"
  | 0 ->
     let p = !@ p_context in
     let dir = Context.get_dir p in
     Printf.printf "dir=%s\n" dir

  | _ -> assert false

