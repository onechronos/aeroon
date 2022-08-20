open Ctypes
open Foreign


module Context = struct
  let t : [`Context] abstract typ =
    abstract ~name:"aeron_context_stct" ~size:1 ~alignment:1

  let init from =
    foreign "aeron_context_init" (ptr (ptr t) @-> returning int) ~from

  let get_dir from =
    foreign "aeron_context_get_dir" ((ptr t) @-> returning string) ~from

  let alloc () = allocate (ptr t) (from_voidp t null)

  type o = {
    init : [ `Context ] abstract Ctypes_static.ptr Ctypes_static.ptr -> int;
    get_dir : [ `Context ] abstract Ctypes_static.ptr -> string;
    alloc : unit -> [ `Context ] abstract Ctypes_static.ptr ptr
  }

  let create from = {
    init = init from;
    get_dir = get_dir from;
    alloc;
  }

end

let _ =
  let lib_so_path = Sys.argv.(1) in
  let lib = Dl.(dlopen ~filename:lib_so_path ~flags:[RTLD_NOW]) in

  let c = Context.create lib in

  let p_context = c.alloc () in
  let err = c.init p_context in
  match err with
  | -1 -> failwith "context_init"
  | 0 ->
     let p = !@ p_context in
     let dir = c.get_dir p in
     Printf.printf "dir=%s\n" dir

  | _ -> assert false



