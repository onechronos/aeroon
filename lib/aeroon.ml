module Bindings = Aeroon_bindings

open struct
  module Ty = Bindings.C.Type
  module F = Bindings.C.Functions
  module Alloc = Bindings.Alloc

  type 'a aptr = 'a Ctypes_static.abstract Ctypes_static.ptr

  let is_null = Ctypes.is_null
  let ( !@ ) = Ctypes.( !@ )
end

module Context = struct
  type t = [ `Context ] aptr

  let create () : t =
    let ptr =
      Alloc.ptr_context
        ~finalise:(fun ctx ->
          if not (is_null ctx) then ignore (F.context_close !@ctx : int))
        ()
    in
    if not (is_null ptr) then (
      let err = F.context_init ptr in
      (* TODO: error code *)
      if err <> 0 then failwith "aeron.context";
      let ctx = !@ptr in
      assert (not (is_null ctx));
      ctx
    ) else
      failwith "aeron.context"

  let get_dir (self : t) : string = F.context_get_dir self

  let set_dir (self : t) dir : unit =
    ignore (F.context_set_dir self dir : _ option)

  let get_driver_timeout_ms (self : t) : int64 =
    F.context_get_driver_timeout_ms self |> Unsigned.UInt64.to_int64

  let set_driver_timeout_ms (self : t) (n : int64) =
    F.context_set_driver_timeout_ms self (Unsigned.UInt64.of_int64 n)

  (* TODO: more config *)

  (* TODO
     let on_new_subscription (self:t)
  *)
end

module Client = struct
  type t = [ `Client ] aptr
end

(** Version of Aeron *)
module Version = struct
  let major = F.version_major ()
  let minor = F.version_minor ()
  let patch = F.version_patch ()
end
