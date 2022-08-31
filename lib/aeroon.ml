module Bindings = Aeroon_bindings

open struct
  module Ty = Bindings.C.Type
  module F = Bindings.C.Functions
  module Alloc = Bindings.Alloc

  let[@inline] ( let@ ) f x = f x
  let failwithf fmt = Format.kasprintf failwith fmt

  type 'a aptr = 'a Ctypes_static.abstract Ctypes_static.ptr

  let is_null = Ctypes.is_null
  let null = Ctypes.null
  let ( !@ ) = Ctypes.( !@ )
end

module Codes = Codes
(** Status codes and other enums *)

(** Main context *)
module Context = struct
  type t = [ `Context ] aptr

  let with_ (f : t -> 'a) : 'a =
    let ptr = Alloc.ptr_context () in
    if is_null ptr then failwith "aeron.context";

    let err = F.context_init ptr in
    (* TODO: error code *)
    if err <> 0 then failwith "aeron: failed to initialize context";
    let ctx = !@ptr in
    assert (not (is_null ctx));

    let@ () =
      Fun.protect ~finally:(fun () ->
          Printf.printf "close ctx\n%!";
          ignore (F.context_close ctx : int))
    in
    f ctx

  let get_dir (self : t) : string option = F.context_get_dir self

  (** Set directory *)
  let set_dir (self : t) dir : unit =
    ignore (F.context_set_dir self (Some dir) : _ option)

  (** Whether to use an invoker to control the conductor agent or spawn a thread. *)
  let set_use_conductor_agent_invoker self b : unit =
    let err = F.context_set_use_conductor_agent_invoker self b in
    if err <> 0 then
      failwith "aeron.context: set_use_conductor_agent_invoker failed"

  let get_use_conductor_agent_invoker self : bool =
    F.context_get_use_conductor_agent_invoker self

  let get_driver_timeout_ms (self : t) : int64 =
    F.context_get_driver_timeout_ms self |> Unsigned.UInt64.to_int64

  let set_driver_timeout_ms (self : t) (n : int64) =
    F.context_set_driver_timeout_ms self (Unsigned.UInt64.of_int64 n)

  (* TODO: more config *)

  (* TODO
     let on_new_subscription (self:t)
  *)
end

module Publication = struct
  type t = [ `Publication ] aptr
end

module Subscription = struct
  type t = [ `Subscription ] aptr
end

(** Client *)
module Client = struct
  type t = [ `Client ] aptr

  (** Create a new client using this context.

    Only one client per context. *)
  let with_ (ctx : Context.t) (f : t -> 'a) : 'a =
    let ptr = Alloc.ptr_client () in
    let err = F.init ptr ctx in
    if err <> 0 || is_null !@ptr then
      failwithf "aeron.client: failed to create (%s)"
        (Codes.Client.to_string err);
    let c = !@ptr in
    let@ () =
      Fun.protect ~finally:(fun () ->
          Printf.printf "finalize client\n%!";
          ignore (F.close c))
    in
    f c

  (** Start the client *)
  let start (self : t) : unit =
    let err = F.start self in
    if err <> 0 then
      failwithf "aeron.start: failed (%s)" (Codes.Client.to_string err);
    ()

  (** Idle, waiting for some messages to arrive *)
  let main_idle_stragey (self : t) ~work_count : unit =
    F.main_idle_strategy self work_count

  let id : t -> int64 = F.client_id
  let ctx : t -> Context.t = F.get_context

  (** Add a publication on the given URI and stream_id *)
  let add_publication (self : t) ~uri ~stream_id : Publication.t =
    let p_async_pub = Alloc.ptr_async_add_publication () in
    if is_null p_async_pub then failwith "aeron: failed to create publication";
    let err = F.async_add_publication p_async_pub self uri stream_id in
    if err <> 0 then
      failwithf "aeron: cannot add publication (%s)"
        (Codes.Publication.to_string err);

    let p_pub = Alloc.ptr_publication () in
    let rec poll () =
      match F.async_add_publication_poll p_pub !@p_async_pub with
      | 1 -> !@p_pub
      | 0 -> poll ()
      | e ->
        failwithf "aeron: failed to poll for publication (%s)"
          (Codes.Publication.to_string e)
    in
    poll ()

  (** Add a subscription on the given URI and stream_id *)
  let add_subscription (self : t) ~uri ~stream_id : Subscription.t =
    let p_async_sub = Alloc.ptr_async_add_subscription () in
    if is_null p_async_sub then failwith "aeron: failed to create subscription";
    let err =
      F.async_add_subscription p_async_sub self uri stream_id None null None
        null
    in
    if err <> 0 then
      failwithf "aeron: cannot add subscription (%s)"
        (Codes.Publication.to_string err);

    let p_sub = Alloc.ptr_subscription () in
    let rec poll () =
      match F.async_add_subscription_poll p_sub !@p_async_sub with
      | 1 -> !@p_sub
      | 0 -> poll ()
      | e ->
        failwithf "aeron: failed to poll for subscription (%s)"
          (Codes.Publication.to_string e)
    in
    poll ()

  (* TODO: exclusive publication *)
  (* TODO: exclusive subscription *)
end

(** Version of Aeron *)
module Version = struct
  let major = F.version_major ()
  let minor = F.version_minor ()
  let patch = F.version_patch ()
end
