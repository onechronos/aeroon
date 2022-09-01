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

  (* TODO: aeron_publication_is_closed  *)
  (* TODO: aeron_publication_is_connected *)

  (** offer publication, return the position.
      @param autoretry if true, some errors will be handled by re-trying
     (backpressure, etc.) *)
  let offer' ?(autoretry = true) (self : t) (msg : string) : int64 =
    let buffer_size = Unsigned.Size_t.of_int (String.length msg) in
    let rec loop n_retry =
      let status = F.publication_offer self msg buffer_size None null in
      if status < 0L then
        if
          autoretry && n_retry < 10
          && (status = Codes.Publication.back_pressured
             || status = Codes.Publication.admin_action
             || status = Codes.Publication.not_connected)
        then
          loop (n_retry + 1)
        else
          failwithf "publication.offer failed (%s, n_retry=%d)"
            (Codes.Publication.to_string status)
            n_retry
      else
        status
    in
    loop 0

  (** Same as {!offer'} but ignore position *)
  let offer ?autoretry (self : t) (msg : string) : unit =
    ignore (offer' ?autoretry self msg : int64)

  let close (self : t) : unit =
    let err = F.publication_close self None null in
    if err <> 0 then
      failwithf "aeron: publication close failed (%s)" (F.errmsg ())
end

module Subscription = struct
  type t = {
    ptr: [ `Subscription ] aptr;
    client: [ `Client ] aptr;
    fragment_assembler: [ `Fragment_assembler ] aptr;
    mutable new_msg_l: string list;
  }

  (* TODO: also a [with_] API so we can close properly? *)
  let create ~client ~ptr () : t =
    let mk_fragment_handler self =
      let handler_for_full_fragment _null buf size _header =
        let message =
          Ctypes.string_from_ptr buf ~length:(Unsigned.Size_t.to_int size)
        in
        let (lazy self) = self in
        self.new_msg_l <- message :: self.new_msg_l
      in
      let fragment_assembler =
        let p_fragment_assembler = Alloc.ptr_fragment_assembler () in
        let err =
          F.fragment_assembler_create p_fragment_assembler
            handler_for_full_fragment null
        in
        if err <> 0 then
          failwithf "aeron: couldn't initialize subscription (%s)" (F.errmsg ());
        !@p_fragment_assembler
      in
      fragment_assembler
    in

    let rec self =
      lazy
        (let fragment_assembler = mk_fragment_handler self in
         { client; ptr; new_msg_l = []; fragment_assembler })
    in
    Lazy.force self

  let[@inline] is_connected (self : t) : bool =
    F.subscription_is_connected self.ptr

  let channel_status (self : t) : [ `ACTIVE | `ERRORED ] =
    match F.subscription_channel_status self.ptr with
    | 1L -> `ACTIVE
    | -1L -> `ERRORED
    | _n -> failwithf "aeron: unknown channel status %Ld" _n

  (** Poll for new subscriptions.
     @param fragment_limit maximum number of fragments to receive
     @param autoretry if true, some errors will be handled by re-trying
     (backpressure, etc.) *)
  let poll ?(autoretry = true) ?(fragment_limit = 10) (self : t) : string list =
    let rec poll () : unit =
      let num_fragments_read =
        F.subscription_poll self.ptr F.fragment_assembler_handler
          self.fragment_assembler
          (Unsigned.Size_t.of_int fragment_limit)
      in
      if num_fragments_read < 0 then
        if autoretry then (
          F.main_idle_strategy self.client num_fragments_read;
          poll ()
        ) else
          failwithf "aeron: subscription_poll failed (%d/%s)" (F.errcode ())
            (F.errmsg ())
      else if num_fragments_read = 0 then (
        F.main_idle_strategy self.client num_fragments_read;
        poll ()
      )
    in
    poll ();
    (* pop new results *)
    let l = self.new_msg_l in
    self.new_msg_l <- [];
    List.rev l
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
    if err <> 0 then failwithf "aeron: cannot add publication (%d)" err;

    let p_pub = Alloc.ptr_publication () in
    let rec poll () =
      match F.async_add_publication_poll p_pub !@p_async_pub with
      | 1 -> !@p_pub
      | 0 -> poll ()
      | e -> failwithf "aeron: failed to poll for publication (%d)" e
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
    if err <> 0 then failwithf "aeron: cannot add subscription (%d)" err;

    let p_sub = Alloc.ptr_subscription () in
    let rec poll () =
      match F.async_add_subscription_poll p_sub !@p_async_sub with
      | 1 -> Subscription.create ~client:self ~ptr:!@p_sub ()
      | 0 -> poll ()
      | e -> failwithf "aeron: failed to poll for subscription (%d)" e
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
