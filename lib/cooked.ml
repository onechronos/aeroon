open Raw

module Log = (val Logs.src_log (Logs.Src.create "aeron"))

open struct
  let check_close_err what b =
    if not b then Log.err (fun k -> k "could not close %s" what)
end

module Version = struct
  let major = version_major ()

  let minor = version_minor ()

  let patch = version_patch ()

  let full = version_full ()
end

let main_idle_strategy = main_idle_strategy

module IdleStrategy = struct
  type t = int -> int -> unit

  let sleeping : t = idle_strategy_sleeping_idle

  let yielding : t = idle_strategy_yielding_idle

  let busy_spinning : t = idle_strategy_busy_spinning_idle

  let noop : t = idle_strategy_noop_idle

  let backoff : t = idle_strategy_backoff_idle
end

module Clock = struct
  let nano = nano_clock

  let epoch = epoch_clock
end

module Context = struct
  type t = context

  let create = context_init

  let close = context_close

  let with_ f =
    match create () with
    | None -> None
    | Some c ->
      Fun.protect
        (fun () -> Some (f c))
        ~finally:(fun () -> check_close_err "context" (close c))
end

module Client = struct
  type t = client

  let create = init

  let start = start

  let close = close

  let with_create_start ctx f =
    match create ctx with
    | None -> None
    | Some c ->
      if not (start c) then
        None
      else
        Fun.protect
          (fun () -> Some (f c))
          ~finally:(fun () -> check_close_err "client" @@ close c)
end

type canal = {
  uri: string;
  stream_id: int;
}

let create_retry async_add async_poll ?(pause_between_attempts_s = 1e-3) client
    { uri; stream_id } : ('a, string) result =
  match async_add client uri stream_id with
  | None -> Error (errmsg ())
  | Some async ->
    let rec poll () : ('a, string) result =
      match async_poll async with
      | Ok pub -> Ok pub
      | TryAgain ->
        Unix.sleepf pause_between_attempts_s;
        poll ()
      | Error -> Error (errmsg ())
    in
    poll ()

module Publication_error = struct
  type t = Raw.publication_error =
    | Not_connected
    | Back_pressured
    | Admin_action
    | Closed
    | Max_position_exceeded
    | Error

  let to_string = function
    | Not_connected -> "not connected"
    | Back_pressured -> "back pressured"
    | Admin_action -> "admin action"
    | Closed -> "closed"
    | Max_position_exceeded -> "max position exceeded"
    | Error -> "error"
end

module Publication = struct
  type t = publication

  let create = create_retry async_add_publication async_add_publication_poll

  let close = publication_close

  let with_ ?pause_between_attempts_s client canal f : _ result =
    match create ?pause_between_attempts_s client canal with
    | Error e -> Error e
    | Ok c ->
      Fun.protect
        (fun () -> Result.Ok (f c))
        ~finally:(fun () -> check_close_err "publication" @@ close c)

  let offer = publication_offer

  let is_connected = publication_is_connected

  let is_closed = publication_is_closed
end

module ExclusivePublication = struct
  type t = exclusive_publication

  let create =
    create_retry async_add_exclusive_publication
      async_add_exclusive_publication_poll

  let close = exclusive_publication_close

  let with_ ?pause_between_attempts_s client canal f : _ result =
    match create ?pause_between_attempts_s client canal with
    | Error e -> Error e
    | Ok c ->
      Fun.protect
        (fun () -> Result.Ok (f c))
        ~finally:(fun () -> check_close_err "exclusive publication" @@ close c)

  let offer = exclusive_publication_offer

  let is_closed = exclusive_publication_is_closed

  let is_connected = exclusive_publication_is_connected

  let try_claim = exclusive_publication_try_claim
end

type fragment_handler = string -> unit

module Image = struct
  type t = image

  let is_closed = image_is_closed

  let poll = image_poll

  type a = image_fragment_assembler

  let create_assembler = image_fragment_assembler_create

  let position = image_position
end

module Subscription = struct
  type t = subscription

  let create = create_retry async_add_subscription async_add_subscription_poll

  let close = subscription_close

  let with_ ?pause_between_attempts_s client canal f : _ result =
    match create ?pause_between_attempts_s client canal with
    | Error e -> Error e
    | Ok c ->
      Fun.protect
        (fun () -> Result.Ok (f c))
        ~finally:(fun () -> check_close_err "subscription" @@ close c)

  let is_closed = subscription_is_closed

  type a = fragment_assembler

  let create_assembler = fragment_assembler_create

  let poll = subscription_poll

  let image_at_index = subscription_image_at_index

  let is_connected = subscription_is_connected

  let image_release = subscription_image_release
end
