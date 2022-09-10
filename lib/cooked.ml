open Raw

module Version = struct
  let major = version_major ()

  let minor = version_minor ()

  let patch = version_patch ()

  let full = version_full ()
end

let main_idle_strategy = main_idle_strategy

module IdleStrategy = struct
  type t = int -> int -> unit

  let sleeping_idle : t = idle_strategy_sleeping_idle

  let yielding_idle : t = idle_strategy_yielding_idle

  let busy_spinning_idle : t = idle_strategy_busy_spinning_idle

  let noop_idle : t = idle_strategy_noop_idle

  let backoff_idle : t = idle_strategy_backoff_idle
end

module Context = struct
  type t = context

  let create = context_init

  let close = context_close
end

module Client = struct
  type t = client

  let create = init

  let start = start

  let close = close
end

let create_pub async_add async_poll ?(pause_between_attempts_s = 1e-3) client
    uri stream_id : ('a, string) result =
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

type publication_error = Raw.publication_error

module Publication = struct
  type t = publication

  let create = create_pub async_add_publication async_add_publication_poll

  let close = publication_close

  let offer = publication_offer

  let is_connected = publication_is_connected

  let is_closed = publication_is_closed
end

module ExclusivePublication = struct
  type t = exclusive_publication

  let create =
    create_pub async_add_exclusive_publication
      async_add_exclusive_publication_poll

  let close = exclusive_publication_close

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
end

module Subscription = struct
  type t = subscription

  let create ?(pause_between_attempts_s = 1e-3) client uri stream_id :
      (t, string) result =
    match async_add_subscription client uri stream_id with
    | None -> Error (errmsg ())
    | Some async ->
      let rec poll () : (t, string) result =
        match async_add_subscription_poll async with
        | Error -> Error (errmsg ())
        | TryAgain ->
          Unix.sleepf pause_between_attempts_s;
          poll ()
        | Ok subscription -> Ok subscription
      in
      poll ()

  let close = subscription_close

  let is_closed = subscription_is_closed

  type a = fragment_assembler

  let create_assembler = fragment_assembler_create

  let poll = subscription_poll

  let image_at_index = subscription_image_at_index

  let is_connected = subscription_is_connected

  let image_release = subscription_image_release
end
