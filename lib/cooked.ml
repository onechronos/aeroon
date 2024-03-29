open Raw

module Log = (val Logs.src_log (Logs.Src.create "aeron"))

open struct
  let check_close_err what b =
    if not b then Log.err (fun k -> k "could not close %s" what)

  let maybe_sleep = function
    | None -> ()
    | Some s -> Unix.sleepf s

  let default_sleep_before_close_s = Some 0.200
end

module Version = struct
  let major = version_major ()

  let minor = version_minor ()

  let patch = version_patch ()

  let full = version_full ()
end

module Error = struct
  type t =
    | Invalid_channel
    | Unknown_subscription
    | Unknown_publication
    | Channel_endpoint_error
    | Unknown_counter
    | Unknown_command_type_id
    | Malformed_command
    | Not_supported
    | Unknown_host
    | Resource_temporarily_unavailable
    | Generic_error

  let code_t_assoc =
    [
      error_code_invalid_channel (), Invalid_channel;
      error_code_unknown_subscription (), Unknown_subscription;
      error_code_unknown_publication (), Unknown_publication;
      error_code_channel_endpoint_error (), Channel_endpoint_error;
      error_code_unknown_counter (), Unknown_counter;
      error_code_unknown_command_type_id (), Unknown_command_type_id;
      error_code_malformed_command (), Malformed_command;
      error_code_not_supported (), Not_supported;
      error_code_unknown_host (), Unknown_host;
      ( error_code_resource_temporarily_unavailable (),
        Resource_temporarily_unavailable );
      error_code_generic_error (), Generic_error;
    ]

  let get () =
    let code = errcode () in
    match List.assoc_opt (-code) code_t_assoc with
    | None -> `U (code, errmsg ())
    | Some t -> `K t
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

  let close ?sleep_before_s t =
    maybe_sleep sleep_before_s;
    context_close t

  let with_ f =
    let c_opt = create () in
    Fun.protect
      (fun () -> f c_opt)
      ~finally:(fun () ->
        Option.iter
          (fun c ->
            check_close_err "context"
              (close ?sleep_before_s:default_sleep_before_close_s c))
          c_opt)
end

module Client = struct
  type t = client

  let create = init

  let start = start

  let close ?sleep_before_s t =
    is_closed t
    ||
    (maybe_sleep sleep_before_s;
     close t)

  let with_create_start ctx f =
    let client_opt = create ctx in
    let client_started =
      client_opt
      |> Option.map (fun c ->
             if start c then
               Some c
             else
               None)
      |> Option.join
    in
    Fun.protect
      (fun () -> f client_started)
      ~finally:(fun () ->
        Option.iter
          (fun c ->
            check_close_err "client"
            @@ close ?sleep_before_s:default_sleep_before_close_s c)
          client_opt)
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
      | Error ->
        (match Error.get () with
        | `K Error.Resource_temporarily_unavailable ->
          Unix.sleepf pause_between_attempts_s;
          poll ()
        | _ -> Error (errmsg ()))
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

(* re-try publication *)
let rec retry_pub_h prev_result ~pause_between_attempts_s ~retry_limit
    (f : unit -> ((_, Publication_error.t) result as 'res)) : 'res =
  match prev_result with
  | Some (retry_count, prev_error) when retry_count = retry_limit ->
    Error prev_error
  | Some (retry_count, _) ->
    actually_retry_pub_h ~pause_between_attempts_s ~retry_limit
      (retry_count + 1) f
  | None -> actually_retry_pub_h ~pause_between_attempts_s ~retry_limit 0 f

and actually_retry_pub_h ~pause_between_attempts_s ~retry_limit retry_count f =
  (* when experiencing back-pressure or admin-action; double the
     [pause_between_attempts_s] *)
  match f () with
  | Error (Publication_error.Back_pressured as e) ->
    Unix.sleepf pause_between_attempts_s;
    let pause_between_attempts_s = 2.0 *. pause_between_attempts_s in
    retry_pub_h (Some (retry_count, e)) ~pause_between_attempts_s ~retry_limit f
  | Error (Publication_error.Admin_action as e) ->
    Unix.sleepf pause_between_attempts_s;
    let pause_between_attempts_s = 2.0 *. pause_between_attempts_s in
    retry_pub_h (Some (retry_count, e)) ~pause_between_attempts_s ~retry_limit f
  | r -> r

let retry_pub_ = retry_pub_h None

module Publication = struct
  type t = publication

  let create = create_retry async_add_publication async_add_publication_poll

  let close ?sleep_before_s t =
    maybe_sleep sleep_before_s;
    publication_close t

  let with_ ?pause_between_attempts_s client canal f =
    let x = create ?pause_between_attempts_s client canal in
    Fun.protect
      (fun () -> f x)
      ~finally:(fun () ->
        Result.iter
          (fun x ->
            check_close_err "publication"
            @@ close ?sleep_before_s:default_sleep_before_close_s x)
          x)

  let offer ?(pause_between_attempts_s = 1e-6) ?(retry_limit = 100) (self : t)
      (msg : string) : _ result =
    retry_pub_ ~pause_between_attempts_s ~retry_limit (fun _ ->
        publication_offer self msg)

  let is_connected = publication_is_connected

  let is_closed = publication_is_closed
end

module ExclusivePublication = struct
  type t = exclusive_publication

  let create =
    create_retry async_add_exclusive_publication
      async_add_exclusive_publication_poll

  let close ?sleep_before_s t =
    maybe_sleep sleep_before_s;
    exclusive_publication_close t

  let with_ ?pause_between_attempts_s client canal f =
    let x = create ?pause_between_attempts_s client canal in
    Fun.protect
      (fun () -> f x)
      ~finally:(fun () ->
        Result.iter
          (fun x ->
            check_close_err "exclusive publication"
            @@ close ?sleep_before_s:default_sleep_before_close_s x)
          x)

  let offer ?(pause_between_attempts_s = 1e-6) ?(retry_limit = 100) (self : t)
      msg : _ result =
    retry_pub_ ~pause_between_attempts_s ~retry_limit (fun _ ->
        exclusive_publication_offer self msg)

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

  let close ?sleep_before_s t =
    maybe_sleep sleep_before_s;
    subscription_close t

  let with_ ?pause_between_attempts_s client canal f =
    let x = create ?pause_between_attempts_s client canal in
    Fun.protect
      (fun () -> f x)
      ~finally:(fun () ->
        Result.iter
          (fun x ->
            check_close_err "exclusive subscription"
            @@ close ?sleep_before_s:default_sleep_before_close_s x)
          x)

  let is_closed = subscription_is_closed

  type a = fragment_assembler

  let create_assembler = fragment_assembler_create

  let poll = subscription_poll

  let image_at_index = subscription_image_at_index

  let is_connected = subscription_is_connected

  let image_release = subscription_image_release
end
