(** Aeron version information *)
module Version : sig
  val major : int

  val minor : int

  val patch : int

  val full : string
end

(** Clock: wall and non-wall time *)
module Clock : sig
  val nano : unit -> int
  (** nanoseconds since epoch for machine; not wall clock time *)

  val epoch : unit -> int
  (** time in milliseconds since epoch; wall clock time *)
end

(** various way sleep, yield, and wait *)
module IdleStrategy : sig
  type t = int -> int -> unit

  val sleeping : t

  val yielding : t

  val busy_spinning : t

  val noop : t

  val backoff : t
end

(** Aeron Context *)
module Context : sig
  type t

  val create : unit -> t option
  (** create an Aeron context *)

  val close : t -> bool
  (** close an Aeron context; using any Aeron function dependent on
      this context thereafter is unsafe, and likely result in
      segfaults *)
end

(** Aeron client *)
module Client : sig
  type t

  val create : Context.t -> t option
  (** create an Aeron client. A result of [None] indicates failure *)

  val start : t -> bool
  (** start an Aeron client. A result of [false] indicates failure *)

  val close : t -> bool
  (** close an Aeron client. A result of [false] indicates failure *)
end

val main_idle_strategy : Client.t -> int -> unit
(** call the Aeron Conductor idle strategy *)

(** abnormal outcomes to attempted publication offers; most outcomes
    are non-fatal and transient. *)
type publication_error = Raw.publication_error =
  | Not_connected
      (** The publication is not connected to a subscriber, this can be an intermittent state as subscribers come and go. *)
  | Back_pressured
      (** The offer failed due to back pressure from the subscribers preventing further transmission. *)
  | Admin_action
      (** The offer failed due to an administration action and should be retried.
      The action is an operation such as log rotation which is likely to have succeeded by the next retry attempt. *)
  | Closed  (** The publication has been closed and should no longer be used. *)
  | Max_position_exceeded
      (** The offer failed due to reaching the maximum position of the stream given term buffer length times the total possible number of terms. *)
  | Error  (** An error has occurred, such as a bad argument. *)

val string_of_publication_error : publication_error -> string
(** convert a value of [publication_error] into a readable string *)

type canal = {
  uri: string;
  stream_id: int;
}
(** a communication endpoint, consisting of an Aeron URI and stream id *)

module Publication : sig
  type t
  (** publication *)

  val create :
    ?pause_between_attempts_s:float -> Client.t -> canal -> (t, string) result
  (** create an Aeron publication *)

  val close : t -> bool
  (** close an Aeron publication; [false] indicates failure *)

  val offer : t -> string -> (int, publication_error) result
  (** [offer pub msg] send string [msg] to publication [pub]. If
      successful, returns new stream position. Otherwise it returns a
      value of [publication_error]. *)

  val is_connected : t -> bool
  (** [is_connected pub] returns [true] if the publication is connected, and [false] otherwise. *)

  val is_closed : t -> bool
  (** [is_closed pub] returns [true] if the publication is closed, and [false] otherwise. *)
end

module ExclusivePublication : sig
  type t
  (** exclusive publication *)

  val create :
    ?pause_between_attempts_s:float -> Client.t -> canal -> (t, string) result
  (** create an Aeron exclusive publication *)

  val close : t -> bool
  (** close an Aeron exclusive publication; [false] indicates failure *)

  val offer : t -> string -> (int, publication_error) result
  (** [offer pub msg] send string [msg] to exclusive publication [pub]. If
      successful, returns new stream position. Otherwise it returns a
      value of [publication_error]. *)

  val is_connected : t -> bool
  (** [is_connected pub] returns [true] if the exclusive publication
      is connected, and [false] otherwise. *)

  val is_closed : t -> bool
  (** [is_closed pub] returns [true] if the exclusive publication is
      closed, and [false] otherwise. *)

  val try_claim : t -> string -> bool
  (** According to the Aeron C API documentation: Try to claim a range
      in the publication log into which a message can be written with
      zero copy semantics.  Once the message has been written then
      aeron_buffer_claim_commit should be called thus making it
      available. A claim length cannot be greater than max payload
      length. Note: This method can only be used for message lengths
      less than MTU length minus header.  If the claim is held for
      more than the aeron.publication.unblock.timeout system property
      then the driver will * assume the publication thread is dead and
      will unblock the claim thus allowing other threads to make
      progress and other claims to be sent to reach end-of-stream
      (EOS). *)
end

type fragment_handler = string -> unit
(** type of callback for subscriptions *)

module Image : sig
  type t
  (** image *)

  val is_closed : t -> bool
  (** [is_closed image] returns [true] if [image] is closed, [false] otherwise *)

  type a
  (** image assembler *)

  val create_assembler : fragment_handler -> a option
  (** [create_assembler f] sets function [f] as the callback to receive messages associated with the subscribed image *)

  val poll : t -> a -> int -> int option
  (** [poll image assembler count] polls for new messages in a
      stream. If new messages are found beyond the last consumed
      position then they will be delivered to the assembler's handler
      up to a limited number of fragments as specified. Returns [Some
      num_consumed_fragments] on success, [None] on error. *)

  val position : t -> int
  (** [position image] returns the position [image] has been consumed
      to by the subscriber. *)
end

module Subscription : sig
  type t

  val create :
    ?pause_between_attempts_s:float -> Client.t -> canal -> (t, string) result
  (** create a subscription *)

  type a
  (** assembler *)

  val create_assembler : fragment_handler -> a option
  (** [create_assembler f] sets function [f] as the callback to
      receive messages associated with the subscription. *)

  val poll : t -> a -> int -> int option
  (** [poll image assembler count] polls for new messages in a stream,
      consuming up to [count] message fragments, possibly across
      multiple images. Messages are delivered to the assembler's
      handler. Returns [Some num_consumed_fragments] on success,
      [None] on error. *)

  val image_release : t -> Image.t -> bool
  (** [image_release sub image] release the given image and relinquish
      desire to use the image directly. Returns [true] on success,
      [false] on failure. *)

  val close : t -> bool
  (** [close sub] closes subscription [sub] *)

  val is_closed : t -> bool
  (** [is_closed sub] returns [true] if subscription [sub] is closed, and [false] otherwise. *)

  val image_at_index : t -> int -> Image.t option
  (** [image_at_index sub idx] return the image at [idx]. Note:
      the returned image is considered retained by the application
      and thus must be released via [image_release] when
      finished or if the image becomes unavailable. *)

  val is_connected : t -> bool
  (** [is_connected sub] returns [true] if subscription [sub] is connected and [false] otherwise *)
end
