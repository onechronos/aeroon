module Version : sig
  val major : int

  val minor : int

  val patch : int

  val full : string
end

module IdleStrategy : sig
  type t = int -> int -> unit

  val sleeping : t

  val yielding : t

  val busy_spinning : t

  val noop : t

  val backoff : t
end

module Context : sig
  type t

  val create : unit -> t option

  val close : t -> bool
end

module Client : sig
  type t

  val create : Context.t -> t option

  val start : t -> bool

  val close : t -> bool
end

val main_idle_strategy : Client.t -> int -> unit

type publication_error = Raw.publication_error

val string_of_publication_error : publication_error -> string

module Publication : sig
  type t

  val create :
    ?pause_between_attempts_s:float ->
    Client.t ->
    string ->
    int ->
    (t, string) result

  val close : t -> bool

  val offer : t -> string -> (int, publication_error) result

  val is_connected : t -> bool

  val is_closed : t -> bool
end

module ExclusivePublication : sig
  type t

  val create :
    ?pause_between_attempts_s:float ->
    Client.t ->
    string ->
    int ->
    (t, string) result

  val close : t -> bool

  val offer : t -> string -> (int, publication_error) result

  val is_connected : t -> bool

  val is_closed : t -> bool

  val try_claim : t -> string -> bool
end

type fragment_handler = string -> unit

module Image : sig
  type t

  val is_closed : t -> bool

  type a

  val create_assembler : fragment_handler -> a option

  val poll : t -> a -> int -> int option
end

module Subscription : sig
  type t

  val create :
    ?pause_between_attempts_s:float ->
    Client.t ->
    string ->
    int ->
    (t, string) result

  type a

  val create_assembler : fragment_handler -> a option

  val poll : t -> a -> int -> int option

  val image_release : t -> Image.t -> bool

  val close : t -> bool

  val is_closed : t -> bool

  val image_at_index : t -> int -> Image.t option

  val is_connected : t -> bool
end
