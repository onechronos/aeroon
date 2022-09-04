external version_major : unit -> int = "aa_version_major"

external version_minor : unit -> int = "aa_version_minor"

external version_patch : unit -> int = "aa_version_patch"

external version_full : unit -> string = "aa_version_full"

type context

type client

external context_init : unit -> context = "aa_context_init"

external context_close : context -> unit = "aa_context_close"

external init : context -> client = "aa_init"

external close : client -> unit = "aa_close"

external start : client -> unit = "aa_start"

external main_do_work : client -> int = "aa_main_do_work"

external main_idle_strategy : client -> int -> unit = "aa_main_idle_strategy"

external is_closed : client -> bool = "aa_is_closed"

type async_add_publication

external async_add_publication :
  client -> string -> int -> async_add_publication = "aa_async_add_publication"

type publication

type 'a poll_result =
  | Ok of 'a
  | TryAgain
  | Error

type async_add_publication_poll_result = publication poll_result

external async_add_publication_poll :
  async_add_publication -> async_add_publication_poll_result
  = "aa_async_add_publication_poll"

type async_add_exclusive_publication

external async_add_exclusive_publication :
  client -> string -> int -> async_add_exclusive_publication
  = "aa_async_add_exclusive_publication"

type exclusive_publication

type async_add_exclusive_publication_poll_result =
  exclusive_publication poll_result

external async_add_exclusive_publication_poll :
  async_add_exclusive_publication -> async_add_exclusive_publication_poll_result
  = "aa_async_add_exclusive_publication_poll"

type subscription

type image

type on_image = subscription -> image -> unit

type async_add_subscription

external async_add_subscription :
  client ->
  string ->
  int ->
  on_image option ->
  on_image option ->
  async_add_subscription option = "aa_async_add_subscription"

type async_add_subscription_poll_result = subscription poll_result

external async_add_subscription_poll :
  async_add_subscription -> async_add_subscription_poll_result
  = "aa_async_add_subscription_poll"

type publication_error =
  | Not_connected
  | Back_pressured
  | Admin_action
  | Closed
  | Max_position_exceeded
  | Error

external publication_offer :
  publication -> string -> (int, publication_error) result
  = "aa_publication_offer"

external exclusive_publication_offer :
  exclusive_publication -> string -> (int, publication_error) result
  = "aa_exclusive_publication_offer"
