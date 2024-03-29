(** Raw C bindings *)

external version_major : unit -> int = "aa_version_major"

external version_minor : unit -> int = "aa_version_minor"

external version_patch : unit -> int = "aa_version_patch"

external version_full : unit -> string = "aa_version_full"

external errmsg : unit -> string = "aa_errmsg"

external errcode : unit -> int = "aa_errcode"

external error_code_invalid_channel : unit -> int
  = "aa_error_code_invalid_channel"

external error_code_unknown_subscription : unit -> int
  = "aa_error_code_unknown_subscription"

external error_code_unknown_publication : unit -> int
  = "aa_error_code_unknown_publication"

external error_code_channel_endpoint_error : unit -> int
  = "aa_error_code_channel_endpoint_error"

external error_code_unknown_counter : unit -> int
  = "aa_error_code_unknown_counter"

external error_code_unknown_command_type_id : unit -> int
  = "aa_error_code_unknown_command_type_id"

external error_code_malformed_command : unit -> int
  = "aa_error_code_malformed_command"

external error_code_not_supported : unit -> int = "aa_error_code_not_supported"

external error_code_unknown_host : unit -> int = "aa_error_code_unknown_host"

external error_code_resource_temporarily_unavailable : unit -> int
  = "aa_error_code_resource_temporarily_unavailable"

external error_code_generic_error : unit -> int = "aa_error_code_generic_error"

external nano_clock : unit -> int = "aa_nano_clock"

external epoch_clock : unit -> int = "aa_epoch_clock"

type context

type client

external context_init : unit -> context option = "aa_context_init"

external context_close : context -> bool = "aa_context_close"

external init : context -> client option = "aa_init"

external close : client -> bool = "aa_close"

external start : client -> bool = "aa_start"

external main_do_work : client -> int = "aa_main_do_work"

external main_idle_strategy : client -> int -> unit = "aa_main_idle_strategy"

external is_closed : client -> bool = "aa_is_closed"

type async_add_publication

external async_add_publication :
  client -> string -> int -> async_add_publication option
  = "aa_async_add_publication"

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
  client -> string -> int -> async_add_exclusive_publication option
  = "aa_async_add_exclusive_publication"

type exclusive_publication

type async_add_exclusive_publication_poll_result =
  exclusive_publication poll_result

external async_add_exclusive_publication_poll :
  async_add_exclusive_publication -> async_add_exclusive_publication_poll_result
  = "aa_async_add_exclusive_publication_poll"

type subscription

type image

type async_add_subscription

external async_add_subscription :
  client -> string -> int -> async_add_subscription option
  = "aa_async_add_subscription"

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

external publication_close : publication -> bool = "aa_publication_close"

external exclusive_publication_close : exclusive_publication -> bool
  = "aa_exclusive_publication_close"

external subscription_close : subscription -> bool = "aa_subscription_close"

external publication_is_closed : publication -> bool
  = "aa_publication_is_closed"

external exclusive_publication_is_closed : exclusive_publication -> bool
  = "aa_exclusive_publication_is_closed"

external subscription_is_closed : subscription -> bool
  = "aa_subscription_is_closed"

external image_is_closed : image -> bool = "aa_image_is_closed"

type fragment_handler = string (* TODO: -> header *) -> unit

type fragment_assembler

external fragment_assembler_create :
  fragment_handler -> fragment_assembler option = "aa_fragment_assembler_create"

type image_fragment_assembler

external image_fragment_assembler_create :
  fragment_handler -> image_fragment_assembler option
  = "aa_image_fragment_assembler_create"

external subscription_poll :
  subscription ->
  (* TODO: -> fragment_assembler_handler *) fragment_assembler ->
  int ->
  int option = "aa_subscription_poll"

external image_poll :
  image ->
  (* TODO: -> image_fragment_assembler_handler *) image_fragment_assembler ->
  int ->
  int option = "aa_image_poll"

external image_position : image -> int = "aa_image_position"

external idle_strategy_sleeping_idle : int -> int -> unit
  = "aa_idle_strategy_sleeping_idle"

external idle_strategy_yielding_idle : int -> int -> unit
  = "aa_idle_strategy_yielding_idle"

external idle_strategy_busy_spinning_idle : int -> int -> unit
  = "aa_idle_strategy_busy_spinning_idle"

external idle_strategy_noop_idle : int -> int -> unit
  = "aa_idle_strategy_noop_idle"

external idle_strategy_backoff_idle : int -> int -> unit
  = "aa_idle_strategy_backoff_idle"

external subscription_image_at_index : subscription -> int -> image option
  = "aa_subscription_image_at_index"

external subscription_is_connected : subscription -> bool
  = "aa_subscription_is_connected"

external publication_is_connected : publication -> bool
  = "aa_publication_is_connected"

external exclusive_publication_is_connected : exclusive_publication -> bool
  = "aa_exclusive_publication_is_connected"

external exclusive_publication_try_claim :
  exclusive_publication -> string -> bool = "aa_exclusive_publication_try_claim"

external subscription_image_release : subscription -> image -> bool
  = "aa_subscription_image_release"
