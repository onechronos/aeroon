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

type async_add_publication_poll_result =
  | Ok of publication
  | TryAgain
  | Error

external async_add_publication_poll :
  async_add_publication -> async_add_publication_poll_result
  = "aa_async_add_publication_poll"
