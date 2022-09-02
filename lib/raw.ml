external version_major : unit -> int = "aa_version_major"

external version_minor : unit -> int = "aa_version_minor"

external version_patch : unit -> int = "aa_version_patch"

external version_full : unit -> string = "aa_version_full"

type context

type client

external context_init : unit -> context = "aa_context_init"

external context_del : context -> unit = "aa_context_finalize"

external client_init : context -> client = "aa_client_init"

external client_del : client -> unit = "aa_client_finalize"

external client_start : client -> unit = "aa_client_start"
