open Ctypes
open C.Type

let a t =
  allocate (ptr t) (from_voidp t null)

let ptr_context () = a context
let ptr_client () = a client
let ptr_publication () = a publication
let ptr_exclusive_publication () = a exclusive_publication
let ptr_subscription () = a subscription
let ptr_fragment_assembler () = a fragment_assembler
let ptr_async_add_subscription () = a async_add_subscription
let ptr_async_add_publication () = a async_add_publication
let ptr_async_add_exclusive_publication () = a async_add_exclusive_publication
