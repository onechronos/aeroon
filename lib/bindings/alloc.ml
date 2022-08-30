open Ctypes
open C.Type

open struct
  let a ?finalise t = allocate ?finalise (ptr t) (from_voidp t null)
end

let ptr_context ?finalise () = a ?finalise context

let ptr_client ?finalise () = a ?finalise client

let ptr_publication ?finalise () = a ?finalise publication

let ptr_exclusive_publication ?finalise () = a ?finalise exclusive_publication

let ptr_subscription ?finalise () = a ?finalise subscription

let ptr_fragment_assembler ?finalise () = a ?finalise fragment_assembler

let ptr_image_fragment_assembler ?finalise () =
  a ?finalise image_fragment_assembler

let ptr_async_add_subscription ?finalise () = a ?finalise async_add_subscription

let ptr_async_add_publication ?finalise () = a ?finalise async_add_publication

let ptr_async_add_exclusive_publication ?finalise () =
  a ?finalise async_add_exclusive_publication
