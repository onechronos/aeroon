open Ctypes

module Types(F: Ctypes.TYPE) = struct

  open F

  type clientd = unit ptr
  let clientd = ptr void

  let abstract_1 name =
    abstract ~name ~size:1 ~alignment:1

  let image : [`Image ] abstract typ =
    abstract_1 "aeron_image_t"

  let publication : [`Publication] abstract typ =
    abstract_1 "aeron_publication_t"

  let counters_reader : [`Counters_reader] abstract typ =
    abstract_1 "aeron_counters_reader_t"

  let context : [`Context] abstract typ =
    abstract_1 "aeron_context_t"

  let async_add_publication : [`Async_add_publication] abstract typ =
    abstract_1 "aeron_async_add_publication_t"

  let async_add_exclusive_publication : [`Async_add_exclusive_publication] abstract typ =
    abstract_1 "aeron_async_add_exclusive_publication_t"

  let async_add_subscription : [`Async_add_subscription] abstract typ =
    abstract_1 "aeron_async_add_subscription_t"

  let async_add_counter : [`Async_add_counter] abstract typ =
    abstract_1 "aeron_async_add_counter_t"

  let async_destination : [`Async_destination] abstract typ =
    abstract_1 "aeron_destination_t"

  let client : [`Client] abstract typ =
    abstract_1 "aeron_t"

  let exclusive_publication : [`Exclusive_publication] abstract typ =
    abstract_1 "aeron_exclusive_publication_t"

  let subscription : [`Subscription] abstract typ =
    abstract_1 "aeron_subscription_t"

  let counter : [`Counter] abstract typ =
    abstract_1 "aeron_counter_t"

  let header : [`Header] abstract typ =
    abstract_1 "aeron_header_t"

  let fragment_assembler : [`Fragment_assembler] abstract typ =
    abstract_1 "aeron_fragment_assembler_t"

  let image_fragment_assembler : [`Image_fragment_assembler] abstract typ =
    abstract_1 "aeron_image_fragment_assembler_t"

end
