open Ctypes

module Types (F : Ctypes.TYPE) = struct
  open F

  type clientd = unit ptr

  let clientd = ptr void
  let abstract_1 name = abstract ~name ~size:1 ~alignment:1
  let image : [ `Image ] abstract typ = abstract_1 "aeron_image_t"

  let publication : [ `Publication ] abstract typ =
    abstract_1 "aeron_publication_t"

  let counters_reader : [ `Counters_reader ] abstract typ =
    abstract_1 "aeron_counters_reader_t"

  let context : [ `Context ] abstract typ = abstract_1 "aeron_context_t"

  let async_add_publication : [ `Async_add_publication ] abstract typ =
    abstract_1 "aeron_async_add_publication_t"

  let async_add_exclusive_publication :
      [ `Async_add_exclusive_publication ] abstract typ =
    abstract_1 "aeron_async_add_exclusive_publication_t"

  let async_add_subscription : [ `Async_add_subscription ] abstract typ =
    abstract_1 "aeron_async_add_subscription_t"

  let async_add_counter : [ `Async_add_counter ] abstract typ =
    abstract_1 "aeron_async_add_counter_t"

  let async_destination : [ `Async_destination ] abstract typ =
    abstract_1 "aeron_destination_t"

  let client : [ `Client ] abstract typ = abstract_1 "aeron_t"

  let exclusive_publication : [ `Exclusive_publication ] abstract typ =
    abstract_1 "aeron_exclusive_publication_t"

  let subscription : [ `Subscription ] abstract typ =
    abstract_1 "aeron_subscription_t"

  let counter : [ `Counter ] abstract typ = abstract_1 "aeron_counter_t"
  let header : [ `Header ] abstract typ = abstract_1 "aeron_header_t"

  let fragment_assembler : [ `Fragment_assembler ] abstract typ =
    abstract_1 "aeron_fragment_assembler_t"

  let image_fragment_assembler : [ `Image_fragment_assembler ] abstract typ =
    abstract_1 "aeron_image_fragment_assembler_t"

  let client_error_driver_timeout =
    constant "AERON_CLIENT_ERROR_DRIVER_TIMEOUT" int

  let client_error_client_timeout =
    constant "AERON_CLIENT_ERROR_CLIENT_TIMEOUT" int

  let client_error_conductor_service_timeout =
    constant "AERON_CLIENT_ERROR_CONDUCTOR_SERVICE_TIMEOUT" int

  let client_error_buffer_full = constant "AERON_CLIENT_ERROR_BUFFER_FULL" int

  let client_max_local_address_str_len =
    constant "AERON_CLIENT_MAX_LOCAL_ADDRESS_STR_LEN" int

  let publication_not_connected =
    constant "AERON_PUBLICATION_NOT_CONNECTED" int64_t

  let publication_back_pressured =
    constant "AERON_PUBLICATION_BACK_PRESSURED" int64_t

  let publication_admin_action =
    constant "AERON_PUBLICATION_ADMIN_ACTION" int64_t

  let publication_closed = constant "AERON_PUBLICATION_CLOSED" int64_t

  let publication_max_position_exceeded =
    constant "AERON_PUBLICATION_MAX_POSITION_EXCEEDED" int64_t

  let publication_error = constant "AERON_PUBLICATION_ERROR" int64_t
end
