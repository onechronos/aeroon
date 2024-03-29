#define CAML_NAME_SPACE

/* OCaml's C FFI */
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/callback.h>
#include <caml/misc.h>
#include <caml/intext.h>
#include <caml/fail.h>
#include <caml/threads.h>

/* C standard library */
#include <string.h>
#include <assert.h>

/* Aeron */
#include <aeronc.h>
#include <aeron_agent.h>
#include <command/aeron_control_protocol.h>

#define context_val(v)                         (*((aeron_context_t                         **) Data_custom_val(v)))
#define client_val(v)                          (*((aeron_t                                 **) Data_custom_val(v)))
#define async_add_publication_val(v)           (*((aeron_async_add_publication_t           **) Data_custom_val(v)))
#define async_add_exclusive_publication_val(v) (*((aeron_async_add_exclusive_publication_t **) Data_custom_val(v)))
#define publication_val(v)                     (*((aeron_publication_t                     **) Data_custom_val(v)))
#define exclusive_publication_val(v)           (*((aeron_exclusive_publication_t           **) Data_custom_val(v)))
#define async_add_subscription_val(v)          (*((aeron_async_add_subscription_t          **) Data_custom_val(v)))
#define subscription_val(v)                    (*((aeron_subscription_t                    **) Data_custom_val(v)))
#define image_val(v)                           (*((aeron_image_t                           **) Data_custom_val(v)))

// version_major : unit -> int
CAMLprim value aa_version_major(value _unit)
{
  CAMLparam1(_unit);
  CAMLlocal1(o_res);
  int version_major = aeron_version_major();
  o_res = Val_int(version_major);
  CAMLreturn(o_res);
}

// version_minor : unit -> int
CAMLprim value aa_version_minor(value _unit)
{
  CAMLparam1(_unit);
  CAMLlocal1(o_res);
  int version_minor = aeron_version_minor();
  o_res = Val_int(version_minor);
  CAMLreturn(o_res);
}

// version_patch : unit -> int
CAMLprim value aa_version_patch(value _unit)
{
  CAMLparam1(_unit);
  CAMLlocal1(o_res);
  int version_patch = aeron_version_patch();
  o_res = Val_int(version_patch);
  CAMLreturn(o_res);
}

// version_full : unit -> string
CAMLprim value aa_version_full(value _unit)
{
  CAMLparam1(_unit);
  CAMLlocal1(o_res);
  const char *version_full = aeron_version_full();
  o_res = caml_copy_string( version_full );
  CAMLreturn(o_res);
}

// errmsg : unit -> string
CAMLprim value aa_errmsg(value _unit)
{
  CAMLparam1(_unit);
  CAMLlocal1(o_res);
  o_res = caml_copy_string( aeron_errmsg() );
  CAMLreturn(o_res);
}

// errcode : unit -> int
CAMLprim value aa_errcode(value _unit)
{
  CAMLparam1(_unit);
  CAMLreturn(Val_int( aeron_errcode() ) );
}

// error_code_invalid_channel : unit -> int
CAMLprim value aa_error_code_invalid_channel(value _unit)
{
  CAMLparam1(_unit);
  CAMLreturn(Val_int(AERON_ERROR_CODE_INVALID_CHANNEL));
}

// error_code_unknown_subscription : unit -> int
CAMLprim value aa_error_code_unknown_subscription(value _unit)
{
  CAMLparam1(_unit);
  CAMLreturn(Val_int(AERON_ERROR_CODE_UNKNOWN_SUBSCRIPTION));
}

// error_code_unknown_publication : unit -> int
CAMLprim value aa_error_code_unknown_publication(value _unit)
{
  CAMLparam1(_unit);
  CAMLreturn(Val_int(AERON_ERROR_CODE_UNKNOWN_PUBLICATION));
}

// error_code_channel_endpoint_error : unit -> int
CAMLprim value aa_error_code_channel_endpoint_error(value _unit)
{
  CAMLparam1(_unit);
  CAMLreturn(Val_int(AERON_ERROR_CODE_CHANNEL_ENDPOINT_ERROR));
}

// error_code_unknown_counter : unit -> int
CAMLprim value aa_error_code_unknown_counter(value _unit)
{
  CAMLparam1(_unit);
  CAMLreturn(Val_int(AERON_ERROR_CODE_UNKNOWN_COUNTER));
}

// error_code_unknown_command_type_id : unit -> int
CAMLprim value aa_error_code_unknown_command_type_id(value _unit)
{
  CAMLparam1(_unit);
  CAMLreturn(Val_int(AERON_ERROR_CODE_UNKNOWN_COMMAND_TYPE_ID));
}

// error_code_malformed_command : unit -> int
CAMLprim value aa_error_code_malformed_command(value _unit)
{
  CAMLparam1(_unit);
  CAMLreturn(Val_int(AERON_ERROR_CODE_MALFORMED_COMMAND));
}

// error_code_not_supported : unit -> int
CAMLprim value aa_error_code_not_supported(value _unit)
{
  CAMLparam1(_unit);
  CAMLreturn(Val_int(AERON_ERROR_CODE_NOT_SUPPORTED));
}

// error_code_unknown_host : unit -> int
CAMLprim value aa_error_code_unknown_host(value _unit)
{
  CAMLparam1(_unit);
  CAMLreturn(Val_int(AERON_ERROR_CODE_UNKNOWN_HOST));
}


// error_code_resource_temporarily_unavailable : unit -> int
CAMLprim value aa_error_code_resource_temporarily_unavailable(value _unit)
{
  CAMLparam1(_unit);
  CAMLreturn(Val_int(AERON_ERROR_CODE_RESOURCE_TEMPORARILY_UNAVAILABLE));
}

// error_code_generic_error : unit -> int
CAMLprim value aa_error_code_generic_error(value _unit)
{
  CAMLparam1(_unit);
  CAMLreturn(Val_int(AERON_ERROR_CODE_GENERIC_ERROR));
}


// nano_clock : unit -> int
CAMLprim value aa_nano_clock(value _unit)
{
  CAMLparam1(_unit);
  CAMLreturn(Val_int( aeron_nano_clock() ) );
}

// epoch_clock : unit -> int
CAMLprim value aa_epoch_clock(value _unit)
{
  CAMLparam1(_unit);
  CAMLreturn(Val_int( aeron_epoch_clock() ) );
}

// context_close : context -> bool
CAMLprim value aa_context_close(value o_context)
{
  CAMLparam1(o_context);
  aeron_context_t* context = context_val(o_context);
  int err = aeron_context_close(context);
  if ( err == 0 ) {
    CAMLreturn(Val_true);
  }
  else if ( err == -1 ) {
    CAMLreturn(Val_false);
  }
  else {
    assert(false);
  }
}

// close : client -> bool
CAMLprim value aa_close(value o_client)
{
  CAMLparam1(o_client);
  aeron_t* client = client_val(o_client);
  int err = aeron_close(client);
  if ( err == 0 ) {
    CAMLreturn(Val_true);
  }
  else if ( err == -1 ) {
    CAMLreturn(Val_false);
  }
  else {
    assert(false);
  }
}

// context_init : unit -> context option
CAMLprim value aa_context_init(value _unit)
{
  CAMLparam1(_unit);
  CAMLlocal1(o_context);

  aeron_context_t* context = NULL;
  int res = aeron_context_init(&context);

  if ( res == 0 ) {
    o_context = caml_alloc_small( sizeof(aeron_context_t*), Abstract_tag);
    assert(context != NULL);
    context_val(o_context) = context;
    CAMLreturn(caml_alloc_some(o_context));
  }
  else if ( res < 0 ) {
    CAMLreturn(Val_none);
  }
  else {
    assert(false);
  }
}

// init : context -> client optoin
CAMLprim value aa_init(value o_context)
{
  CAMLparam1(o_context);
  CAMLlocal1(o_client);

  aeron_context_t* ctx = context_val(o_context);
  aeron_t *client = NULL;
  int err = aeron_init(&client, ctx);

  if ( err == 0 ) {
    o_client = caml_alloc_small( sizeof(aeron_t*), Abstract_tag);
    client_val(o_client) = client;
    CAMLreturn(caml_alloc_some(o_client));
  }
  else if ( err < 0 ) {
    CAMLreturn(Val_none);
  }
  else {
    assert(false);
  }
}

// start : client -> bool
CAMLprim value aa_start(value o_client)
{
  CAMLparam1(o_client);

  aeron_t* client = client_val(o_client);
  int err = aeron_start(client);

  if ( err == 0 ) {
    CAMLreturn(Val_true);
  }
  else if ( err < 0 ) {
    CAMLreturn(Val_false);
  }
  else {
    assert(false);
  }
}

// main_do_work : client -> int
CAMLprim value aa_main_do_work(value o_client)
{
  CAMLparam1(o_client);
  aeron_t* client = client_val( o_client );
  int err = aeron_main_do_work( client );
  CAMLreturn(Val_int(err));
}

// main_idle_strategy : client -> init -> unit
CAMLprim value aa_main_idle_strategy(value o_client, value o_work_count)
{
  CAMLparam2(o_client, o_work_count);

  aeron_t* client = client_val( o_client );
  int work_count = Int_val( o_work_count );

  aeron_main_idle_strategy( client, work_count );
  CAMLreturn(Val_unit);
}

// is_closed : client -> bool
CAMLprim value aa_is_closed(value o_client)
{
  CAMLparam1(o_client);

  aeron_t* client = client_val( o_client );
  bool result = aeron_is_closed( client );
  CAMLreturn(Val_bool(result));
}

// async_add_publication : client -> string -> int -> async_add_publication option
CAMLprim value aa_async_add_publication(value o_client, value o_uri, value o_stream_id)
{
  CAMLparam3(o_client, o_uri, o_stream_id);
  CAMLlocal1(o_async);

  aeron_t* client = client_val( o_client );
  const char* uri = String_val(o_uri);
  int32_t stream_id = Int_val(o_stream_id);
  aeron_async_add_publication_t* async = NULL;
  int err = aeron_async_add_publication( &async, client, uri, stream_id );
  if ( err == 0 ) {
    o_async = caml_alloc_small( sizeof(aeron_async_add_publication_t*), Abstract_tag);
    async_add_publication_val(o_async) = async;
    CAMLreturn(caml_alloc_some(o_async));
  }
  else if ( err == -1 ) {
    CAMLreturn(Val_none);
  }
  else {
    assert(false);
  }
}

// async_add_publication_poll : async_add_publication -> publication poll_result
CAMLprim value aa_async_add_publication_poll(value o_async)
{
  CAMLparam1(o_async);
  CAMLlocal2(o_pub, o_res);

  aeron_publication_t* pub = NULL;
  aeron_async_add_publication_t* async = async_add_publication_val(o_async);
  int err = aeron_async_add_publication_poll( &pub, async );
  if ( err == 1 ) {
    o_pub = caml_alloc_small( sizeof(aeron_publication_t*), Abstract_tag);
    publication_val(o_pub) = pub;

    // Ok pub
    o_res = caml_alloc(1, 0);
    Store_field( o_res, 0, o_pub );
  }
  else if ( err == 0 ) {
    // TryAgain
    o_res = Val_int(0);
  }
  else if ( err == -1 ) {
    // Error
    o_res = Val_int(1);
  }
  else {
    assert(false);
  }
  CAMLreturn(o_res);
}

// async_add_exclusive_publication : client -> string -> int ->
//   async_add_exclusive_publication option
CAMLprim value aa_async_add_exclusive_publication(value o_client, value o_uri, value o_stream_id)
{
  CAMLparam3(o_client, o_uri, o_stream_id);
  CAMLlocal1(o_async);

  aeron_t* client = client_val( o_client );
  const char* uri = String_val(o_uri);
  int32_t stream_id = Int_val(o_stream_id);
  aeron_async_add_exclusive_publication_t* async = NULL;
  int err = aeron_async_add_exclusive_publication( &async, client, uri, stream_id );
  if ( err == 0 ) {
    o_async = caml_alloc_small( sizeof(aeron_t*), Abstract_tag);
    async_add_exclusive_publication_val(o_async) = async;
    CAMLreturn(caml_alloc_some(o_async));
  }
  else if ( err == -1 ) {
    CAMLreturn(Val_none);
  }
  else {
    assert(false);
  }
}

// async_add_exclusive_publication_poll : async_add_exclusive_publication ->
//   exclusive_publication poll_result
CAMLprim value aa_async_add_exclusive_publication_poll(value o_async)
{
  CAMLparam1(o_async);
  CAMLlocal2(o_pub, o_res);

  aeron_exclusive_publication_t* pub = NULL;
  aeron_async_add_exclusive_publication_t* async = async_add_exclusive_publication_val(o_async);
  int err = aeron_async_add_exclusive_publication_poll( &pub, async );
  if ( err == 1 ) {
    o_pub = caml_alloc_small( sizeof(aeron_exclusive_publication_t*), Abstract_tag);
    exclusive_publication_val(o_pub) = pub;

    // Ok pub
    o_res = caml_alloc(1, 0);
    Store_field( o_res, 0, o_pub );
  }
  else if ( err == 0 ) {
    // TryAgain
    o_res = Val_int(0);
  }
  else if ( err == -1 ) {
    // Error
    o_res = Val_int(1);
  }
  else {
    assert(false);
  }
  CAMLreturn(o_res);
}

// async_add_subscription : client -> string -> int -> async_add_subscription option
CAMLprim value aa_async_add_subscription(value o_client,
					 value o_uri,
					 value o_stream_id )
// note: for simplicity, we ignore on_available_image and ignore
// on_unavailable_image callbacks
{
  CAMLparam3(o_client, o_uri, o_stream_id );
  CAMLlocal2(o_async, o_res);

  aeron_t* client = client_val(o_client);
  const char* uri = String_val(o_uri);
  int32_t stream_id = Int_val(o_stream_id);

  aeron_async_add_subscription_t* async = NULL;
  int err = aeron_async_add_subscription( &async,
					  client,
					  uri,
					  stream_id,
					  NULL, NULL, NULL, NULL );
  if ( err == 0 ) {
    o_async = caml_alloc_small( sizeof(aeron_async_add_subscription_t*), Abstract_tag);
    async_add_subscription_val( o_async ) = async;

    // Some async
    o_res = caml_alloc_some( o_async );
  }
  else if ( err == -1 ) {
    // None
    o_res = Val_none;
  }
    
  CAMLreturn(o_res);
}

// async_add_subscription_poll : async_add_subscription -> subscription poll_result
CAMLprim value aa_async_add_subscription_poll(value o_async)
{
  CAMLparam1(o_async);
  CAMLlocal2(o_sub, o_res);

  aeron_async_add_subscription_t* async = async_add_subscription_val(o_async);
  aeron_subscription_t* sub = NULL;
  int err = aeron_async_add_subscription_poll( &sub, async );
  if ( err == 1 ) {
    o_sub = caml_alloc_small( sizeof(aeron_subscription_t*), Abstract_tag);
    subscription_val(o_sub) = sub;

    // Ok sub
    o_res = caml_alloc(1, 0);
    Store_field( o_res, 0, o_sub );
  }
  else if ( err == 0 ) {
    // TryAgain
    o_res = Val_int(0);
  }
  else if ( err == -1 ) {
    // Error
    o_res = Val_int(1);
  }
  else {
    assert(false);
  }
  CAMLreturn(o_res);
}


// publication_error_code : int -> publication_error
CAMLprim value publication_error_code(int64_t res)
{
  CAMLparam0 ();

  int variant_code = -1;
  if ( res == AERON_PUBLICATION_NOT_CONNECTED ) {
    variant_code = 0;
  }
  else if ( res == AERON_PUBLICATION_BACK_PRESSURED ) {
    variant_code = 1;
  }
  else if ( res == AERON_PUBLICATION_ADMIN_ACTION ) {
    variant_code = 2;
  }
  else if ( res == AERON_PUBLICATION_CLOSED ) {
    variant_code = 3;
  }
  else if ( res == AERON_PUBLICATION_MAX_POSITION_EXCEEDED ) {
    variant_code = 4;
  }
  else if ( res == AERON_PUBLICATION_ERROR ) {
    variant_code = 5;
  }
  else {
    assert(false);
  }
  CAMLreturn(Val_int(variant_code));

}

// publication_result : int -> (int, publication_error) result
CAMLprim value publication_result(int64_t res)
{
  CAMLparam0 ();
  CAMLlocal1( o_res );

  if ( res >= 0L ) {
    // Ok position
    o_res = caml_alloc(1, 0);
    Store_field( o_res, 0, Val_int((int)res) );
  }
  else {
    // Error code
    o_res = caml_alloc(1, 1);
    Store_field( o_res, 0, publication_error_code(res) );
  }
  CAMLreturn( o_res );
}

// publication_offer : publication -> string -> (int, publication_error) result
CAMLprim value aa_publication_offer(value o_publication, value o_buffer)
// here, we do not expose aeron_reserved_value_supplier (and its data)
{
  CAMLparam2(o_publication, o_buffer);
  CAMLlocal1(o_res);
  aeron_publication_t* publication = publication_val(o_publication);
  const char* buffer = String_val(o_buffer);
  size_t length = caml_string_length(o_buffer);

  // note cast of result, and non-use of aeron_reserved_value_supplier
  int64_t res = aeron_publication_offer( publication, buffer, length, NULL, NULL );

  CAMLreturn(publication_result(res));
}

// exclusive_publication_offer : exclusive_publication -> string ->
//   (int, publication_error) result
CAMLprim value aa_exclusive_publication_offer(value o_exclusive_publication, value o_buffer)
// identical to aa_publication_offer, except that we use
// aeron_exclusive_publcation_offer instead of aeron_publication_offer
// essentially: 's/pulication/exclusive_publication/g'
{
  CAMLparam2(o_exclusive_publication, o_buffer);
  aeron_exclusive_publication_t* exclusive_publication = exclusive_publication_val(o_exclusive_publication);
  const char* buffer = String_val(o_buffer);
  size_t length = caml_string_length(o_buffer);

  int64_t res = aeron_exclusive_publication_offer( exclusive_publication, buffer, length, NULL, NULL );
  CAMLreturn(publication_result(res));
}

// exclusive_publication_try_claim : exclusive_publication -> string -> bool
CAMLprim value aa_exclusive_publication_try_claim( value o_exclusive_publication,
						   value o_buffer )
{
  CAMLparam2( o_exclusive_publication, o_buffer );
  aeron_exclusive_publication_t* exclusive_publication =
    exclusive_publication_val(o_exclusive_publication);
  const char* buffer = String_val(o_buffer);
  int length = caml_string_length(o_buffer);

  aeron_buffer_claim_t buffer_claim;
  while (aeron_exclusive_publication_try_claim( exclusive_publication,
						length,
						&buffer_claim ) < 0) {
    aeron_idle_strategy_busy_spinning_idle(NULL, 0);
  }
  
  memcpy(buffer_claim.data, buffer, length);
  int err = aeron_buffer_claim_commit(&buffer_claim);
  if ( err == 0 ) {
    CAMLreturn(Val_true);
  }
  else if ( err == -1 ) {
    CAMLreturn(Val_false);
  }
  else {
    assert(false);
  }
}

// subscription_close : subscription -> bool
CAMLprim value aa_subscription_close(value o_subscription)
{
  CAMLparam1(o_subscription);
  aeron_subscription_t* subscription =
    subscription_val(o_subscription);
  int res = aeron_subscription_close( subscription, NULL, NULL );
  if (res == 0) {
    CAMLreturn(Val_true);
  }
  else if ( res == -1 ) {
    CAMLreturn(Val_false);
  }
  else {
    assert(false);
  }
}


// publication_close : publication -> bool
CAMLprim value aa_publication_close(value o_publication)
{
  CAMLparam1(o_publication);
  aeron_publication_t* publication =
    publication_val(o_publication);
  int res = aeron_publication_close( publication, NULL, NULL );
  if (res == 0) {
    CAMLreturn(Val_true);
  }
  else if ( res == -1 ) {
    CAMLreturn(Val_false);
  }
  else {
    assert(false);
  }
}

// exclusive_publication_close : exclusive_publication -> bool
CAMLprim value aa_exclusive_publication_close(value o_exclusive_publication)
{
  CAMLparam1(o_exclusive_publication);
  aeron_exclusive_publication_t* exclusive_publication =
    exclusive_publication_val(o_exclusive_publication);
  int res = aeron_exclusive_publication_close( exclusive_publication, NULL, NULL );
  if (res == 0) {
    CAMLreturn(Val_true);
  }
  else if ( res == -1 ) {
    CAMLreturn(Val_false);
  }
  else {
    assert(false);
  }
}

// --not-exposed-- as an OCaml function
void aa_fragment_handler(void* clientd,
			 const uint8_t* buffer,
			 size_t length,
			 aeron_header_t* _header) // ignore header for now
{
  CAMLparam0();
  CAMLlocal3(o_buffer, o_function, o_res);
  caml_c_thread_register();
  o_buffer = caml_alloc_initialized_string(length, buffer);
  o_function = *((value*) clientd);
  o_res = caml_callback( o_function, o_buffer );
  CAMLreturn0;
}

typedef struct _fragment_assembler_z {
  value* cb;
  aeron_fragment_assembler_t* a;
} fragment_assembler_z;

typedef struct _image_fragment_assembler_z {
  value* cb;
  aeron_image_fragment_assembler_t* a;
} image_fragment_assembler_z;


#define fragment_assembler_z_val(v) ((fragment_assembler_z*) Data_custom_val(v))
#define image_fragment_assembler_z_val(v) ((image_fragment_assembler_z*) Data_custom_val(v))

void fragment_assembler_z_fini(value o_fragment_assembler)
{
  // CAMLparam1(o_fragment_assembler); <-- don't do this!
  fragment_assembler_z* fragment_assembler =
    fragment_assembler_z_val(o_fragment_assembler);
  caml_remove_global_root(fragment_assembler->cb);
  free(fragment_assembler->cb);
  // sample clients seem to suggest that this is safe even after a
  // client and context are closed, so we don't bother to check the
  // status of the client and context here.
  aeron_fragment_assembler_delete(fragment_assembler->a);
}

void image_fragment_assembler_z_fini(value o_image_fragment_assembler)
{
  // CAMLparam1(o_image_fragment_assembler); <-- don't do this!
  image_fragment_assembler_z* image_fragment_assembler =
    image_fragment_assembler_z_val(o_image_fragment_assembler);
  caml_remove_global_root(image_fragment_assembler->cb);
  free(image_fragment_assembler->cb);
  // sample clients seem to suggest that this is safe even after a
  // client and context are closed, so we don't bother to check the
  // status of the client and context here.
  aeron_image_fragment_assembler_delete(image_fragment_assembler->a);
}

static struct custom_operations fragment_assembler_z_ops =
  {
   .identifier = "aa.fragment_assembler_z",
   .finalize = fragment_assembler_z_fini,
   .compare = custom_compare_default,
   .hash = custom_hash_default,
   .serialize = custom_serialize_default,
   .deserialize = custom_deserialize_default,
  };

static struct custom_operations image_fragment_assembler_z_ops =
  {
   .identifier = "aa.image_fragment_assembler_z",
   .finalize = image_fragment_assembler_z_fini,
   .compare = custom_compare_default,
   .hash = custom_hash_default,
   .serialize = custom_serialize_default,
   .deserialize = custom_deserialize_default,
  };


// fragment_assembler_create : (string -> unit) -> fragment_assembler option
CAMLprim value aa_fragment_assembler_create( value o_delegate )
{
  CAMLparam1( o_delegate );
  CAMLlocal2( o_fragment_assembler, o_res );

  fragment_assembler_z fragment_assembler;
  aeron_fragment_assembler_t* a;
  fragment_assembler.cb = malloc(sizeof(value));
  *fragment_assembler.cb = o_delegate;
  void* clientd = (void*)fragment_assembler.cb;
  int res = aeron_fragment_assembler_create( &a, aa_fragment_handler, clientd );
  if ( res == 0 ) {
    // Some fragment_assembler
    caml_register_global_root(fragment_assembler.cb);
    fragment_assembler.a = a;
    o_fragment_assembler = caml_alloc_custom( &fragment_assembler_z_ops,
					      sizeof(fragment_assembler_z),
					      0, 1);
    memcpy( fragment_assembler_z_val(o_fragment_assembler),
	    &fragment_assembler,
	    sizeof(fragment_assembler_z) );
    o_res = caml_alloc_some( o_fragment_assembler );
  }
  else if ( res == -1 ) {
    // None
    free(fragment_assembler.cb);
    o_res = Val_none;
  }
  else {
    assert(false);
  }
  CAMLreturn(o_res);
}

// image_fragment_assembler_create : (string -> unit) -> image_fragment_assembler option
CAMLprim value aa_image_fragment_assembler_create( value o_delegate )
{
  CAMLparam1( o_delegate );
  CAMLlocal2( o_image_fragment_assembler, o_res );

  image_fragment_assembler_z image_fragment_assembler;
  aeron_image_fragment_assembler_t* a;
  image_fragment_assembler.cb = malloc(sizeof(value));
  *image_fragment_assembler.cb = o_delegate;
  void* clientd = (void*)image_fragment_assembler.cb;
  int res = aeron_image_fragment_assembler_create( &a, aa_fragment_handler, clientd );
  if ( res == 0 ) {
    // Some fragment_assembler
    caml_register_global_root(image_fragment_assembler.cb);
    image_fragment_assembler.a = a;
    o_image_fragment_assembler = caml_alloc_custom( &image_fragment_assembler_z_ops,
						    sizeof(image_fragment_assembler_z),
						    0, 1);
    memcpy( image_fragment_assembler_z_val(o_image_fragment_assembler),
	    &image_fragment_assembler,
	    sizeof(image_fragment_assembler_z) );
    o_res = caml_alloc_some( o_image_fragment_assembler );
  }
  else if ( res == -1 ) {
    // None
    free(image_fragment_assembler.cb);
    o_res = Val_none;
  }
  else {
    assert(false);
  }
  CAMLreturn(o_res);
}

// subscription_poll : subscription -> fragment_assembler -> int -> int option
CAMLprim value aa_subscription_poll( value o_subscription,
				     // TODO: o_fragment_assembler_handler
				     value o_fragment_assembler,
				     value o_fragment_limit )
{
  CAMLparam3(o_subscription, o_fragment_assembler, o_fragment_limit );
  CAMLlocal1(o_res);
  aeron_subscription_t* subscription = subscription_val(o_subscription);
  fragment_assembler_z* fragment_assembler = fragment_assembler_z_val(o_fragment_assembler);

  size_t fragment_limit = Int_val(o_fragment_limit);
  int res = aeron_subscription_poll( subscription,
				     aeron_fragment_assembler_handler,
				     fragment_assembler->a,
				     fragment_limit );
  if ( res >= 0 ) {
    // Some num_fragments_received
    o_res = caml_alloc_some( Val_int(res) );
  }
  else { /* Note: documentation says that (-1) is the only other
	    possible result, but sample clients indicate otherwise */
    // None
    o_res = Val_none;
  }
  CAMLreturn(o_res);
}

// image_poll : image -> image_fragment_assembler -> int -> int option
CAMLprim value aa_image_poll( value o_image,
			      // TODO: o_fragment_assembler_handler
			      value o_image_fragment_assembler,
			      value o_fragment_limit )
// like subscription_poll, but 's/subscription/image/g'
{
  CAMLparam3(o_image, o_image_fragment_assembler, o_fragment_limit );
  CAMLlocal1(o_res);
  aeron_image_t* image = image_val(o_image);
  image_fragment_assembler_z* image_fragment_assembler =
    image_fragment_assembler_z_val(o_image_fragment_assembler);
  size_t fragment_limit = Int_val(o_fragment_limit);
  int res = aeron_image_poll( image,
			      aeron_image_fragment_assembler_handler,
			      image_fragment_assembler->a,
			      fragment_limit );
  if ( res >= 0 ) {
    // Some num_fragments_received
    o_res = caml_alloc_some( Val_int(res) );
  }
  else { /* Note: documentation says that (-1) is the only other
	    possible result, but sample clients indicate otherwise */
    // None
    o_res = Val_none;
  }
  CAMLreturn(o_res);
}

// idle_strategy_sleeping_idle : int -> int -> unit
CAMLprim value aa_idle_strategy_sleeping_idle( value o_nanos, value o_work_count )
{
  CAMLparam2(o_nanos, o_work_count);
  int64_t nanos = Int_val(o_nanos);
  int work_count = Int_val(o_work_count);
  aeron_idle_strategy_sleeping_idle( (void*)(&nanos), work_count );
  CAMLreturn(Val_unit);
}

// idle_strategy_yielding_idle : int -> int -> unit
CAMLprim value aa_idle_strategy_yielding_idle( value o_nanos, value o_work_count )
{
  CAMLparam2(o_nanos, o_work_count);
  int64_t nanos = Int_val(o_nanos);
  int work_count = Int_val(o_work_count);
  aeron_idle_strategy_yielding_idle( (void*)(&nanos), work_count );
  CAMLreturn(Val_unit);
}

// idle_strategy_busy_spinning_idle : int -> int -> unit
CAMLprim value aa_idle_strategy_busy_spinning_idle( value o_nanos, value o_work_count )
{
  CAMLparam2(o_nanos, o_work_count);
  int64_t nanos = Int_val(o_nanos);
  int work_count = Int_val(o_work_count);
  aeron_idle_strategy_busy_spinning_idle( (void*)(&nanos), work_count );
  CAMLreturn(Val_unit);
}

// idle_strategy_noop_idle : int -> int -> unit
CAMLprim value aa_idle_strategy_noop_idle( value o_nanos, value o_work_count )
{
  CAMLparam2(o_nanos, o_work_count);
  int64_t nanos = Int_val(o_nanos);
  int work_count = Int_val(o_work_count);
  aeron_idle_strategy_noop_idle( (void*)(&nanos), work_count );
  CAMLreturn(Val_unit);
}

// idle_strategy_backoff_idle : int -> int -> unit
CAMLprim value aa_idle_strategy_backoff_idle( value o_nanos, value o_work_count )
{
  CAMLparam2(o_nanos, o_work_count);
  int64_t nanos = Int_val(o_nanos);
  int work_count = Int_val(o_work_count);
  aeron_idle_strategy_backoff_idle( (void*)(&nanos), work_count );
  CAMLreturn(Val_unit);
}

// subscription_image_at_index : subscription -> int -> image option
CAMLprim value aa_subscription_image_at_index( value o_subscription, value o_index )
{
  CAMLparam2(o_subscription, o_index);
  CAMLlocal2(o_image, o_res);
  aeron_subscription_t* subscription = subscription_val(o_subscription);
  size_t index = Int_val(o_index);
  aeron_image_t* image = aeron_subscription_image_at_index( subscription, index );
  if ( image == NULL ) {
    o_res = Val_none;
  }
  else {
    o_image = caml_alloc_small( sizeof(aeron_image_t*), Abstract_tag );
    image_val(o_image) = image;
    o_res = caml_alloc_some( o_image );
  }
  CAMLreturn(o_res);
}  

// image_position : image -> int
CAMLprim value aa_image_position( value o_image )
{
  CAMLparam1(o_image);
  aeron_image_t* image = image_val(o_image);
  int64_t res = aeron_image_position(image);
  CAMLreturn(Val_int(res));
}

// subscription_is_connected : subscription -> bool
CAMLprim value aa_subscription_is_connected( value o_subscription )
{
  CAMLparam1(o_subscription);
  aeron_subscription_t* subscription = subscription_val(o_subscription);
  bool result = aeron_subscription_is_connected(subscription);
  CAMLreturn(Val_bool(result));
}

// publication_is_connected : publication -> bool
CAMLprim value aa_publication_is_connected( value o_publication )
{
  CAMLparam1(o_publication);
  aeron_publication_t* publication = publication_val(o_publication);
  bool result = aeron_publication_is_connected(publication);
  CAMLreturn(Val_bool(result));
}

// exclusive_publication_is_connected : exclusive_publication -> bool
CAMLprim value aa_exclusive_publication_is_connected( value o_exclusive_publication )
{
  CAMLparam1(o_exclusive_publication);
  aeron_exclusive_publication_t* exclusive_publication =
    exclusive_publication_val(o_exclusive_publication);
  bool result = aeron_exclusive_publication_is_connected(exclusive_publication);
  CAMLreturn(Val_bool(result));
}

// image_is_closed : image : bool
CAMLprim value aa_image_is_closed( value o_image )
{
  CAMLparam1(o_image);
  aeron_image_t* image = image_val(o_image);
  bool result = aeron_image_is_closed(image);
  CAMLreturn(Val_bool(result));
}

// subscription_is_closed : subscription -> bool
CAMLprim value aa_subscription_is_closed( value o_subscription )
{
  CAMLparam1(o_subscription);
  aeron_subscription_t* subscription = subscription_val(o_subscription);
  bool result = aeron_subscription_is_closed(subscription);
  CAMLreturn(Val_bool(result));
}

// publication_is_closed : publication -> bool
CAMLprim value aa_publication_is_closed(value o_publication)
{
  CAMLparam1(o_publication);
  aeron_publication_t* publication = publication_val(o_publication);
  bool result = aeron_publication_is_closed( publication );
  CAMLreturn(Val_bool(result));
}

// exclusive_publication_is_closed : exclusive_publication -> bool
CAMLprim value aa_exclusive_publication_is_closed(value o_exclusive_publication)
{
  CAMLparam1(o_exclusive_publication);
  aeron_exclusive_publication_t* exclusive_publication =
    exclusive_publication_val(o_exclusive_publication);
  bool result = aeron_exclusive_publication_is_closed( exclusive_publication );
  CAMLreturn(Val_bool(result));
}

// subscription_image_release : subscription -> image -> bool
CAMLprim value aa_subscription_image_release(value o_subscription, value o_image)
{
  CAMLparam2(o_subscription, o_image);
  aeron_subscription_t* subscription = subscription_val(o_subscription);
  aeron_image_t* image = image_val(o_image);
  int res = aeron_subscription_image_release( subscription, image );
  if ( res == 0 ) {
    CAMLreturn(Val_true);
  }
  else if ( res == -1 ) {
    CAMLreturn(Val_false);
  }
  else {
    assert(false);
  }

}
