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

/* C standard library */
#include <string.h>
#include <assert.h>

/* Aeron */
#include <aeronc.h>
#include <aeron_agent.h>

#define context_val(v)                         (*((aeron_context_t                         **) Data_custom_val(v)))
#define client_val(v)                          (*((aeron_t                                 **) Data_custom_val(v)))
#define async_add_publication_val(v)           (*((aeron_async_add_publication_t           **) Data_custom_val(v)))
#define async_add_exclusive_publication_val(v) (*((aeron_async_add_exclusive_publication_t **) Data_custom_val(v)))
#define publication_val(v)                     (*((aeron_publication_t                     **) Data_custom_val(v)))
#define exclusive_publication_val(v)           (*((aeron_exclusive_publication_t           **) Data_custom_val(v)))
#define async_add_subscription_val(v)          (*((aeron_async_add_subscription_t          **) Data_custom_val(v)))
#define subscription_val(v)                    (*((aeron_subscription_t                    **) Data_custom_val(v)))
#define image_val(v)                           (*((aeron_image_t                           **) Data_custom_val(v)))
#define fragment_assembler_val(v)              (*((aeron_fragment_assembler_t              **) Data_custom_val(v)))
#define image_fragment_assembler_val(v)        (*((aeron_image_fragment_assembler_t        **) Data_custom_val(v)))
#define buffer_claim_val(v)                    (*((aeron_buffer_claim_t                    **) Data_custom_val(v)))

CAMLprim value aa_version_major(value _unit)
{
  CAMLparam1(_unit);
  CAMLlocal1(o_res);
  int version_major = aeron_version_major();
  o_res = Val_int(version_major);
  CAMLreturn(o_res);
}

CAMLprim value aa_version_minor(value _unit)
{
  CAMLparam1(_unit);
  CAMLlocal1(o_res);
  int version_minor = aeron_version_minor();
  o_res = Val_int(version_minor);
  CAMLreturn(o_res);
}

CAMLprim value aa_version_patch(value _unit)
{
  CAMLparam1(_unit);
  CAMLlocal1(o_res);
  int version_patch = aeron_version_patch();
  o_res = Val_int(version_patch);
  CAMLreturn(o_res);
}

CAMLprim value aa_version_full(value _unit)
{
  CAMLparam1(_unit);
  CAMLlocal1(o_res);
  const char *version_full = aeron_version_full();
  o_res = caml_copy_string( version_full );
  CAMLreturn(o_res);
}

CAMLprim value aa_errmsg(value _unit)
{
  CAMLparam1(_unit);
  CAMLlocal1(o_res);
  o_res = caml_copy_string( aeron_errmsg() );
  CAMLreturn(o_res);
}

CAMLprim value aa_errcode(value _unit)
{
  CAMLparam1(_unit);
  CAMLreturn(Val_int( aeron_errcode() ) );
}

CAMLprim value aa_nano_clock(value _unit)
{
  CAMLparam1(_unit);
  CAMLreturn(Val_int( aeron_nano_clock() ) );
}

CAMLprim value aa_epoch_clock(value _unit)
{
  CAMLparam1(_unit);
  CAMLreturn(Val_int( aeron_epoch_clock() ) );
}

void aa_context_close(value o_context)
{
  CAMLparam1(o_context);
  aeron_context_t* context = context_val(o_context);
  int err = aeron_context_close(context);
  if ( err == 0 ) {
    return;
  }
  else if ( err == -1 ) {
    caml_failwith("aa.context_close");
  }
  else {
    assert(false);
  }
}

void aa_close(value o_client)
{
  CAMLparam1(o_client);
  aeron_t* client = client_val(o_client);
  int err = aeron_close(client);
  if ( err == 0 ) {
    return;
  }
  else if ( err == -1 ) {
    caml_failwith("aa.close");
  }
  else {
    assert(false);
  }
}

CAMLprim value aa_context_init(value x0)
{
  CAMLparam1(x0);
  CAMLlocal1(res);

  aeron_context_t* y0 = NULL;
  int y1 = aeron_context_init(&y0);

  if ( y1 == 0 ) {
    res = caml_alloc_small( sizeof(aeron_context_t*), Abstract_tag);
    assert(y0 != NULL);
    context_val(res) = y0;
    CAMLreturn(res);
  }
  else if ( y1 < 0 ) {
    caml_failwith("aa.context_init");
  }
  else {
    assert(false);
  }
}

CAMLprim value aa_init(value x0)
{
  CAMLparam1(x0);
  CAMLlocal1(res);

  aeron_context_t* ctx = context_val(x0);
  aeron_t *client = NULL;
  int err = aeron_init(&client, ctx);

  if ( err == 0 ) {
    res = caml_alloc_small( sizeof(aeron_t*), Abstract_tag);
    client_val(res) = client;
    CAMLreturn(res);
  }
  else if ( err < 0 ) {
    caml_failwith("aa.init");
  }
  else {
    assert(false);
  }
}

CAMLprim value aa_start(value o_client)
{
  CAMLparam1(o_client);

  aeron_t* client = client_val(o_client);
  int err = aeron_start(client);

  if ( err == 0 ) {
    CAMLreturn(Val_unit);
  }
  else if ( err < 0 ) {
    caml_failwith("aa.start");
  }
  else {
    assert(false);
  }
}

CAMLprim value aa_main_do_work(value o_client)
{
  CAMLparam1(o_client);
  aeron_t* client = client_val( o_client );
  int err = aeron_main_do_work( client );
  CAMLreturn(Val_int(err));
}

CAMLprim value aa_main_idle_strategy(value o_client, value o_work_count)
{
  CAMLparam2(o_client, o_work_count);

  aeron_t* client = client_val( o_client );
  int work_count = Int_val( o_work_count );

  aeron_main_idle_strategy( client, work_count );
  CAMLreturn(Val_unit);
}

CAMLprim value aa_is_closed(value o_client)
{
  CAMLparam1(o_client);

  aeron_t* client = client_val( o_client );
  bool result = aeron_is_closed( client );
  CAMLreturn(Val_bool(result));
}

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
    CAMLreturn(o_async);
  }
  else if ( err == -1 ) {
    caml_failwith("aa.async_add_publication");
  }
  else {
    assert(false);
  }
}

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

CAMLprim value aa_async_add_exclusive_publication(value o_client, value o_uri, value o_stream_id)
{
  CAMLparam3(o_client, o_uri, o_stream_id);
  CAMLlocal1(res);

  aeron_t* client = client_val( o_client );
  const char* uri = String_val(o_uri);
  int32_t stream_id = Int_val(o_stream_id);
  aeron_async_add_exclusive_publication_t* async = NULL;
  int err = aeron_async_add_exclusive_publication( &async, client, uri, stream_id );
  if ( err == 0 ) {
    res = caml_alloc_small( sizeof(aeron_t*), Abstract_tag);
    async_add_exclusive_publication_val(res) = async;
    CAMLreturn(res);
  }
  else if ( err == -1 ) {
    caml_failwith("aa.async_add_exclusive_publication");
  }
  else {
    assert(false);
  }
}

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

// callback for either on_available_image_handler or on_unavailable_image_handler.
// We use an OCaml closure as the Aeron client data (clientd)
void aa_on_image(void *clientd, aeron_subscription_t *subscription, aeron_image_t *image)
{
  CAMLparam0 ();
  CAMLlocal3( o_subscription, o_image, o_function );

  o_subscription = caml_alloc_small( sizeof(aeron_subscription_t*), Abstract_tag);
  subscription_val( o_subscription ) = subscription;

  o_image = caml_alloc_small( sizeof(aeron_image_t*), Abstract_tag);
  image_val( o_image ) = image;

  o_function = (value)clientd;
  caml_callback2( o_function, o_subscription, o_image );
  CAMLreturn0;
}

CAMLprim value aa_async_add_subscription(value o_client,
					 value o_uri,
					 value o_stream_id,
					 value o_on_available_image_opt,
					 value o_on_unavailable_image_opt)
{
  CAMLparam5(o_client, o_uri, o_stream_id, o_on_available_image_opt, o_on_unavailable_image_opt);
  CAMLlocal4(o_on_available_image, o_on_unavailable_image, o_async, o_res);

  aeron_t* client = client_val(o_client);
  const char* uri = String_val(o_uri);
  int32_t stream_id = Int_val(o_stream_id);

  // use the OCaml closure as the Aeron clientd (client data)
  value* on_available_image = NULL; 
  aeron_on_available_image_t on_available_image_handler = NULL;
  if ( o_on_available_image_opt != Val_none ) {
    o_on_available_image = Some_val(o_on_available_image_opt);
    on_available_image = (value*) malloc(sizeof(value));
    assert(on_available_image);
    *on_available_image = o_on_available_image;
    caml_register_global_root(on_available_image);
    on_available_image_handler = aa_on_image;
  }

  void* on_unavailable_image = NULL;
  aeron_on_unavailable_image_t on_unavailable_image_handler = NULL;
  if ( o_on_unavailable_image_opt != Val_none ) {
    o_on_unavailable_image = Some_val(o_on_unavailable_image_opt);
    on_unavailable_image = (void*)o_on_unavailable_image;
    caml_register_global_root(&o_on_unavailable_image);
    on_unavailable_image_handler = aa_on_image;
  }

  aeron_async_add_subscription_t* async = NULL;
  int err = aeron_async_add_subscription( &async,
					  client,
					  uri,
					  stream_id,
					  on_available_image_handler,
					  on_available_image,
					  on_unavailable_image_handler,
					  on_unavailable_image );
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


// returns publication_error
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

// returns Ok of int | Error of publication_error
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

// here, we do not expose aeron_reserved_value_supplier (and its data)
CAMLprim value aa_publication_offer(value o_publication, value o_buffer)
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

// identical to aa_publication_offer, except that we use
// aeron_exclusive_publcation_offer instead of aeron_publication_offer
// essentially: 's/pulication/exclusive_publication/g'
CAMLprim value aa_exclusive_publication_offer(value o_exclusive_publication, value o_buffer)
{
  CAMLparam2(o_exclusive_publication, o_buffer);
  aeron_exclusive_publication_t* exclusive_publication = exclusive_publication_val(o_exclusive_publication);
  const char* buffer = String_val(o_buffer);
  size_t length = caml_string_length(o_buffer);

  int64_t res = aeron_exclusive_publication_offer( exclusive_publication, buffer, length, NULL, NULL );
  CAMLreturn(publication_result(res));
}

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
  bool res;
  if ( err == 0 ) {
    res = true;
  }
  else if ( err == -1 ) {
    res = false;
  }
  else {
    assert(false);
  }
  CAMLreturn(Val_bool(res));
}

void aa_notification(void* clientd)
{
  CAMLparam0 ();
  CAMLlocal1( o_function );
  o_function = (value)clientd;
  caml_callback( o_function, Val_unit );
  CAMLreturn0;
}

CAMLprim value aa_publication_close(value o_publication, value o_on_close_complete_opt)
{
  CAMLparam2(o_publication, o_on_close_complete_opt);
  CAMLlocal2(o_on_close_complete, o_res);
  aeron_publication_t* publication = publication_val(o_publication);
  void* on_close_complete = NULL;
  if ( o_on_close_complete_opt != Val_none ) {
    o_on_close_complete = Some_val(o_on_close_complete_opt);
    on_close_complete = malloc(sizeof(value));
    assert(on_close_complete);
    on_close_complete = (void*)o_on_close_complete;
    caml_register_global_root(on_close_complete);
  }
  int res = aeron_publication_close( publication, aa_notification, on_close_complete );
  if (res == 0) {
    o_res = Val_bool(true);
  }
  else if ( res == -1 ) {
    o_res = Val_bool(false);
  }
  else {
    assert(false);
  }
  CAMLreturn(o_res);
}

void aa_fragment_handler(void* clientd,
			 const uint8_t* buffer,
			 size_t length,
			 aeron_header_t* _header) // ignore header for now
{
  CAMLparam0();
  CAMLlocal3(o_buffer, o_function, o_res);
  o_buffer = caml_alloc_initialized_string(length, buffer);
  o_function = *((value*) clientd);
  o_res = caml_callback( o_function, o_buffer );
  CAMLreturn0;
}

CAMLprim value aa_fragment_assembler_create( value o_delegate )
{
  CAMLparam1( o_delegate );
  CAMLlocal2( o_fragment_assembler, o_res );
  aeron_fragment_assembler_t* fragment_assembler = NULL;
  value* cb = malloc(sizeof(value));
  printf("o_delegate: %lx\n", o_delegate);
  *cb = o_delegate;
  printf("*cb: %lx\n", *cb);
  caml_register_global_root(cb);
  void* clientd = (void*)cb;
  int res = aeron_fragment_assembler_create( &fragment_assembler,
					     aa_fragment_handler,
					     clientd );
  if ( res == 0 ) {
    // Some fragment_assembler
    o_fragment_assembler = caml_alloc_small( sizeof(aeron_fragment_assembler_t*), Abstract_tag);
    fragment_assembler_val(o_fragment_assembler) = fragment_assembler;
    o_res = caml_alloc_some( o_fragment_assembler );
  }
  else if ( res == -1 ) {
    // None
    o_res = Val_none;
  }
  else {
    assert(false);
  }
  CAMLreturn(o_res);
}

// apply 's/fragment/image_fragment/g' to function aa_fragment_assembler_create
CAMLprim value aa_image_fragment_assembler_create( value o_delegate )
{
  CAMLparam1( o_delegate );
  CAMLlocal2( o_image_fragment_assembler, o_res );
  aeron_image_fragment_assembler_t* image_fragment_assembler = NULL;
  value* cb = malloc(sizeof(value));
  printf("o_delegate: %lx\n", o_delegate);
  *cb = o_delegate;
  printf("*cb: %lx\n", *cb);
  caml_register_global_root(cb);
  void* clientd = (void*)cb;
  int res = aeron_image_fragment_assembler_create( &image_fragment_assembler,
						   aa_fragment_handler,
						   clientd );
  if ( res == 0 ) {
    // Some image_fragment_assembler
    o_image_fragment_assembler =
      caml_alloc_small( sizeof(aeron_image_fragment_assembler_t*), Abstract_tag);
    image_fragment_assembler_val(o_image_fragment_assembler) = image_fragment_assembler;
    o_res = caml_alloc_some( o_image_fragment_assembler );
  }
  else if ( res == -1 ) {
    // None
    o_res = Val_none;
  }
  else {
    assert(false);
  }
  CAMLreturn(o_res);
}

CAMLprim value aa_subscription_poll( value o_subscription,
				     // TODO: o_fragment_assembler_handler
				     value o_fragment_assembler,
				     value o_fragment_limit )
{
  CAMLparam3(o_subscription, o_fragment_assembler, o_fragment_limit );
  CAMLlocal1(o_res);
  aeron_subscription_t* subscription = subscription_val(o_subscription);
  aeron_fragment_assembler_t* fragment_assembler = fragment_assembler_val(o_fragment_assembler);
  size_t fragment_limit = Int_val(o_fragment_limit);
  int res = aeron_subscription_poll( subscription,
				     aeron_fragment_assembler_handler,
				     fragment_assembler,
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

// like subscription_poll, but 's/subscription/image/g'
CAMLprim value aa_image_poll( value o_image,
			      // TODO: o_fragment_assembler_handler
			      value o_image_fragment_assembler,
			      value o_fragment_limit )
{
  CAMLparam3(o_image, o_image_fragment_assembler, o_fragment_limit );
  CAMLlocal1(o_res);
  aeron_image_t* image = image_val(o_image);
  aeron_image_fragment_assembler_t* image_fragment_assembler =
    image_fragment_assembler_val(o_image_fragment_assembler);
  size_t fragment_limit = Int_val(o_fragment_limit);
  int res = aeron_image_poll( image,
			      aeron_image_fragment_assembler_handler,
			      image_fragment_assembler,
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

CAMLprim value aa_idle_strategy_sleeping_idle( value o_nanos, value o_work_count )
{
  CAMLparam2(o_nanos, o_work_count);
  int64_t nanos = Int_val(o_nanos);
  int work_count = Int_val(o_work_count);
  aeron_idle_strategy_sleeping_idle( (void*)(&nanos), work_count );
  CAMLreturn(Val_unit);
}

CAMLprim value aa_idle_strategy_yielding_idle( value o_nanos, value o_work_count )
{
  CAMLparam2(o_nanos, o_work_count);
  int64_t nanos = Int_val(o_nanos);
  int work_count = Int_val(o_work_count);
  aeron_idle_strategy_yielding_idle( (void*)(&nanos), work_count );
  CAMLreturn(Val_unit);
}

CAMLprim value aa_idle_strategy_busy_spinning_idle( value o_nanos, value o_work_count )
{
  CAMLparam2(o_nanos, o_work_count);
  int64_t nanos = Int_val(o_nanos);
  int work_count = Int_val(o_work_count);
  aeron_idle_strategy_busy_spinning_idle( (void*)(&nanos), work_count );
  CAMLreturn(Val_unit);
}

CAMLprim value aa_idle_strategy_noop_idle( value o_nanos, value o_work_count )
{
  CAMLparam2(o_nanos, o_work_count);
  int64_t nanos = Int_val(o_nanos);
  int work_count = Int_val(o_work_count);
  aeron_idle_strategy_noop_idle( (void*)(&nanos), work_count );
  CAMLreturn(Val_unit);
}

CAMLprim value aa_idle_strategy_backoff_idle( value o_nanos, value o_work_count )
{
  CAMLparam2(o_nanos, o_work_count);
  int64_t nanos = Int_val(o_nanos);
  int work_count = Int_val(o_work_count);
  aeron_idle_strategy_backoff_idle( (void*)(&nanos), work_count );
  CAMLreturn(Val_unit);
}

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

CAMLprim value aa_image_position( value o_image )
{
  CAMLparam1(o_image);
  aeron_image_t* image = image_val(o_image);
  int64_t res = aeron_image_position(image);
  CAMLreturn(Val_int(res));
}

CAMLprim value aa_subscription_is_connected( value o_subscription )
{
  CAMLparam1(o_subscription);
  aeron_subscription_t* subscription = subscription_val(o_subscription);
  bool result = aeron_subscription_is_connected(subscription);
  CAMLreturn(Val_bool(result));
}

CAMLprim value aa_publication_is_connected( value o_publication )
{
  CAMLparam1(o_publication);
  aeron_publication_t* publication = publication_val(o_publication);
  bool result = aeron_publication_is_connected(publication);
  CAMLreturn(Val_bool(result));
}

CAMLprim value aa_exclusive_publication_is_connected( value o_exclusive_publication )
{
  CAMLparam1(o_exclusive_publication);
  aeron_exclusive_publication_t* exclusive_publication =
    exclusive_publication_val(o_exclusive_publication);
  bool result = aeron_exclusive_publication_is_connected(exclusive_publication);
  CAMLreturn(Val_bool(result));
}

CAMLprim value aa_image_is_closed( value o_image )
{
  CAMLparam1(o_image);
  aeron_image_t* image = image_val(o_image);
  bool result = aeron_image_is_closed(image);
  CAMLreturn(Val_bool(result));
}

CAMLprim value aa_subscription_is_closed( value o_subscription )
{
  CAMLparam1(o_subscription);
  aeron_subscription_t* subscription = subscription_val(o_subscription);
  bool result = aeron_subscription_is_closed(subscription);
  CAMLreturn(Val_bool(result));
}

CAMLprim value aa_publication_is_closed(value o_publication)
{
  CAMLparam1(o_publication);
  aeron_publication_t* publication = publication_val(o_publication);
  bool res = aeron_publication_is_closed( publication );
  CAMLreturn(Val_bool(res));
}

CAMLprim value aa_exclusive_publication_is_closed(value o_exclusive_publication)
{
  CAMLparam1(o_exclusive_publication);
  aeron_exclusive_publication_t* exclusive_publication =
    exclusive_publication_val(o_exclusive_publication);
  bool res = aeron_exclusive_publication_is_closed( exclusive_publication );
  CAMLreturn(Val_bool(res));
}

