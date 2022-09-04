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

#define context_val(v)                         (*((aeron_context_t                         **) Data_custom_val(v)))
#define client_val(v)                          (*((aeron_t                                 **) Data_custom_val(v)))
#define async_add_publication_val(v)           (*((aeron_async_add_publication_t           **) Data_custom_val(v)))
#define async_add_exclusive_publication_val(v) (*((aeron_async_add_exclusive_publication_t **) Data_custom_val(v)))
#define publication_val(v)                     (*((aeron_publication_t                     **) Data_custom_val(v)))
#define exclusive_publication_val(v)           (*((aeron_exclusive_publication_t           **) Data_custom_val(v)))
#define async_add_subscription_val(v)          (*((aeron_async_add_subscription_t          **) Data_custom_val(v)))
#define subscription_val(v)                    (*((aeron_subscription_t                    **) Data_custom_val(v)))
#define image_val(v)                           (*((aeron_image_t                           **) Data_custom_val(v)))

CAMLprim value aa_version_major(value x0)
{
  CAMLparam1(x0);
  CAMLlocal1(x1);
  int y0 = aeron_version_major();
  x1 = Val_int(y0);
  CAMLreturn(x1);
}

CAMLprim value aa_version_minor(value x0)
{
  CAMLparam1(x0);
  CAMLlocal1(x1);
  int y0 = aeron_version_minor();
  x1 = Val_int(y0);
  CAMLreturn(x1);
}

CAMLprim value aa_version_patch(value x0)
{
  CAMLparam1(x0);
  CAMLlocal1(x1);
  int y0 = aeron_version_patch();
  x1 = Val_int(y0);
  CAMLreturn(x1);
}

CAMLprim value aa_version_full(value x0)
{
  CAMLparam1(x0);
  CAMLlocal1(x1);
  const char *y0 = aeron_version_full();
  x1 = caml_alloc_initialized_string( strlen(y0), y0 );
  CAMLreturn(x1);
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
  value o_subscription = caml_alloc_small( sizeof(aeron_subscription_t*), Abstract_tag);
  subscription_val( o_subscription ) = subscription;

  value o_image = caml_alloc_small( sizeof(aeron_image_t*), Abstract_tag);
  image_val( o_image ) = image;

  value o_function = (value)clientd;
  caml_callback2( o_function, o_subscription, o_image );
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
  void* on_available_image = NULL;
  aeron_on_available_image_t on_available_image_handler = NULL;
  if ( o_on_available_image_opt != Val_none ) {
    o_on_available_image = Some_val(o_on_available_image_opt);
    on_available_image = (void*)o_on_available_image;
    on_available_image_handler = aa_on_image;
  }

  void* on_unavailable_image = NULL;
  aeron_on_unavailable_image_t on_unavailable_image_handler = NULL;
  if ( o_on_unavailable_image_opt != Val_none ) {
    o_on_unavailable_image = Some_val(o_on_unavailable_image_opt);
    on_unavailable_image = (void*)o_on_unavailable_image;
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
