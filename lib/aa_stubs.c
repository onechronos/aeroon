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

#define context_val(v) (*((aeron_context_t **) Data_custom_val(v)))
#define client_val(v) (*((aeron_t **) Data_custom_val(v)))

void aa_context_finalize(value x0)
{
  CAMLparam1(x0);
  aeron_context_t* y0 = context_val(x0);
  int y1 = aeron_context_close(y0);
  if ( y1 == 0 ) {
    return;
  }
  else if ( y1 == -1 ) {
    caml_failwith("aa.context_close");
  }
  else {
    assert(false);
  }
}


void aa_client_finalize(value x0)
{
  CAMLparam1(x0);
  aeron_t* y0 = client_val(x0);
  int y1 = aeron_close(y0);
  if ( y1 == 0 ) {
    return;
  }
  else if ( y1 == -1 ) {
    caml_failwith("aa.client_close");
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

CAMLprim value aa_client_init(value x0)
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
    caml_failwith("aa.client_init");
  }
  else {
    assert(false);
  }
}


CAMLprim value aa_client_start(value x0)
{
  CAMLparam1(x0);
  CAMLlocal1(res);

  aeron_t *client = client_val(x0);
  int err = aeron_start(client);

  if ( err == 0 ) {
    res = Val_unit;
    CAMLreturn(res);
  }
  else if ( err == -1 ) {
    caml_failwith("aa.client_start");
  }
  else {
    assert(false);
  }
}
