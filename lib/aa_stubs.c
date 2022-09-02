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

void aa_context_finalize(value x0)
{
  CAMLparam1(x0);
  aeron_context_t* y0 = (aeron_context_t*)Data_custom_val(x0);
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

static struct custom_operations aa_context_ops =
  {
   .identifier = "aa.context",             
   .finalize = aa_context_finalize,
   .compare = custom_compare_default,
   .hash = custom_hash_default,
   .serialize = custom_serialize_default,
   .deserialize = custom_deserialize_default,
  };

#define context_val(v) (*((aeron_context_t **) Data_custom_val(v)))

CAMLprim value aa_context_init(value x0)
{
  CAMLparam1(x0);
  CAMLlocal1(x1);

  aeron_context_t* y0 = NULL;
  int y1 = aeron_context_init(&y0);

  if ( y1 == 0 ) {
    x1 = caml_alloc_custom( &aa_context_ops, sizeof(aeron_context_t*), 0, 1 );
    context_val(x1) = y0;
    CAMLreturn(x1);
  }
  else if ( y1 == -1 ) {
    caml_failwith("aa.context_init");
  }
  else {
    assert(false);
  }
}
