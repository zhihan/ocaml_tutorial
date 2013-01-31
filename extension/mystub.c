#include "myvector.h"

#include <stdio.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>


/* OCaml C wrapper */


static struct custom_operations vector_ops = {
    "myvector",
    custom_finalize_default,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default
};

#define Instance_val(v) (*((realVector **) Data_custom_val(v)))

static value alloc_vector(realVector* vec)
{
  value v = caml_alloc_custom(&vector_ops,
                              sizeof(realVector*), 0, 1);
  Instance_val(v) = vec;
  return v;
}

value new_vector_stub(value unit)
{
  CAMLparam1(unit);
  realVector* v = newVector();
  CAMLreturn(alloc_vector(v));
}

value delete_vector_stub(value vec)
{
    CAMLparam1(vec);
    realVector* v = Instance_val(vec);
    deleteVector(v);
    CAMLreturn(Val_unit);
    
}

value vector_append_stub(value vec, value d)
{
    CAMLparam2(vec, d);
    realVector* v = Instance_val(vec);
    double x = Double_val(d);
    vector_append(v, x);
    CAMLreturn(Val_unit);
}

value vector_get_stub(value vec, value i)
{
    CAMLparam2(vec, i);
    CAMLlocal1(result);

    realVector* v = Instance_val(vec);
    int x = Int_val(i);
    result = caml_alloc(Double_wosize, Double_tag);
    Store_double_val(result, vector_get(v,x));
    CAMLreturn(result);
    
}

value vector_size_stub(value vec)
{
    CAMLparam1(vec);
    realVector* v = Instance_val(vec);
    CAMLreturn(Val_int(vector_size(v)));
}
