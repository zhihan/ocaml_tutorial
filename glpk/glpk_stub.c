/* This is a simple OCaml stub for calling glpk library. */

#include <stdio.h>

#include <glpk.h> /* Make sure glpk is installed */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>


/* Boiler plate for declaring custom objects */
#define Glp_prob_val(v) (*(glp_prob**) Data_custom_val(v))

static void finalize_glp_prob(value block) {
    glp_delete_prob(Glp_prob_val(block));
}

static struct custom_operations glp_prob_ops = {
    "glp_prob",
    finalize_glp_prob,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default,
    custom_compare_ext_default
};    

value create_prob_stub(value unit)
{
    CAMLparam1(unit);
    glp_prob* p = glp_create_prob();
    value result = caml_alloc_custom(&glp_prob_ops,
                                     sizeof(glp_prob*),
                                     0, 1);
    Glp_prob_val(result) = p;
    CAMLreturn(result);
}

value set_prob_name_stub(value t, value name)
{
    CAMLparam2(t, name);
    glp_prob* p = Glp_prob_val(t);
    char* n = String_val(name);
    glp_set_prob_name(p, n);
    CAMLreturn(Val_unit);
}

value get_prob_name_stub(value t)
{
    CAMLparam1(t);
    glp_prob* p = Glp_prob_val(t);
    const char* r = glp_get_prob_name(p);
    CAMLreturn(caml_copy_string(r));
}

value set_prob_obj_max_stub(value t)
{
    CAMLparam1(t);
    glp_prob* p = Glp_prob_val(t);
    glp_set_obj_dir(p, GLP_MAX);
    CAMLreturn(Val_unit);
}

value set_prob_obj_min_stub(value t)
{
    CAMLparam1(t);
    glp_prob* p = Glp_prob_val(t);
    glp_set_obj_dir(p, GLP_MIN);
    CAMLreturn(Val_unit);
}

value get_prob_obj_dir_stub(value t)
{
    CAMLparam1(t);
    glp_prob* p = Glp_prob_val(t);
    int i = glp_get_obj_dir(p);
    int result;
    switch (i) 
    {
      case GLP_MIN:
        result = 0;
        break;
      case GLP_MAX:
        result = 1;
        break;
      default:
        result = 0; /* xxx */
    }
    CAMLreturn(Val_int(result));
}

value add_rows_stub(value t, value i) 
{
    CAMLparam2(t, i);
    glp_prob* p = Glp_prob_val(t);
    int n = Int_val(i);
    glp_add_rows(p, n);
    CAMLreturn(Val_unit);
}

value set_row_name_stub(value t, value idx, value name) 
{
    CAMLparam3(t, idx, name);
    glp_prob* p = Glp_prob_val(t);
    int i = Int_val(idx);
    const char* n = String_val(name);
    glp_set_row_name(p, i, n);
    CAMLreturn(Val_unit);
}

value set_row_bnds_free_stub(value t, value idx)
{
    CAMLparam2(t, idx);
    glp_prob* p = Glp_prob_val(t);
    int i = Int_val(idx);
    glp_set_row_bnds(p, i, GLP_FR, 0.0, 0.0);
    CAMLreturn(Val_unit);
}

value set_row_bnds_lower_stub(value t, value idx, value lb)
{
    CAMLparam3(t, idx, lb);
    glp_prob* p = Glp_prob_val(t);
    int i = Int_val(idx);
    double l = Double_val(lb);
    glp_set_row_bnds(p, i, GLP_LO, l, 0.0);
    CAMLreturn(Val_unit);
}
value set_row_bnds_upper_stub(value t, value idx, value ub)
{
    CAMLparam3(t, idx, ub);
    glp_prob* p = Glp_prob_val(t);
    int i = Int_val(idx);
    double l = Double_val(ub);
    glp_set_row_bnds(p, i, GLP_UP, 0.0, l);
    CAMLreturn(Val_unit);
}

value set_row_bnds_fixed_stub(value t, value idx, value ub)
{
    CAMLparam3(t, idx, ub);
    glp_prob* p = Glp_prob_val(t);
    int i = Int_val(idx);
    double l = Double_val(ub);
    glp_set_row_bnds(p, i, GLP_FX, l, l);
    CAMLreturn(Val_unit);
}

value set_row_bnds_double_stub(value t, value idx, value lb, value ub)
{
    CAMLparam4(t, idx, lb, ub);
    glp_prob* p = Glp_prob_val(t);
    int i = Int_val(idx);
    double l = Double_val(lb);
    double u = Double_val(ub);
    glp_set_row_bnds(p, i, GLP_DB, l, u);
    CAMLreturn(Val_unit);
}


value add_cols_stub(value t, value nVal) 
{
    CAMLparam2(t, nVal);
    glp_prob* p = Glp_prob_val(t);
    int n = Int_val(nVal);
    glp_add_cols(p, n);
    CAMLreturn(Val_unit);
}

value set_col_name_stub(value t, value idx, value name) 
{
    CAMLparam3(t, idx, name);
    glp_prob* p = Glp_prob_val(t);
    int i = Int_val(idx);
    const char* n = String_val(name);
    glp_set_col_name(p, i, n);
    CAMLreturn(Val_unit);
}

value load_matrix_stub(value t, value num, value I, value J, value v)
{
    CAMLparam5(t, num, I, J, v);
    int* ia;
    int* ja;
    double* ar;

    glp_prob* p = Glp_prob_val(t);
    int n = Int_val(num);

    /* Load the arrays from Caml to C arrays */
    ia = (int*) malloc((n+1) * sizeof(int));
    ja = (int*) malloc((n+1) * sizeof(int));
    ar = (double*)malloc((n+1) * sizeof(double));

    int i;
    /* Note glpk uses 1-based index */
    for (i=0; i<n; i++) {
        ia[i+1] = Int_val(Field(I, i));
        ja[i+1] = Int_val(Field(J, i));
        ar[i+1] = Double_field(v, i);
    }
    glp_load_matrix(p, n, ia, ja, ar);
    
    free(ia);
    free(ja);
    free(ar);
    CAMLreturn(Val_unit);
}