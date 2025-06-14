#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <yyjson.h>

static struct custom_operations yyjson_doc_ops = {
  "yyjson.doc.ops",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations yyjson_val_ops = {
  "yyjson.val.ops",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations yyjson_mut_doc_ops = {
  "yyjson.mut.doc.ops",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations yyjson_mut_val_ops = {
  "yyjson.mut.val.ops",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations yyjson_alc_ops = {
  "yyjson.alc.ops",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

#define Doc_val(v) (*((yyjson_doc **) Data_custom_val(v)))
#define Val_val(v) (*((yyjson_val **) Data_custom_val(v)))
#define Mutdoc_val(v) (*((yyjson_mut_doc **) Data_custom_val(v)))
#define Mutval_val(v) (*((yyjson_mut_val **) Data_custom_val(v)))
#define Alc_val(v) ((yyjson_alc *)Data_custom_val(v))

// Pool allocator

static void *custom_caml_stat_alloc(void *ctx, size_t sz) {
    return caml_stat_alloc(sz);
}

static void *custom_caml_stat_resize(void *ctx, void* ptr, size_t old, size_t new) {
    return caml_stat_resize(ptr, new);
}

static void custom_caml_stat_free(void *ctx, void* ptr) {
    return caml_stat_free(ptr);
}

CAMLprim value ml_yyjson_alc_caml(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(alc);

  alc = caml_alloc_custom(&yyjson_alc_ops, sizeof(yyjson_alc), 0, 1);
  Alc_val(alc)->malloc = custom_caml_stat_alloc;
  Alc_val(alc)->realloc = custom_caml_stat_resize;
  Alc_val(alc)->free = custom_caml_stat_free;
  CAMLreturn(alc);
}

CAMLprim value ml_yyjson_alc_pool_init(value ba) {
  CAMLparam1(ba);
  CAMLlocal1(alc);

  alc = caml_alloc_custom(&yyjson_alc_ops, sizeof(yyjson_alc), 0, 1);
  bool success = yyjson_alc_pool_init(Alc_val(alc),
                                      Caml_ba_data_val(ba),
                                      Caml_ba_array_val(ba)->dim[0]);
    if (!success)
        caml_failwith("yyjson_alc_pool_init");

  CAMLreturn(alc);
}

CAMLprim value ml_yyjson_alc_dyn_new(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(alc);

  alc = caml_alloc_custom(&yyjson_alc_ops, sizeof(yyjson_alc), 0, 1);
  yyjson_alc *new_alc = yyjson_alc_dyn_new();
  memcpy(Alc_val(alc), new_alc, sizeof(yyjson_alc));
  CAMLreturn(alc);
}

CAMLprim value ml_yyjson_alc_dyn_free(value alc) {
  yyjson_alc_dyn_free(Alc_val(alc));
  return Val_unit;
}

CAMLprim value ml_yyjson_alc_free(value alc, value buf) {
  Alc_val(alc)->free(Alc_val(alc)->ctx, Caml_ba_data_val(buf));
  return Val_unit;
}

CAMLprim value ml_yyjson_version (value unit) {
    return(Val_int(yyjson_version()));
}

CAMLprim value ml_yyjson_read_opts(value buf, value pos, value len, value flg, value alc) {
    CAMLparam5(buf, pos, len, flg, alc);
    CAMLlocal1(x);
    yyjson_read_err err;
    x = caml_alloc_custom(&yyjson_doc_ops, sizeof(yyjson_doc **), 0, 1);
    yyjson_alc *calc = NULL;
    char *data = NULL;
    switch (Tag_val(buf)) {
    case String_tag:
        data = (char *)String_val(buf) + Long_val(pos);
        break;
    default:
        data = ((char *)Caml_ba_data_val(buf)) + Long_val(pos);
    }
    if (Is_some(alc))
      calc = Alc_val(Field(alc, 0));
    Doc_val(x) = yyjson_read_opts(data, Long_val(len), Int_val(flg), calc, &err);
    if (!Doc_val(x))
        caml_failwith(err.msg);
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_read_file(value file, value flg, value alc) {
    CAMLparam3(file, flg, alc);
    CAMLlocal1(x);
    yyjson_read_err err;
    yyjson_alc *calc = NULL;
    if (Is_some (alc)) calc = Alc_val(Field(alc, 0));
    yyjson_doc *doc = yyjson_read_file(String_val(file), Int_val(flg), calc, &err);
    if (!doc)
        caml_failwith(err.msg);

    x = caml_alloc_custom(&yyjson_doc_ops,
                          sizeof (yyjson_doc **),
                          0, 1);
    Doc_val(x) = doc;
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_doc_get_root(value doc) {
    CAMLparam1(doc);
    CAMLlocal1(x);
    x = caml_alloc_custom(&yyjson_val_ops,
                          sizeof (yyjson_val **),
                          0, 1);
    Val_val(x) = yyjson_doc_get_root(Doc_val(doc));
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_doc_get_read_size(value doc) {
    return Val_long(yyjson_doc_get_read_size(Doc_val(doc)));
}
CAMLprim value ml_yyjson_doc_get_val_count(value doc) {
    return Val_long(yyjson_doc_get_val_count(Doc_val(doc)));
}
CAMLprim value ml_yyjson_doc_free(value doc) {
    yyjson_doc_free(Doc_val(doc));
    return Val_unit;
}

// Write API

CAMLprim value ml_yyjson_write_opts(value doc, value flg, value alc) {
    CAMLparam3(doc, flg, alc);
    CAMLlocal1(x);
    yyjson_write_err err;
    size_t len;
    yyjson_alc *calc = NULL;
    if (Is_some (alc)) calc = Alc_val(Field(alc, 0));
    char* res = yyjson_write_opts(Doc_val(doc),
                                  Int_val(flg),
                                  calc,
                                  &len,
                                  &err);
    if (!res)
        caml_failwith(err.msg);
    x = caml_ba_alloc_dims(CAML_BA_CHAR|CAML_BA_C_LAYOUT, 1, res, len);
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_write_file(value path, value doc, value flg, value alc) {
    CAMLparam4(path, doc, flg, alc);
    CAMLlocal1(x);
    yyjson_write_err err;
    yyjson_alc *calc = NULL;
    if (Is_some (alc)) calc = Alc_val(Field(alc, 0));
    bool res = yyjson_write_file(String_val(path),
                                 Doc_val(doc),
                                 Int_val(flg),
                                 calc,
                                 &err);
    if (!res)
        caml_failwith(err.msg);
    CAMLreturn(Val_unit);
}

// Mutable Write API

CAMLprim value ml_yyjson_mut_write_opts(value doc, value flg, value alc) {
    CAMLparam3(doc, flg, alc);
    CAMLlocal1(x);
    yyjson_write_err err;
    size_t len;
    yyjson_alc *calc = NULL;
    if (Is_some (alc)) calc = Alc_val(Field(alc, 0));
    char* res = yyjson_mut_write_opts(Mutdoc_val(doc),
                                      Int_val(flg),
                                      calc,
                                      &len,
                                      &err);
    if (!res)
        caml_failwith(err.msg);
    x = caml_ba_alloc_dims(CAML_BA_CHAR|CAML_BA_C_LAYOUT, 1, res, len);
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_mut_write_file(value path, value doc, value flg, value alc) {
    CAMLparam4(path, doc, flg, alc);
    CAMLlocal1(x);
    yyjson_write_err err;
    yyjson_alc *calc = NULL;
    if (Is_some (alc)) calc = Alc_val(Field(alc, 0));
    bool res = yyjson_mut_write_file(String_val(path),
                                     Mutdoc_val(doc),
                                     Int_val(flg),
                                     calc,
                                     &err);
    if (!res)
        caml_failwith(err.msg);
    CAMLreturn(Val_unit);
}

// Mutable JSON doc API

CAMLprim value ml_yyjson_mut_doc_new(value alc) {
    CAMLparam1(alc);
    CAMLlocal1(x);
    yyjson_alc *calc = NULL;
    if (Is_some (alc)) calc = Alc_val(Field(alc, 0));
    yyjson_mut_doc *doc = yyjson_mut_doc_new(calc);
    if (!doc)
        caml_failwith("yyjson_mut_doc_new");

    x = caml_alloc_custom(&yyjson_mut_doc_ops,
                          sizeof (yyjson_mut_doc **),
                          0, 1);
    Mutdoc_val(x) = doc;
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_mut_doc_free(value doc) {
    yyjson_mut_doc_free(Mutdoc_val(doc));
    return Val_unit;
}

CAMLprim value ml_yyjson_mut_doc_set_root(value doc, value root) {
    yyjson_mut_doc_set_root(Mutdoc_val(doc), Mutval_val(root));
    return Val_unit;
}

// Mutable array API

CAMLprim value ml_yyjson_mut_arr(value doc) {
    CAMLparam1(doc);
    CAMLlocal1(x);
    yyjson_mut_val *v = yyjson_mut_arr(Mutdoc_val(doc));
    if (!v)
        caml_failwith("yyjson_mut_arr");

    x = caml_alloc_custom(&yyjson_mut_val_ops,
                          sizeof (yyjson_mut_val **),
                          0, 1);
    Mutval_val(x) = v;
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_mut_arr_add_val(value arr, value v) {
    return Val_bool(yyjson_mut_arr_add_val(Mutval_val(arr), Mutval_val(v)));
}

// Mutable object API

CAMLprim value ml_yyjson_mut_obj(value doc) {
    CAMLparam1(doc);
    CAMLlocal1(x);
    yyjson_mut_val *v = yyjson_mut_obj(Mutdoc_val(doc));
    if (!v)
        caml_failwith("yyjson_mut_obj");

    x = caml_alloc_custom(&yyjson_mut_val_ops,
                          sizeof (yyjson_mut_val **),
                          0, 1);
    Mutval_val(x) = v;
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_mut_obj_add(value obj, value k, value v) {
    return Val_bool(yyjson_mut_obj_add(Mutval_val(obj), Mutval_val(k), Mutval_val(v)));
}

// Mutable value API

CAMLprim value ml_yyjson_mut_null(value doc) {
    CAMLparam1(doc);
    CAMLlocal1(x);
    x = caml_alloc_custom(&yyjson_mut_val_ops,
                          sizeof (yyjson_mut_val **),
                          0, 1);
    Mutval_val(x) = yyjson_mut_null(Mutdoc_val(doc));
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_mut_bool(value doc, value b) {
    CAMLparam2(doc, b);
    CAMLlocal1(x);
    x = caml_alloc_custom(&yyjson_mut_val_ops,
                          sizeof (yyjson_mut_val **),
                          0, 1);
    Mutval_val(x) = yyjson_mut_bool(Mutdoc_val(doc), Bool_val(b));
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_mut_uint(value doc, value b) {
    CAMLparam2(doc, b);
    CAMLlocal1(x);
    x = caml_alloc_custom(&yyjson_mut_val_ops,
                          sizeof (yyjson_mut_val **),
                          0, 1);
    Mutval_val(x) = yyjson_mut_uint(Mutdoc_val(doc), Long_val(b));
    CAMLreturn(x);
}
CAMLprim value ml_yyjson_mut_sint(value doc, value b) {
    CAMLparam2(doc, b);
    CAMLlocal1(x);
    x = caml_alloc_custom(&yyjson_mut_val_ops,
                          sizeof (yyjson_mut_val **),
                          0, 1);
    Mutval_val(x) = yyjson_mut_sint(Mutdoc_val(doc), Long_val(b));
    CAMLreturn(x);
}
CAMLprim value ml_yyjson_mut_real(value doc, value b) {
    CAMLparam2(doc, b);
    CAMLlocal1(x);
    x = caml_alloc_custom(&yyjson_mut_val_ops,
                          sizeof (yyjson_mut_val **),
                          0, 1);
    Mutval_val(x) = yyjson_mut_real(Mutdoc_val(doc), Double_val(b));
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_mut_strcpy(value doc, value b) {
    CAMLparam2(doc, b);
    CAMLlocal1(x);
    x = caml_alloc_custom(&yyjson_mut_val_ops,
                          sizeof (yyjson_mut_val **),
                          0, 1);
    Mutval_val(x) = yyjson_mut_strcpy(Mutdoc_val(doc), String_val(b));
    CAMLreturn(x);
}

// Array iteration API

CAMLprim value ml_yyjson_array_iter(value v) {
    CAMLparam1(v);
    CAMLlocal1(mlarr);
    yyjson_arr_iter iter = yyjson_arr_iter_with(Val_val(v));
    size_t sz = yyjson_arr_size(Val_val(v));
    mlarr = caml_alloc_tuple(sz);
    CAMLlocalN(elt, sz);
    for (int i = 0; i < sz; i++) {
      elt[i] = caml_alloc_custom(&yyjson_val_ops, sizeof(yyjson_val **), 0, 1);
      Val_val(elt[i]) = yyjson_arr_iter_next(&iter);
      Store_field(mlarr, i, elt[i]);
    }
    CAMLreturn(mlarr);
}

// Mutable Array iteration API

CAMLprim value ml_yyjson_mut_array_iter(value v) {
    CAMLparam1(v);
    CAMLlocal1(mlarr);
    yyjson_mut_arr_iter iter = yyjson_mut_arr_iter_with(Mutval_val(v));
    size_t sz = yyjson_mut_arr_size(Mutval_val(v));
    mlarr = caml_alloc_tuple(sz);
    CAMLlocalN(elt, sz);
    for (int i = 0; i < sz; i++) {
      elt[i] = caml_alloc_custom(&yyjson_val_ops, sizeof(yyjson_val **), 0, 1);
      Mutval_val(elt[i]) = yyjson_mut_arr_iter_next(&iter);
      Store_field(mlarr, i, elt[i]);
    }
    CAMLreturn(mlarr);
}

// Object iteration API

CAMLprim value ml_yyjson_obj_iter(value v) {
    CAMLparam1(v);
    CAMLlocal1(mlobj);
    yyjson_obj_iter iter = yyjson_obj_iter_with(Val_val(v));
    size_t sz = yyjson_obj_size(Val_val(v));
    mlobj = caml_alloc_tuple(sz);
    CAMLlocalN(mlk, sz);
    CAMLlocalN(mlv, sz);
    CAMLlocalN(tup, sz);
    for (int i = 0; i < sz; i++) {
        yyjson_val *key = yyjson_obj_iter_next(&iter);
        mlk[i] = caml_copy_string(yyjson_get_str(key));
        mlv[i] = caml_alloc_custom(&yyjson_val_ops, sizeof(yyjson_val **), 0, 1);
        Val_val(mlv[i]) = yyjson_obj_iter_get_val(key);
        tup[i] = caml_alloc_tuple(2);
        Store_field(tup[i], 0, mlk[i]);
        Store_field(tup[i], 1, mlv[i]);
        Store_field(mlobj, i, tup[i]);
    }
    CAMLreturn(mlobj);
}

// Mutable Object iteration API

CAMLprim value ml_yyjson_mut_obj_iter(value v) {
    CAMLparam1(v);
    CAMLlocal1(mlobj);
    yyjson_mut_obj_iter iter = yyjson_mut_obj_iter_with(Mutval_val(v));
    size_t sz = yyjson_mut_obj_size(Mutval_val(v));
    mlobj = caml_alloc_tuple(sz);
    CAMLlocalN(mlk, sz);
    CAMLlocalN(mlv, sz);
    CAMLlocalN(tup, sz);
    for (int i = 0; i < sz; i++) {
        yyjson_mut_val *key = yyjson_mut_obj_iter_next(&iter);
        mlk[i] = caml_copy_string(yyjson_mut_get_str(key));
        mlv[i] = caml_alloc_custom(&yyjson_val_ops, sizeof(yyjson_val **), 0, 1);
        Mutval_val(mlv[i]) = yyjson_mut_obj_iter_get_val(key);
        tup[i] = caml_alloc_tuple(2);
        Store_field(tup[i], 0, mlk[i]);
        Store_field(tup[i], 1, mlv[i]);
        Store_field(mlobj, i, tup[i]);
    }
    CAMLreturn(mlobj);
}

// Value content API (immutable)

CAMLprim value ml_yyjson_get_type(value v) {
    return Val_int(yyjson_get_type(Val_val(v)));
}
CAMLprim value ml_yyjson_get_subtype(value v) {
    return Val_int(yyjson_get_subtype(Val_val(v)) >> 3);
}

CAMLprim value ml_yyjson_get_bool(value v) {
    return Val_bool(yyjson_get_bool(Val_val(v)));
}

CAMLprim value ml_yyjson_get_int(value v) {
    return Val_int(yyjson_get_int(Val_val(v)));
}

CAMLprim value ml_yyjson_get_sint(value v) {
    CAMLparam1(v);
    CAMLlocal1(x);
    x = caml_copy_int64(yyjson_get_sint(Val_val(v)));
    CAMLreturn(x);
}
CAMLprim value ml_yyjson_get_real(value v) {
    CAMLparam1(v);
    CAMLlocal1(x);
    x = caml_copy_double(yyjson_get_real(Val_val(v)));
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_get_str(value v) {
    CAMLparam1(v);
    CAMLlocal1(x);
    x = caml_copy_string(yyjson_get_str(Val_val(v)));
    CAMLreturn(x);
}

// Mutable value content API

CAMLprim value ml_yyjson_mut_get_type(value v) {
    return Val_int(yyjson_mut_get_type(Mutval_val(v)));
}
CAMLprim value ml_yyjson_mut_get_subtype(value v) {
    return Val_int(yyjson_mut_get_subtype(Mutval_val(v)) >> 3);
}

CAMLprim value ml_yyjson_mut_get_bool(value v) {
    return Val_bool(yyjson_mut_get_bool(Mutval_val(v)));
}

CAMLprim value ml_yyjson_mut_get_int(value v) {
    return Val_int(yyjson_mut_get_int(Mutval_val(v)));
}

CAMLprim value ml_yyjson_mut_get_sint(value v) {
    CAMLparam1(v);
    CAMLlocal1(x);
    x = caml_copy_int64(yyjson_mut_get_sint(Mutval_val(v)));
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_mut_get_real(value v) {
    CAMLparam1(v);
    CAMLlocal1(x);
    x = caml_copy_double(yyjson_mut_get_real(Mutval_val(v)));
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_mut_get_str(value v) {
    CAMLparam1(v);
    CAMLlocal1(x);
    x = caml_copy_string(yyjson_mut_get_str(Mutval_val(v)));
    CAMLreturn(x);
}
