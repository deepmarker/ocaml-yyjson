#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <yyjson.h>

/* static void doc_free(value doc) { */
/*     yyjson_doc_free((*(yyjson_doc **) Data_custom_val(doc))); */
/* } */

/* static void mut_doc_free(value doc) { */
/*     yyjson_mut_doc_free((*(yyjson_mut_doc **) Data_custom_val(doc))); */
/* } */

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

static struct custom_operations yyjson_mut_doc_ops = {
    "yyjson.mut.doc.ops", custom_finalize_default, custom_compare_default,
    custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

#define Doc_val(v) (*((yyjson_doc **) Data_custom_val(v)))
#define Mutdoc_val(v) (*((yyjson_mut_doc **) Data_custom_val(v)))

#define Val_val(v) ((yyjson_val *) Data_abstract_val(v))
#define Mutval_val(v) ((yyjson_mut_val *)Data_abstract_val(v))

CAMLprim value ml_yyjson_version (value unit) {
    return(Val_int(yyjson_version()));
}

CAMLprim value ml_yyjson_read_opts(value buf, value pos, value len, value flg) {
    CAMLparam4(buf, pos, len, flg);
    CAMLlocal1(x);
    yyjson_read_err err;
    x = caml_alloc_custom(&yyjson_doc_ops, sizeof(yyjson_doc *), 0, 1);
    char *data = NULL;
    switch (Tag_val(buf)) {
    case String_tag:
        data = (char *)String_val(buf) + Long_val(pos);
        break;
    default:
        data = ((char *)Caml_ba_data_val(buf)) + Long_val(pos);
    }
    Doc_val(x) = yyjson_read_opts(data, Long_val(len), Int_val(flg), NULL, &err);
    if (!Doc_val(x)) caml_failwith(err.msg);
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_read_file(value file, value flg) {
    CAMLparam2(file, flg);
    CAMLlocal1(x);
    yyjson_read_err err;
    yyjson_doc *doc = yyjson_read_file(String_val(file), Int_val(flg), NULL, &err);
    if (!doc) caml_failwith(err.msg);
    x = caml_alloc_custom(&yyjson_doc_ops,
                          sizeof (yyjson_doc *),
                          0, 1);
    Doc_val(x) = doc;
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_doc_get_root(value doc) {
    CAMLparam1(doc);
    CAMLlocal1(x);
    x = caml_alloc(2, Abstract_tag);
    yyjson_val *v = yyjson_doc_get_root(Doc_val(doc));
    memcpy(Val_val(x), v, sizeof(yyjson_val));
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
    Doc_val(doc) = NULL;
    return Val_unit;
}

// Write API

CAMLprim value ml_yyjson_write_opts(value doc, value flg) {
    CAMLparam2(doc, flg);
    CAMLlocal1(x);
    yyjson_write_err err;
    size_t len;
    char* res = yyjson_write_opts(Doc_val(doc),
                                  Int_val(flg),
                                  NULL,
                                  &len,
                                  &err);
    if (res == NULL) caml_failwith(err.msg);
    x = caml_copy_string(res);
    free(res);
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_write_file(value doc, value path, value flg) {
    CAMLparam3(path, doc, flg);
    CAMLlocal1(x);
    yyjson_write_err err;
    bool res = yyjson_write_file(String_val(path),
                                 Doc_val(doc),
                                 Int_val(flg),
                                 NULL,
                                 &err);
    if (!res) caml_failwith(err.msg);
    CAMLreturn(Val_unit);
}

// Mutable Write API

CAMLprim value ml_yyjson_mut_write_opts(value doc, value flg) {
    CAMLparam2(doc, flg);
    CAMLlocal1(x);
    yyjson_write_err err;
    size_t len;
    char* res = yyjson_mut_write_opts(Mutdoc_val(doc),
                                      Int_val(flg),
                                      NULL,
                                      &len,
                                      &err);
    if (res == NULL) caml_failwith(err.msg);
    x = caml_copy_string(res);
    free(res);
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_mut_write_file(value doc, value path, value flg) {
    CAMLparam3(path, doc, flg);
    CAMLlocal1(x);
    yyjson_write_err err;
    bool res = yyjson_mut_write_file(String_val(path),
                                     Mutdoc_val(doc),
                                     Int_val(flg),
                                     NULL,
                                     &err);
    if (!res) caml_failwith(err.msg);
    CAMLreturn(Val_unit);
}

// Mutable JSON doc API

CAMLprim value ml_yyjson_mut_doc_new(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(x);
    yyjson_mut_doc *doc = yyjson_mut_doc_new(NULL);
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
  Mutdoc_val(doc) = NULL;
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
    if (!v) caml_failwith("yyjson_mut_arr");
    x = caml_alloc(3, Abstract_tag);
    memcpy(Mutval_val(x), v, sizeof(yyjson_mut_val));
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_mut_arr_add_val(value doc, value arr, value v) {
    CAMLparam3(doc, arr, v);
    if (Mutdoc_val(doc) == NULL) {
        caml_failwith("mutdoc is NULL");
    }

    bool res = yyjson_mut_arr_add_val(Mutval_val(arr), Mutval_val(v));
    CAMLreturn(Val_bool(res));
}

// Mutable object API

CAMLprim value ml_yyjson_mut_obj(value doc) {
    CAMLparam1(doc);
    CAMLlocal1(x);
    yyjson_mut_val *v = yyjson_mut_obj(Mutdoc_val(doc));
    if (!v) caml_failwith("yyjson_mut_obj");
    x = caml_alloc(3, Abstract_tag);
    memcpy(Mutval_val(x), v, sizeof(yyjson_mut_val));
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_mut_obj_add(value doc, value obj, value k, value v) {
    CAMLparam4(doc, obj, k, v);
    if (Mutdoc_val(doc) == NULL) {
        caml_failwith("mutdoc is NULL");
    }
    bool ret = yyjson_mut_obj_add(Mutval_val(obj), Mutval_val(k), Mutval_val(v));
    CAMLreturn(Val_bool(ret));
}

// Mutable value API

CAMLprim value ml_yyjson_mut_null(value doc) {
    CAMLparam1(doc);
    CAMLlocal1(x);
    x = caml_alloc(3, Abstract_tag);
    yyjson_mut_val *v = yyjson_mut_null(Mutdoc_val(doc));
    memcpy(Mutval_val(x), v, sizeof(yyjson_mut_val));
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_mut_bool(value doc, value b) {
    CAMLparam2(doc, b);
    CAMLlocal1(x);
    x = caml_alloc(3, Abstract_tag);
    yyjson_mut_val *v = yyjson_mut_bool(Mutdoc_val(doc), Bool_val(b));
    memcpy(Mutval_val(x), v, sizeof(yyjson_mut_val));
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_mut_uint(value doc, value b) {
    CAMLparam2(doc, b);
    CAMLlocal1(x);
    x = caml_alloc(3, Abstract_tag);
    yyjson_mut_val *v = yyjson_mut_uint(Mutdoc_val(doc), Long_val(b));
    memcpy(Mutval_val(x), v, sizeof(yyjson_mut_val));
    CAMLreturn(x);
}
CAMLprim value ml_yyjson_mut_sint(value doc, value b) {
    CAMLparam2(doc, b);
    CAMLlocal1(x);
    x = caml_alloc(3, Abstract_tag);
    yyjson_mut_val *v = yyjson_mut_sint(Mutdoc_val(doc), Long_val(b));
    memcpy(Mutval_val(x), v, sizeof(yyjson_mut_val));
    CAMLreturn(x);
}
CAMLprim value ml_yyjson_mut_real(value doc, value b) {
    CAMLparam2(doc, b);
    CAMLlocal1(x);
    x = caml_alloc(3, Abstract_tag);
    yyjson_mut_val *v = yyjson_mut_real(Mutdoc_val(doc), Double_val(b));
    memcpy(Mutval_val(x), v, sizeof(yyjson_mut_val));
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_mut_strcpy(value doc, value b) {
    CAMLparam2(doc, b);
    CAMLlocal1(x);
    x = caml_alloc(3, Abstract_tag);
    yyjson_mut_val *v = yyjson_mut_strcpy(Mutdoc_val(doc), String_val(b));
    memcpy(Mutval_val(x), v, sizeof(yyjson_mut_val));
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_array_iter(value doc, value v) {
    CAMLparam2(doc, v);
    CAMLlocal2(mlarr, elt);

    if (Doc_val(doc) == NULL) {
        caml_failwith("doc is NULL");
    }

    yyjson_arr_iter iter = yyjson_arr_iter_with(Val_val(v));
    mlarr = caml_alloc_tuple(iter.max);

    for (size_t i = 0; i < iter.max; i++) {
        elt = caml_alloc(2, Abstract_tag);
        if (Doc_val(doc) == NULL) {
            caml_failwith("doc is NULL");
        }
        yyjson_val *v = yyjson_arr_iter_next(&iter);
        memcpy(Val_val(elt), v, sizeof(yyjson_val));
        Store_field(mlarr, i, elt);
    }

    CAMLreturn(mlarr);
}

CAMLprim value ml_yyjson_mut_array_iter(value doc, value v) {
    CAMLparam2(doc, v);
    CAMLlocal2(mlarr, elt);

    if (Mutdoc_val(doc) == NULL) {
        caml_failwith("mutdoc is NULL");
    }

    yyjson_mut_arr_iter iter = yyjson_mut_arr_iter_with(Mutval_val(v));
    mlarr = caml_alloc_tuple(iter.max);

    for (size_t i = 0; i < iter.max; i++) {
        elt = caml_alloc(3, Abstract_tag);
        if (Mutdoc_val(doc) == NULL) {
            caml_failwith("mutdoc is NULL");
        }
        yyjson_mut_val *v = yyjson_mut_arr_iter_next(&iter);
        memcpy(Mutval_val(elt), v, sizeof(yyjson_mut_val));
        Store_field(mlarr, i, elt);
    }

    CAMLreturn(mlarr);
}

// Object iteration API

CAMLprim value ml_yyjson_obj_iter(value doc, value v) {
    CAMLparam2(doc, v);
    CAMLlocal4(mlobj, mlk, mlv, tup);

    if (Doc_val(doc) == NULL) {
        caml_failwith("doc is NULL");
    }

    yyjson_obj_iter iter = yyjson_obj_iter_with(Val_val(v));
    mlobj = caml_alloc_tuple(iter.max);

    for (int i = 0; i < iter.max; i++) {
        yyjson_val *key = yyjson_obj_iter_next(&iter);
        const char *keystr = yyjson_get_str(key);
        mlk = caml_copy_string(keystr);
        mlv = caml_alloc(2, Abstract_tag);
        yyjson_val *v = yyjson_obj_iter_get_val(key);
        memcpy(Val_val(mlv), v, sizeof(yyjson_val));
        tup = caml_alloc_tuple(2);
        Store_field(tup, 0, mlk);
        Store_field(tup, 1, mlv);
        Store_field(mlobj, i, tup);
    }
    CAMLreturn(mlobj);
}

CAMLprim value ml_yyjson_mut_obj_iter(value doc, value v) {
    CAMLparam2(doc, v);
    CAMLlocal4(mlobj, mlk, mlv, tup);

    if (Mutdoc_val(doc) == NULL) {
        caml_failwith("mutdoc is NULL");
    }

    yyjson_mut_obj_iter iter = yyjson_mut_obj_iter_with(Mutval_val(v));
    mlobj = caml_alloc_tuple(iter.max);

    for (int i = 0; i < iter.max; i++) {
        yyjson_mut_val *key = yyjson_mut_obj_iter_next(&iter);
        const char *keystr = yyjson_mut_get_str(key);
        mlk = caml_copy_string(keystr);
        mlv = caml_alloc(3, Abstract_tag);
        yyjson_mut_val *v = yyjson_mut_obj_iter_get_val(key);
        memcpy(Mutval_val(mlv), v, sizeof(yyjson_mut_val));
        tup = caml_alloc_tuple(2);
        Store_field(tup, 0, mlk);
        Store_field(tup, 1, mlv);
        Store_field(mlobj, i, tup);
    }
    CAMLreturn(mlobj);
}

// Value content API (immutable)

CAMLprim value ml_yyjson_get_type(value doc, value v) {
    CAMLparam2(doc, v);
    if (Doc_val(doc) == NULL) {
        caml_failwith("doc is NULL");
    }
    CAMLreturn(Val_int(yyjson_get_type(Val_val(v))));
}

CAMLprim value ml_yyjson_get_subtype(value doc, value v) {
    CAMLparam2(doc, v);
    if (Doc_val(doc) == NULL) {
        caml_failwith("doc is NULL");
    }
    CAMLreturn(Val_int(yyjson_get_subtype(Val_val(v)) >> 3));
}

CAMLprim value ml_yyjson_get_bool(value doc, value v) {
    CAMLparam2(doc, v);
    if (Doc_val(doc) == NULL) {
        caml_failwith("doc is NULL");
    }
    CAMLreturn(Val_bool(yyjson_get_bool(Val_val(v))));
}

CAMLprim value ml_yyjson_get_sint_int(value doc, value v) {
    CAMLparam2(doc, v);
    if (Doc_val(doc) == NULL) {
        caml_failwith("doc is NULL");
    }
    CAMLreturn(Val_long(yyjson_get_sint(Val_val(v))));
}

CAMLprim value ml_yyjson_get_sint(value doc, value v) {
    CAMLparam2(doc, v);
    if (Doc_val(doc) == NULL) {
        caml_failwith("doc is NULL");
    }
    CAMLlocal1(x);
    x = caml_copy_int64(yyjson_get_sint(Val_val(v)));
    CAMLreturn(x);
}
CAMLprim value ml_yyjson_get_real(value doc, value v) {
    CAMLparam2(doc, v);
    if (Doc_val(doc) == NULL) {
        caml_failwith("doc is NULL");
    }
    CAMLlocal1(x);
    x = caml_copy_double(yyjson_get_real(Val_val(v)));
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_get_str(value doc, value v) {
    CAMLparam2(doc, v);
    if (Doc_val(doc) == NULL) {
        caml_failwith("doc is NULL");
    }
    CAMLlocal1(x);
    x = caml_copy_string(yyjson_get_str(Val_val(v)));
    CAMLreturn(x);
}

// Mutable value content API

CAMLprim value ml_yyjson_mut_get_type(value doc, value v) {
    CAMLparam2(doc, v);
    if (Mutdoc_val(doc) == NULL) {
        caml_failwith("mutdoc is NULL");
    }
    CAMLreturn(Val_int(yyjson_mut_get_type(Mutval_val(v))));
}
CAMLprim value ml_yyjson_mut_get_subtype(value doc, value v) {
    CAMLparam2(doc, v);
    if (Mutdoc_val(doc) == NULL) {
        caml_failwith("mutdoc is NULL");
    }
    CAMLreturn(Val_int(yyjson_mut_get_subtype(Mutval_val(v)) >> 3));
}

CAMLprim value ml_yyjson_mut_get_bool(value doc, value v) {
    CAMLparam2(doc, v);
    if (Mutdoc_val(doc) == NULL) {
        caml_failwith("mutdoc is NULL");
    }
    CAMLreturn(Val_bool(yyjson_mut_get_bool(Mutval_val(v))));
}

CAMLprim value ml_yyjson_mut_get_int(value doc, value v) {
    CAMLparam2(doc, v);
    if (Mutdoc_val(doc) == NULL) {
        caml_failwith("mutdoc is NULL");
    }
    CAMLreturn(Val_int(yyjson_mut_get_int(Mutval_val(v))));
}

CAMLprim value ml_yyjson_mut_get_sint(value doc, value v) {
    CAMLparam2(doc, v);
    if (Mutdoc_val(doc) == NULL) {
        caml_failwith("mutdoc is NULL");
    }
    CAMLlocal1(x);
    x = caml_copy_int64(yyjson_mut_get_sint(Mutval_val(v)));
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_mut_get_real(value doc, value v) {
    CAMLparam2(doc, v);
    if (Mutdoc_val(doc) == NULL) {
        caml_failwith("mutdoc is NULL");
    }
    CAMLlocal1(x);
    x = caml_copy_double(yyjson_mut_get_real(Mutval_val(v)));
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_mut_get_str(value doc, value v) {
    CAMLparam2(doc, v);
    if (Mutdoc_val(doc) == NULL) {
        caml_failwith("mutdoc is NULL");
    }
    CAMLlocal1(x);
    x = caml_copy_string(yyjson_mut_get_str(Mutval_val(v)));
    CAMLreturn(x);
}
