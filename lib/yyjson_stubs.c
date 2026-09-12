#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <yyjson.h>

static void doc_free(value doc) {
    yyjson_doc_free((*(yyjson_doc **) Data_custom_val(doc)));
}

/* yyjson_mut_doc_free() is documented as a no-op on NULL, so this is safe
   after an explicit ml_yyjson_mut_doc_free(). */
static void mut_doc_free(value doc) {
    yyjson_mut_doc_free((*(yyjson_mut_doc **) Data_custom_val(doc)));
}

static struct custom_operations yyjson_doc_ops = {
  "yyjson.doc.ops",
  doc_free,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static struct custom_operations yyjson_mut_doc_ops = {
    "yyjson.mut.doc.ops",       mut_doc_free,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

/* A mutable document grows on demand; this is only the hint used to apply
   GC pressure, not an allocation size. */
#define MUT_DOC_MEM_HINT 4096

#define Doc_val(v) (*((yyjson_doc **) Data_custom_val(v)))
#define Mutdoc_val(v) (*((yyjson_mut_doc **) Data_custom_val(v)))

CAMLprim value ml_is_doc_null(value doc) {
    return Val_bool(Doc_val(doc) == NULL);
}
CAMLprim value ml_is_mutdoc_null(value doc) {
    return Val_bool(Mutdoc_val(doc) == NULL);
}
/* Version of the shared library actually linked at run time. */
CAMLprim value ml_yyjson_version (value unit) {
    return(Val_int(yyjson_version()));
}
/* Version of the header this binding was compiled against. Comparing the
   two detects a header/library skew, which would silently shift flag
   values and struct layouts. */
CAMLprim value ml_yyjson_version_compiled (value unit) {
    return(Val_int(YYJSON_VERSION_HEX));
}

CAMLprim value ml_yyjson_read_opts(value buf, value pos, value len, value flg) {
    CAMLparam4(buf, pos, len, flg);
    CAMLlocal1(x);
    yyjson_read_err err;
    char *data = NULL;
    switch (Tag_val(buf)) {
    case String_tag:
        data = (char *)String_val(buf) + Long_val(pos);
        break;
    default:
        data = ((char *)Caml_ba_data_val(buf)) + Long_val(pos);
    }
    /* Parse before allocating: no OCaml allocation may happen while [data]
       points into an OCaml string, and a failed parse must not leave a
       custom block with an uninitialised pointer behind. */
    yyjson_doc *doc = yyjson_read_opts(data, Long_val(len), Int_val(flg), NULL, &err);
    if (!doc) caml_failwith(err.msg);
    x = caml_alloc_custom_mem(&yyjson_doc_ops, sizeof(yyjson_doc *), Long_val(len));
    Doc_val(x) = doc;
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_read_file(value file, value flg) {
    CAMLparam2(file, flg);
    CAMLlocal1(x);
    yyjson_read_err err;
    yyjson_doc *doc = yyjson_read_file(String_val(file), Int_val(flg), NULL, &err);
    if (!doc) caml_failwith(err.msg);
    x = caml_alloc_custom_mem(&yyjson_doc_ops,
                              sizeof (yyjson_doc *),
                              yyjson_doc_get_read_size(doc));
    Doc_val(x) = doc;
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_doc_get_root(value doc) {
    return Val_ptr(yyjson_doc_get_root(Doc_val(doc)));
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

/* yyjson hands back a length; use it rather than re-deriving one with
   strlen(), which would also truncate at an embedded NUL. */
static value copy_written(char *res, size_t len) {
    CAMLparam0();
    CAMLlocal1(x);
    x = caml_alloc_initialized_string(len, res);
    CAMLreturn(x);
}

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
    x = copy_written(res, len);
    free(res);
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_val_write_opts(value doc, value val, value flg) {
    CAMLparam3(doc, val, flg);
    CAMLlocal1(x);
    yyjson_write_err err;
    size_t len;
    char* res = yyjson_val_write_opts(Ptr_val(val),
                                  Int_val(flg),
                                  NULL,
                                  &len,
                                  &err);
    if (res == NULL) caml_failwith(err.msg);
    x = copy_written(res, len);
    free(res);
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_mut_val_write_opts(value doc, value val, value flg) {
    CAMLparam3(doc, val, flg);
    CAMLlocal1(x);
    yyjson_write_err err;
    size_t len;
    char* res = yyjson_mut_val_write_opts(Ptr_val(val),
                                  Int_val(flg),
                                  NULL,
                                  &len,
                                  &err);
    if (res == NULL) caml_failwith(err.msg);
    x = copy_written(res, len);
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
    x = copy_written(res, len);
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

    x = caml_alloc_custom_mem(&yyjson_mut_doc_ops,
                              sizeof (yyjson_mut_doc *),
                              MUT_DOC_MEM_HINT);
    Mutdoc_val(x) = doc;
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_mut_doc_free(value doc) {
  yyjson_mut_doc_free(Mutdoc_val(doc));
  Mutdoc_val(doc) = NULL;
  return Val_unit;
}

CAMLprim value ml_yyjson_mut_doc_set_root(value doc, value root) {
  yyjson_mut_doc_set_root(Mutdoc_val(doc), Ptr_val(root));
  return Val_unit;
}

// Mutable array API

CAMLprim value ml_yyjson_mut_arr(value doc) {
    CAMLparam1(doc);
    CAMLlocal1(x);
    yyjson_mut_val *v = yyjson_mut_arr(Mutdoc_val(doc));
    if (!v) caml_failwith("yyjson_mut_arr");
    CAMLreturn(Val_ptr(v));
}

CAMLprim value ml_yyjson_mut_arr_add_val(value doc, value arr, value v) {
    return Val_bool(yyjson_mut_arr_add_val(Ptr_val(arr), Ptr_val(v)));
}

// Mutable object API

CAMLprim value ml_yyjson_mut_obj(value doc) {
    CAMLparam1(doc);
    CAMLlocal1(x);
    yyjson_mut_val *v = yyjson_mut_obj(Mutdoc_val(doc));
    if (!v) caml_failwith("yyjson_mut_obj");
    CAMLreturn(Val_ptr(v));
}

CAMLprim value ml_yyjson_mut_obj_add(value doc, value obj, value k, value v) {
    return Val_bool(yyjson_mut_obj_add(Ptr_val(obj), Ptr_val(k), Ptr_val(v)));
}

// Mutable value API

CAMLprim value ml_yyjson_mut_null(value doc) {
    return Val_ptr(yyjson_mut_null(Mutdoc_val(doc)));
}
CAMLprim value ml_yyjson_mut_bool(value doc, value b) {
    return Val_ptr(yyjson_mut_bool(Mutdoc_val(doc), Bool_val(b)));
}
CAMLprim value ml_yyjson_mut_uint(value doc, value b) {
    return Val_ptr(yyjson_mut_uint(Mutdoc_val(doc), Long_val(b)));
}
CAMLprim value ml_yyjson_mut_sint(value doc, value b) {
    return Val_ptr(yyjson_mut_sint(Mutdoc_val(doc), Long_val(b)));
}
CAMLprim value ml_yyjson_mut_real(value doc, value b) {
    return Val_ptr(yyjson_mut_real(Mutdoc_val(doc), Double_val(b)));
}
CAMLprim value ml_yyjson_mut_strcpy(value doc, value b) {
    return Val_ptr(yyjson_mut_strncpy(Mutdoc_val(doc),
                                      String_val(b),
                                      caml_string_length(b)));
}

/* [va] is an immediate (Val_ptr p = p + 1), so the fields holding one need
   no write barrier; only the key strings and the pair blocks do. */
CAMLprim value ml_yyjson_array_iter(value doc, value v) {
    CAMLparam2(doc, v);
    CAMLlocal1(mlarr);

    if (Doc_val(doc) == NULL) {
        caml_failwith("doc is NULL");
    }

    yyjson_arr_iter iter = yyjson_arr_iter_with(Ptr_val(v));
    mlarr = caml_alloc_tuple(iter.max);

    for (size_t i = 0; i < iter.max; i++) {
        yyjson_val *elt = yyjson_arr_iter_next(&iter);
        Field(mlarr, i) = Val_ptr(elt);
    }

    CAMLreturn(mlarr);
}

CAMLprim value ml_yyjson_mut_array_iter(value doc, value v) {
    CAMLparam2(doc, v);
    CAMLlocal1(mlarr);

    if (Mutdoc_val(doc) == NULL) {
        caml_failwith("mutdoc is NULL");
    }

    yyjson_mut_arr_iter iter = yyjson_mut_arr_iter_with(Ptr_val(v));
    mlarr = caml_alloc_tuple(iter.max);

    for (size_t i = 0; i < iter.max; i++) {
        yyjson_mut_val *elt = yyjson_mut_arr_iter_next(&iter);
        Field(mlarr, i) = Val_ptr(elt);
    }

    CAMLreturn(mlarr);
}

// Object iteration API

CAMLprim value ml_yyjson_obj_iter(value doc, value v) {
    CAMLparam2(doc, v);
    CAMLlocal3(mlobj, mlk, tup);

    if (Doc_val(doc) == NULL) {
        caml_failwith("doc is NULL");
    }

    yyjson_obj_iter iter = yyjson_obj_iter_with(Ptr_val(v));
    mlobj = caml_alloc_tuple(iter.max);

    for (size_t i = 0; i < iter.max; i++) {
        yyjson_val *key = yyjson_obj_iter_next(&iter);
        mlk = caml_alloc_initialized_string(yyjson_get_len(key),
                                            yyjson_get_str(key));
        yyjson_val *val = yyjson_obj_iter_get_val(key);
        tup = caml_alloc_tuple(2);
        Store_field(tup, 0, mlk);
        Field(tup, 1) = Val_ptr(val);
        Store_field(mlobj, i, tup);
    }
    CAMLreturn(mlobj);
}

CAMLprim value ml_yyjson_obj_get(value doc, value obj, value key) {
    CAMLparam3(doc, obj, key);
    CAMLlocal1(some);

    if (Doc_val(doc) == NULL) {
        caml_failwith("doc is NULL");
    }

    yyjson_val *found = yyjson_obj_getn(Ptr_val(obj),
                                        String_val(key),
                                        caml_string_length(key));
    if (found == NULL) CAMLreturn(Val_int(0));
    some = caml_alloc_small(1, 0);
    Field(some, 0) = Val_ptr(found);
    CAMLreturn(some);
}

/* Ordered member lookup.

   yyjson_obj_iter_getn resumes the scan where the previous lookup stopped
   and wraps around, so reading n members in the document's own key order
   costs one pass instead of n independent searches.

   The iterator lives in an abstract block rather than a custom one: it
   borrows pointers into the document and owns nothing, so there is nothing
   to finalize, and compare, hash and marshal are all meaningless for it.
   That saves the ops word a custom block spends, and the indirection
   through it. Like a custom block, an abstract block is not traced by the
   GC, which is what makes it legal to hold raw pointers -- and it must
   therefore never hold an OCaml value. */

#define Objiter_wosize \
    ((sizeof(yyjson_obj_iter) + sizeof(value) - 1) / sizeof(value))
#define Objiter_val(v) ((yyjson_obj_iter *) Data_abstract_val(v))

CAMLprim value ml_yyjson_obj_iter_init(value doc, value v) {
    CAMLparam2(doc, v);
    CAMLlocal1(x);

    if (Doc_val(doc) == NULL) {
        caml_failwith("doc is NULL");
    }

    /* An abstract block is not zero-filled, but yyjson_obj_iter_init writes
       every field. It clears the iterator if the value is not an object,
       which makes every subsequent lookup return NULL. */
    x = caml_alloc_small(Objiter_wosize, Abstract_tag);
    yyjson_obj_iter_init(Ptr_val(v), Objiter_val(x));
    CAMLreturn(x);
}

CAMLprim value ml_yyjson_obj_iter_getn(value it, value key) {
    CAMLparam2(it, key);
    CAMLlocal1(some);

    yyjson_val *found = yyjson_obj_iter_getn(Objiter_val(it),
                                             String_val(key),
                                             caml_string_length(key));
    if (found == NULL) CAMLreturn(Val_int(0));
    some = caml_alloc_small(1, 0);
    Field(some, 0) = Val_ptr(found);
    CAMLreturn(some);
}

/* Array stepping, so a fold needs no intermediate array. get_next handles
   the container offset, so this is O(1) per element even when the array is
   not flat -- unlike yyjson_arr_get, which is a linear search. */

CAMLprim value ml_yyjson_arr_size(value doc, value v) {
    return Val_long(yyjson_arr_size(Ptr_val(v)));
}
CAMLprim value ml_yyjson_arr_first(value doc, value v) {
    return Val_ptr(yyjson_arr_get_first(Ptr_val(v)));
}
CAMLprim value ml_yyjson_arr_next(value doc, value v) {
    return Val_ptr(unsafe_yyjson_get_next(Ptr_val(v)));
}

/* Object stepping, for members whose names are not known ahead of time.
   An object is stored as alternating key and value slots: the value sits
   immediately after its key, and the next key is one step past that
   value -- a step that follows the container offset, so a nested object
   or array is skipped rather than descended into. */

CAMLprim value ml_yyjson_obj_size(value doc, value v) {
    return Val_long(yyjson_obj_size(Ptr_val(v)));
}
CAMLprim value ml_yyjson_obj_first_key(value doc, value v) {
    if (yyjson_obj_size(Ptr_val(v)) == 0) return Val_ptr(NULL);
    return Val_ptr(unsafe_yyjson_get_first(Ptr_val(v)));
}
CAMLprim value ml_yyjson_obj_key_value(value doc, value key) {
    return Val_ptr((yyjson_val *) Ptr_val(key) + 1);
}
CAMLprim value ml_yyjson_obj_next_key(value doc, value key) {
    return Val_ptr(unsafe_yyjson_get_next((yyjson_val *) Ptr_val(key) + 1));
}

CAMLprim value ml_yyjson_obj_get_string(value doc, value obj, value key) {
    CAMLparam3(doc, obj, key);
    CAMLlocal2(some, string);

    if (Doc_val(doc) == NULL) {
        caml_failwith("doc is NULL");
    }

    yyjson_val *found = yyjson_obj_getn(Ptr_val(obj),
                                        String_val(key),
                                        caml_string_length(key));
    const char *contents = yyjson_get_str(found);
    if (contents == NULL) CAMLreturn(Val_int(0));
    string = caml_alloc_initialized_string(yyjson_get_len(found), contents);
    some = caml_alloc_small(1, 0);
    Field(some, 0) = string;
    CAMLreturn(some);
}

CAMLprim value ml_yyjson_mut_obj_iter(value doc, value v) {
    CAMLparam2(doc, v);
    CAMLlocal3(mlobj, mlk, tup);

    if (Mutdoc_val(doc) == NULL) {
        caml_failwith("mutdoc is NULL");
    }

    yyjson_mut_obj_iter iter = yyjson_mut_obj_iter_with(Ptr_val(v));
    mlobj = caml_alloc_tuple(iter.max);

    for (size_t i = 0; i < iter.max; i++) {
        yyjson_mut_val *key = yyjson_mut_obj_iter_next(&iter);
        mlk = caml_alloc_initialized_string(yyjson_mut_get_len(key),
                                            yyjson_mut_get_str(key));
        yyjson_mut_val *val = yyjson_mut_obj_iter_get_val(key);
        tup = caml_alloc_tuple(2);
        Store_field(tup, 0, mlk);
        Field(tup, 1) = Val_ptr(val);
        Store_field(mlobj, i, tup);
    }
    CAMLreturn(mlobj);
}

// Value content API (immutable)

CAMLprim value ml_yyjson_get_type(value doc, value v) {
    return Val_int(yyjson_get_type(Ptr_val(v)));
}
CAMLprim value ml_yyjson_get_subtype(value doc, value v) {
    return Val_int(yyjson_get_subtype(Ptr_val(v)) >> 3);
}
CAMLprim value ml_yyjson_get_bool(value doc, value v) {
    return Val_bool(yyjson_get_bool(Ptr_val(v)));
}
CAMLprim value ml_yyjson_get_sint_int(value doc, value v) {
    return Val_long(yyjson_get_sint(Ptr_val(v)));
}

CAMLprim value ml_yyjson_get_sint(value doc, value v) {
    CAMLparam2(doc, v);
    CAMLlocal1(x);
    x = caml_copy_int64(yyjson_get_sint(Ptr_val(v)));
    CAMLreturn(x);
}
/* Bit pattern of the u64; the OCaml side interprets it as unsigned. */
CAMLprim value ml_yyjson_get_uint(value doc, value v) {
    CAMLparam2(doc, v);
    CAMLlocal1(x);
    x = caml_copy_int64((int64_t)yyjson_get_uint(Ptr_val(v)));
    CAMLreturn(x);
}
CAMLprim value ml_yyjson_get_real(value doc, value v) {
    CAMLparam2(doc, v);
    CAMLlocal1(x);
    x = caml_copy_double(yyjson_get_real(Ptr_val(v)));
    CAMLreturn(x);
}
/* Correct for all three number subtypes, including u64 > INT64_MAX. */
CAMLprim value ml_yyjson_get_num(value doc, value v) {
    CAMLparam2(doc, v);
    CAMLlocal1(x);
    x = caml_copy_double(yyjson_get_num(Ptr_val(v)));
    CAMLreturn(x);
}
CAMLprim value ml_yyjson_get_str(value doc, value v) {
    CAMLparam2(doc, v);
    CAMLlocal1(x);
    yyjson_val *val = Ptr_val(v);
    const char *s = yyjson_get_str(val);
    if (s == NULL) caml_failwith("Yyjson.get_string: value is not a string");
    x = caml_alloc_initialized_string(yyjson_get_len(val), s);
    CAMLreturn(x);
}

// Mutable value content API

CAMLprim value ml_yyjson_mut_get_type(value doc, value v) {
    return Val_int(yyjson_mut_get_type(Ptr_val(v)));
}
CAMLprim value ml_yyjson_mut_get_subtype(value doc, value v) {
    return Val_int(yyjson_mut_get_subtype(Ptr_val(v)) >> 3);
}
CAMLprim value ml_yyjson_mut_get_bool(value doc, value v) {
    return Val_bool(yyjson_mut_get_bool(Ptr_val(v)));
}
CAMLprim value ml_yyjson_mut_get_int(value doc, value v) {
    return Val_int(yyjson_mut_get_int(Ptr_val(v)));
}

CAMLprim value ml_yyjson_mut_get_sint(value doc, value v) {
    CAMLparam2(doc, v);
    CAMLlocal1(x);
    x = caml_copy_int64(yyjson_mut_get_sint(Ptr_val(v)));
    CAMLreturn(x);
}
CAMLprim value ml_yyjson_mut_get_real(value doc, value v) {
    CAMLparam2(doc, v);
    CAMLlocal1(x);
    x = caml_copy_double(yyjson_mut_get_real(Ptr_val(v)));
    CAMLreturn(x);
}
CAMLprim value ml_yyjson_mut_get_str(value doc, value v) {
    CAMLparam2(doc, v);
    CAMLlocal1(x);
    yyjson_mut_val *val = Ptr_val(v);
    const char *s = yyjson_mut_get_str(val);
    if (s == NULL) caml_failwith("Yyjson.Mutable.get_string: value is not a string");
    x = caml_alloc_initialized_string(yyjson_mut_get_len(val), s);
    CAMLreturn(x);
}
