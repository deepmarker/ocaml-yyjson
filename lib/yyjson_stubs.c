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
#define YY_ba_val(v) ((value *)Data_custom_val(v))

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
    if (res == NULL) caml_failwith(err.msg);
    x = caml_copy_string(res);
    // Release memory
    if (calc != NULL) {
        calc->free(NULL, res);
    } else {
        free(res);
    }
    CAMLreturn(x);
}

/* How do I add a custom finalizer to the above alloc_dims to avoid leaking
 * memory?*/



/* You need to use `caml_ba_alloc_dims` with a custom
   finalizer. Here's how to modify your code: */

/* ```c */
/* // Add a finalization function for the bigarray */
/* static void finalize_yyjson_buffer(value ba) { */
/*     void *data = Caml_ba_data_val(ba); */
/*     if (data) { */
/*         free(data);  // or use appropriate deallocator */
/*     } */
/* } */

/* CAMLprim value ml_yyjson_write_opts(value doc, value flg, value alc) { */
/*     CAMLparam3(doc, flg, alc); */
/*     CAMLlocal1(x); */
/*     yyjson_write_err err; */
/*     size_t len; */
/*     yyjson_alc *calc = NULL; */
/*     if (Is_some (alc)) calc = Alc_val(Field(alc, 0)); */
/*     char* res = yyjson_write_opts(Doc_val(doc), */
/*                                   Int_val(flg), */
/*                                   calc, */
/*                                   &len, */
/*                                   &err); */
/*     if (!res) */
/*         caml_failwith(err.msg); */

/*     // Allocate bigarray with managed=0 and set up finalization */
/*     x = caml_ba_alloc_dims(CAML_BA_CHAR|CAML_BA_C_LAYOUT|CAML_BA_MANAGED,  */
/*                            1, res, len); */

/*     CAMLreturn(x); */
/* } */
/* ``` */

/* The key is adding `CAML_BA_MANAGED` flag to
   `caml_ba_alloc_dims`. This tells OCaml to call `free()` on the data
   pointer when the bigarray is garbage collected. */

/* If you need a custom deallocator (e.g., using the allocator that
   was used to allocate), you'd need to wrap the buffer in a custom
   block instead, or ensure you use the matching free function from
   yyjson/your allocator. */

/* Could you design a custom block approach to pack both the big array
   and the alc that may have been used to allocate the memory and
   using a finalizer that would correctly clean up things?*/

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
    if (res == NULL) caml_failwith(err.msg);
    x = caml_copy_string(res);
    // Release memory
    if (calc != NULL) {
        calc->free(NULL, res);
    } else {
        free(res);
    }
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

CAMLprim value ml_yyjson_array_iter(value v) {
    CAMLparam1(v);
    CAMLlocal2(mlarr, elt);

    yyjson_arr_iter iter = yyjson_arr_iter_with(Val_val(v));
    mlarr = caml_alloc_tuple(iter.max);

    for (size_t i = 0; i < iter.max; i++) {
        elt = caml_alloc_custom(&yyjson_val_ops, sizeof(yyjson_val *), 0, 1);
        Val_val(elt) = yyjson_arr_iter_next(&iter);
        Store_field(mlarr, i, elt);
    }

    CAMLreturn(mlarr);
}

CAMLprim value ml_yyjson_mut_array_iter(value v) {
    CAMLparam1(v);
    CAMLlocal2(mlarr, elt);

    yyjson_mut_arr_iter iter = yyjson_mut_arr_iter_with(Mutval_val(v));
    mlarr = caml_alloc_tuple(iter.max);

    for (size_t i = 0; i < iter.max; i++) {
        elt = caml_alloc_custom(&yyjson_mut_val_ops, sizeof(yyjson_mut_val *), 0, 1);
        Mutval_val(elt) = yyjson_mut_arr_iter_next(&iter);
        Store_field(mlarr, i, elt);
    }

    CAMLreturn(mlarr);
}

// Object iteration API

CAMLprim value ml_yyjson_obj_iter(value v) {
    CAMLparam1(v);
    CAMLlocal4(mlobj, mlk, mlv, tup);
    yyjson_obj_iter iter = yyjson_obj_iter_with(Val_val(v));
    mlobj = caml_alloc_tuple(iter.max);

    for (int i = 0; i < iter.max; i++) {
        yyjson_val *key = yyjson_obj_iter_next(&iter);
        const char *keystr = yyjson_get_str(key);
        mlk = caml_copy_string(keystr);
        mlv = caml_alloc_custom(&yyjson_val_ops, sizeof(yyjson_val **), 0, 1);
        Val_val(mlv) = yyjson_obj_iter_get_val(key);
        tup = caml_alloc_tuple(2);
        Store_field(tup, 0, mlk);
        Store_field(tup, 1, mlv);
        Store_field(mlobj, i, tup);
    }
    CAMLreturn(mlobj);
}

// Mutable Object iteration API

CAMLprim value ml_yyjson_mut_obj_iter(value v) {
    CAMLparam1(v);
    CAMLlocal4(mlobj, mlk, mlv, tup);
    yyjson_mut_obj_iter iter = yyjson_mut_obj_iter_with(Mutval_val(v));
    mlobj = caml_alloc_tuple(iter.max);

    for (int i = 0; i < iter.max; i++) {
        yyjson_mut_val *key = yyjson_mut_obj_iter_next(&iter);
        const char *keystr = yyjson_mut_get_str(key);
        mlk = caml_copy_string(keystr);
        mlv = caml_alloc_custom(&yyjson_mut_val_ops, sizeof(yyjson_mut_val **), 0, 1);
        Mutval_val(mlv) = yyjson_mut_obj_iter_get_val(key);
        tup = caml_alloc_tuple(2);
        Store_field(tup, 0, mlk);
        Store_field(tup, 1, mlv);
        Store_field(mlobj, i, tup);
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

CAMLprim value ml_yyjson_get_sint_int(value v) {
    return Val_long(yyjson_get_sint(Val_val(v)));
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


/* There is a case of memory corruption when using this code in production.

   Can you analyze what can go wrong?

 */



/* # Potential Memory Issu */

/* ## 1. **Missing Finalizers** (Critical) */

/* Your custom operations use `custom_finalize_default`, which means
   OCaml never calls `yyjson_doc_free()` or
   `yyjson_mut_doc_free()`. Allocated documents leak memory! */

/* ```c */
/* static void finalize_yyjson_doc(value v) { */
/*   yyjson_doc *doc = Doc_val(v); */
/*   if (doc) yyjson_doc_free(doc); */
/* } */

/* static struct custom_operations yyjson_doc_ops = { */
/*   "yyjson.doc.ops", */
/*   finalize_yyjson_doc,  // <-- Add this */
/*   // ... */
/* }; */
/* ``` */

/* Do the same for `yyjson_mut_doc_ops`. */

/* ## 2. **Dangling Pointers in Values** */

/* `yyjson_val` and `yyjson_mut_val` are **not independent** - they're
   pointers into their parent document's memory. When a document is
   freed (manually via `ml_yyjson_doc_free`), all `Val_val` references
   become dangling pointers. */

/* **Problem**: You can do this: */
/* ```ocaml */
/* let v = Yyjson.doc_get_root doc in */
/* Yyjson.doc_free doc; */
/* Yyjson.get_str v  (* SEGFAULT - doc is freed *) */
/* ``` */

/* **Solutions**: */
/* - Don't expose manual `doc_free` */
/* - Keep a reference to the parent doc in each val (add a field) */
/* - Document the lifetime requirement clearly */

/* ## 3. **`CAMLlocalN` Stack Overflow** */

/* `ml_yyjson_mut_obj_iter` still has the `CAMLlocalN(mlk, sz)` issue
   I mentioned earlier. Use the single-local pattern. */

/* ## 4. **Missing NULL Checks** */

/* Several functions don't check for NULL: */
/* ```c */
/* Mutval_val(x) = yyjson_mut_null(Mutdoc_val(doc)); */
/* // What if yyjson_mut_null returns NULL? */
/* ``` */

/* ## 5. **Write API Memory Leak** */

/* In `ml_yyjson_write_opts`, you allocate a bigarray pointing to
   `res` (allocated by yyjson), but there's **no way to free it**. You
   need `Alc_val(alc)->free()` to be called when the bigarray is
   GC'd. */

/* **Fix**: Use `BA_MANAGED | BA_EXTERNAL` with a custom finalizer. */

/* The most critical issue is **#1** (missing finalizers) and **#2**
   (dangling pointers after manual free). */

/* How can I use valgrind to detect memory corruption that arise when
   using this binding? */



/* # Using Valgrind to Detect Memory Corruption in OCaml C Bindings */

/* ## Basic Setup */

/* ```bash */
/* # Compile with debug symbols */
/* ocamlopt -g -runtime-variant d your_code.ml -o test */

/* # Run with valgrind */
/* valgrind --leak-check=full \ */
/*          --show-leak-kinds=all \ */
/*          --track-origins=yes \ */
/*          --verbose \ */
/*          --log-file=valgrind.log \ */
/*          ./test */
/* ``` */

/* ## Key Valgrind Options */

/* ```bash */
/* # More comprehensive checking */
/* valgrind --leak-check=full \ */
/*          --show-reachable=yes \ */
/*          --track-origins=yes \ */
/*          --read-var-info=yes \ */
/*          --expensive-definedness-checks=yes \ */
/*          --malloc-fill=0xAA \ */
/*          --free-fill=0xDD \ */
/*          ./test */
/* ``` */

/* ## Suppressions for OCaml Runtime */

/* OCaml's GC causes false positives. Create `ocaml.supp`: */

/* ``` */
/* { */
/*    ocaml_gc */
/*    Memcheck:Leak */
/*    ... */
/*    fun:caml_* */
/* } */
/* { */
/*    ocaml_alloc */
/*    Memcheck:Leak */
/*    match-leak-kinds: reachable */
/*    fun:malloc */
/*    ... */
/*    fun:caml_alloc* */
/* } */
/* ``` */

/* Use with: `valgrind --suppressions=ocaml.supp ./test` */

/* ## Specific Tests for Your Binding */

/* ```ocaml */
/* (* test_yyjson.ml *) */

/* (* Test #1: Use-after-free *) */
/* let test_uaf () = */
/*   let doc = Yyjson.read_string "{\"x\":1}" in */
/*   let root = Yyjson.doc_get_root doc in */
/*   Yyjson.doc_free doc;  (* Free the doc *) */
/*   Yyjson.get_str root   (* Access freed memory - BOOM *) */

/* (* Test #2: Memory leak *) */
/* let test_leak () = */
/*   for i = 1 to 1000 do */
/*     let doc = Yyjson.read_string "{\"x\":1}" in */
/*     ignore doc  (* Never freed - should see leak *) */
/*   done */

/* (* Test #3: Double free *) */
/* let test_double_free () = */
/*   let doc = Yyjson.read_string "{}" in */
/*   Yyjson.doc_free doc; */
/*   Yyjson.doc_free doc  (* Double free *) */

/* (* Test #4: Invalid read in iteration *) */
/* let test_iter_uaf () = */
/*   let doc = Yyjson.read_string "[1,2,3]" in */
/*   let arr = Yyjson.doc_get_root doc in */
/*   Yyjson.doc_free doc; */
/*   Yyjson.array_iter arr  (* Iterating freed memory *) */

/* let () = */
/*   (* Run tests *) */
/*   try test_leak () with _ -> () */
/* ``` */

/* ## Expected Valgrind Output for Issues */

/* ### Use-After-Free: */
/* ``` */
/* Invalid read of size 8 */
/*    at 0x...: yyjson_get_str */
/*    by 0x...: ml_yyjson_get_str */
/* Address 0x... is 24 bytes inside a block of size 128 free'd */
/*    previously allocated by yyjson_doc_new */
/* ``` */

/* ### Memory Leak: */
/* ``` */
/* LEAK SUMMARY: */
/*    definitely lost: 128,000 bytes in 1,000 blocks */
/*    indirectly lost: 64,000 bytes in 500 blocks */
/* ``` */

/* ### Double Free: */
/* ``` */
/* Invalid free() / delete / delete[] / realloc() */
/*    at 0x...: free */
/*    by 0x...: yyjson_doc_free */
/* Address 0x... is 0 bytes inside a block of size 128 free'd */
/* ``` */

/* ## Debugging Tips */

/* 1. **Use GDB with Valgrind**: */
/*    ```bash */
/*    valgrind --vgdb=yes --vgdb-error=0 ./test */
/*    # In another terminal: */
/*    gdb ./test */
/*    (gdb) target remote | vgdb */
/*    ``` */

/* 2. **Track specific allocations**: */
/*    ```bash */
/*    valgrind --trace-children=yes --log-file=val_%p.log ./test */
/*    ``` */

/* 3. **Memory errors only** (skip leaks): */
/*    ```bash */
/*    valgrind --leak-check=no --track-origins=yes ./test */
/*    ``` */

/* ## Alternative: AddressSanitizer (Faster) */

/* ```bash */
/* # Compile with ASAN */
/* export ASAN_OPTIONS=detect_leaks=1:symbolize=1 */
/* ocamlopt -g -ccopt -fsanitize=address \ */
/*          -cclib -fsanitize=address \ */
/*          your_code.ml -o test */

/* ./test */
/* ``` */

/* ASAN is faster but requires recompilation of OCaml runtime ideally. */
