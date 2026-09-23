open Common

type doc
type va [@@immediate]

exception Doc_is_null

(* Always safe. *)
external free : doc -> unit = "ml_yyjson_mut_doc_free" [@@noalloc]
external is_doc_null : doc -> bool = "ml_is_mutdoc_null" [@@noalloc]

(* Explicit inlinable wrappers rather than partial applications of a
   combinator, which would force each external into a closure. *)
let[@inline] check doc = if is_doc_null doc then raise Doc_is_null

external create : unit -> doc = "ml_yyjson_mut_doc_new"

external doc_set_root_unsafe
  :  doc
  -> va
  -> unit
  = "ml_yyjson_mut_doc_set_root"
[@@noalloc]

let[@inline] doc_set_root doc va =
  check doc;
  doc_set_root_unsafe doc va
;;

(* atom functions functions *)
external null_unsafe : doc -> va = "ml_yyjson_mut_null"
external bool_unsafe : doc -> bool -> va = "ml_yyjson_mut_bool"
external _uint : doc -> int -> va = "ml_yyjson_mut_uint"
external sint_unsafe : doc -> int -> va = "ml_yyjson_mut_sint"
external sint64_unsafe : doc -> int64 -> va = "ml_yyjson_mut_sint64"
external uint64_unsafe : doc -> int64 -> va = "ml_yyjson_mut_uint64"
external float_unsafe : doc -> float -> va = "ml_yyjson_mut_real"
external string_unsafe : doc -> string -> va = "ml_yyjson_mut_strcpy"

let[@inline] null doc =
  check doc;
  null_unsafe doc
;;

let[@inline] bool doc b =
  check doc;
  bool_unsafe doc b
;;

let[@inline] sint doc i =
  check doc;
  sint_unsafe doc i
;;

let[@inline] sint64 doc i =
  check doc;
  sint64_unsafe doc i
;;

let[@inline] uint64 doc i =
  check doc;
  uint64_unsafe doc i
;;

let[@inline] float doc f =
  check doc;
  float_unsafe doc f
;;

let[@inline] string doc s =
  check doc;
  string_unsafe doc s
;;

(* object functions *)
external create_obj_unsafe : doc -> va = "ml_yyjson_mut_obj"

external obj_add_unsafe
  :  doc
  -> va
  -> va
  -> va
  -> bool
  = "ml_yyjson_mut_obj_add"
[@@noalloc]

let[@inline] create_obj doc =
  check doc;
  create_obj_unsafe doc
;;

let[@inline] obj_add doc obj k v =
  check doc;
  obj_add_unsafe doc obj k v
;;

(* array functions *)
external create_arr_unsafe : doc -> va = "ml_yyjson_mut_arr"

external arr_add_unsafe
  :  doc
  -> va
  -> va
  -> bool
  = "ml_yyjson_mut_arr_add_val"
[@@noalloc]

let[@inline] create_arr doc =
  check doc;
  create_arr_unsafe doc
;;

let[@inline] arr_add doc arr v =
  check doc;
  arr_add_unsafe doc arr v
;;

(* get functions (noalloc) *)
external get_type_unsafe : doc -> va -> json_typ = "ml_yyjson_mut_get_type" [@@noalloc]

external get_subtype_unsafe
  :  doc
  -> va
  -> json_subtyp
  = "ml_yyjson_mut_get_subtype"
[@@noalloc]

external _get_bool : doc -> va -> bool = "ml_yyjson_mut_get_bool" [@@noalloc]
external get_int_unsafe : doc -> va -> int = "ml_yyjson_mut_get_int" [@@noalloc]

let[@inline] get_type doc va =
  check doc;
  get_type_unsafe doc va
;;

let[@inline] get_subtype doc va =
  check doc;
  get_subtype_unsafe doc va
;;

let[@inline] get_int doc va =
  check doc;
  get_int_unsafe doc va
;;

(* get functions (alloc) *)
external get_sint_unsafe : doc -> va -> int64 = "ml_yyjson_mut_get_sint"
external get_float_unsafe : doc -> va -> float = "ml_yyjson_mut_get_real"
external get_string_unsafe : doc -> va -> string = "ml_yyjson_mut_get_str"

let[@inline] get_sint doc va =
  check doc;
  get_sint_unsafe doc va
;;

let[@inline] get_float doc va =
  check doc;
  get_float_unsafe doc va
;;

(* Raises [Failure] if the value is not a string. *)
let[@inline] get_string doc va =
  check doc;
  get_string_unsafe doc va
;;

(* iterators *)
external arr_iter_unsafe : doc -> va -> va array = "ml_yyjson_mut_array_iter"

external obj_iter_unsafe
  :  doc
  -> va
  -> (string * va) array
  = "ml_yyjson_mut_obj_iter"

let[@inline] arr_iter doc va =
  check doc;
  arr_iter_unsafe doc va
;;

let[@inline] obj_iter doc va =
  check doc;
  obj_iter_unsafe doc va
;;

(* write functions *)
external write_opts_unsafe : doc -> int -> string = "ml_yyjson_mut_write_opts"

external write_val_opts_unsafe
  :  doc
  -> va
  -> int
  -> string
  = "ml_yyjson_mut_val_write_opts"

external write_file_unsafe : doc -> string -> int -> unit = "ml_yyjson_mut_write_file"

let to_file ?(flags = []) doc path =
  check doc;
  write_file_unsafe doc path (WriteFlag.to_int flags)
;;

let to_string ?(flags = []) doc =
  check doc;
  write_opts_unsafe doc (WriteFlag.to_int flags)
;;

let to_string_val ?(flags = []) doc va =
  check doc;
  write_val_opts_unsafe doc va (WriteFlag.to_int flags)
;;
