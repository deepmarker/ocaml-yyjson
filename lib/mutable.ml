open Common

type doc
type va [@@immediate]

exception Doc_is_null

(* Always safe. *)
external free : doc -> unit = "ml_yyjson_mut_doc_free" [@@noalloc]
external is_doc_null : doc -> bool = "ml_is_mutdoc_null" [@@noalloc]

let with_check_doc0 f doc = if is_doc_null doc then raise Doc_is_null else f doc
let with_check_doc1 f doc va = if is_doc_null doc then raise Doc_is_null else f doc va

let with_check_doc2 f doc v1 v2 =
  if is_doc_null doc then raise Doc_is_null else f doc v1 v2
;;

external create : unit -> doc = "ml_yyjson_mut_doc_new"
external doc_set_root : doc -> va -> unit = "ml_yyjson_mut_doc_set_root" [@@noalloc]

let doc_set_root = with_check_doc1 doc_set_root

(* atom functions functions *)
external null : doc -> va = "ml_yyjson_mut_null" [@@noalloc]
external bool : doc -> bool -> va = "ml_yyjson_mut_bool" [@@noalloc]
external _uint : doc -> int -> va = "ml_yyjson_mut_uint" [@@noalloc]
external sint : doc -> int -> va = "ml_yyjson_mut_sint" [@@noalloc]
external float : doc -> float -> va = "ml_yyjson_mut_real" [@@noalloc]
external string : doc -> string -> va = "ml_yyjson_mut_strcpy" [@@noalloc]

let null = with_check_doc0 null
let bool = with_check_doc1 bool
let sint = with_check_doc1 sint
let float = with_check_doc1 float
let string = with_check_doc1 string

(* object functions *)
external create_obj : doc -> va = "ml_yyjson_mut_obj"
external obj_add : doc -> va -> va -> va -> bool = "ml_yyjson_mut_obj_add" [@@noalloc]

let create_obj = with_check_doc0 create_obj
let obj_add = with_check_doc2 obj_add

(* array functions *)
external create_arr : doc -> va = "ml_yyjson_mut_arr"
external arr_add : doc -> va -> va -> bool = "ml_yyjson_mut_arr_add_val" [@@noalloc]

let create_arr = with_check_doc0 create_arr
let arr_add = with_check_doc2 arr_add

(* get functions (noalloc) *)
external get_type : doc -> va -> json_typ = "ml_yyjson_mut_get_type" [@@noalloc]
external get_subtype : doc -> va -> json_subtyp = "ml_yyjson_mut_get_subtype" [@@noalloc]
external _get_bool : doc -> va -> bool = "ml_yyjson_mut_get_bool" [@@noalloc]
external get_int : doc -> va -> int = "ml_yyjson_mut_get_int" [@@noalloc]

let get_type = with_check_doc1 get_type
let get_subtype = with_check_doc1 get_subtype
let get_int = with_check_doc1 get_int

(* get functions (alloc) *)
external get_sint : doc -> va -> int64 = "ml_yyjson_mut_get_sint"
external get_float : doc -> va -> float = "ml_yyjson_mut_get_real"
external get_string : doc -> va -> string = "ml_yyjson_mut_get_str"

let get_sint = with_check_doc1 get_sint
let get_float = with_check_doc1 get_float
let get_string = with_check_doc1 get_string

(* iterators *)
external arr_iter : doc -> va -> va array = "ml_yyjson_mut_array_iter"
external obj_iter : doc -> va -> (string * va) array = "ml_yyjson_mut_obj_iter"

let arr_iter = with_check_doc1 arr_iter
let obj_iter = with_check_doc1 obj_iter

(* write functions *)
external write_opts : doc -> int -> string = "ml_yyjson_mut_write_opts"
external write_val_opts : doc -> va -> int -> string = "ml_yyjson_mut_val_write_opts"
external write_file : doc -> string -> int -> unit = "ml_yyjson_mut_write_file"

let write_opts = with_check_doc1 write_opts
let write_val_opts = with_check_doc1 write_val_opts
let write_file = with_check_doc2 write_file
let to_file ?(flags = []) doc path = write_file doc path (WriteFlag.to_int flags)
let to_string ?(flags = []) doc = write_opts doc (WriteFlag.to_int flags)
let to_string_val ?(flags = []) doc va = write_val_opts doc va (WriteFlag.to_int flags)
