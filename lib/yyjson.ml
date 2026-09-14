open StdLabels
include Common

type doc
type va [@@immediate]

exception Unexpected_type of json_typ

(* Always safe. *)
external version : unit -> int = "ml_yyjson_version" [@@noalloc]

external version_compiled : unit -> int = "ml_yyjson_version_compiled" [@@noalloc]
external is_doc_null : doc -> bool = "ml_is_doc_null" [@@noalloc]
external free_doc : doc -> unit = "ml_yyjson_doc_free" [@@noalloc]

(* These are bound as explicit [let[@inline]] wrappers rather than partial
   applications of a [with_check_doc] combinator: a partial application
   forces the external into a closure, so every accessor call becomes a
   generic [caml_apply] plus two indirect calls and loses [@@noalloc]. *)
let[@inline] check doc = if is_doc_null doc then raise Mutable.Doc_is_null

external doc_get_root_unsafe : doc -> va = "ml_yyjson_doc_get_root"

let[@inline] doc_get_root doc =
  check doc;
  doc_get_root_unsafe doc
;;

type version =
  { major : int
  ; minor : int
  ; patch : int
  }

let decode_version v =
  { major = v lsr 16; minor = (v lsr 8) land 0xff; patch = v land 0xff }
;;

let version = lazy (decode_version (version ()))
let compiled_version = lazy (decode_version (version_compiled ()))

type value =
  { doc : doc
  ; va : va
  }

let value_of_doc doc = { doc; va = doc_get_root doc }
let doc_of_value { doc; _ } = doc

external arr_iter_unsafe : doc -> va -> va array = "ml_yyjson_array_iter"
external obj_iter_unsafe : doc -> va -> (string * va) array = "ml_yyjson_obj_iter"
external obj_get_va : doc -> va -> string -> va option = "ml_yyjson_obj_get"

external obj_get_string_va
  :  doc
  -> va
  -> string
  -> string option
  = "ml_yyjson_obj_get_string"

let[@inline] arr_iter doc va =
  check doc;
  arr_iter_unsafe doc va
;;

let[@inline] obj_iter doc va =
  check doc;
  obj_iter_unsafe doc va
;;

let obj_get { doc; va } key =
  match obj_get_va doc va key with
  | None -> None
  | Some va -> Some { doc; va }
;;

let obj_get_string { doc; va } key = obj_get_string_va doc va key

(* no alloc*)
external get_type_unsafe : doc -> va -> json_typ = "ml_yyjson_get_type" [@@noalloc]

external get_subtype_unsafe
  :  doc
  -> va
  -> json_subtyp
  = "ml_yyjson_get_subtype"
[@@noalloc]

external get_bool_unsafe : doc -> va -> bool = "ml_yyjson_get_bool" [@@noalloc]

(* [ml_yyjson_get_sint_int] is deliberately not bound: [Val_long] of an
   [int64] truncates to 63 bits, which is what used to make [view] report
   Int64.max_int as -1. Read integers through [get_int64]/[get_uint64]. *)

let[@inline] get_type doc va =
  check doc;
  get_type_unsafe doc va
;;

let[@inline] get_subtype doc va =
  check doc;
  get_subtype_unsafe doc va
;;

let[@inline] get_bool doc va =
  check doc;
  get_bool_unsafe doc va
;;

(* alloc *)
external get_int64_unsafe : doc -> va -> int64 = "ml_yyjson_get_sint"
external get_uint64_unsafe : doc -> va -> int64 = "ml_yyjson_get_uint"
external _get_real : doc -> va -> float = "ml_yyjson_get_real"
external get_num_unsafe : doc -> va -> float = "ml_yyjson_get_num"
external get_string_unsafe : doc -> va -> string = "ml_yyjson_get_str"

let[@inline] get_int64 doc va =
  check doc;
  get_int64_unsafe doc va
;;

let[@inline] get_uint64 doc va =
  check doc;
  get_uint64_unsafe doc va
;;

(* Correct for sint, uint and real alike, including u64 > INT64_MAX. *)
let[@inline] get_num doc va =
  check doc;
  get_num_unsafe doc va
;;

let[@inline] get_string doc va =
  check doc;
  get_string_unsafe doc va
;;

let typ { doc; va } = get_type doc va

let string_value { doc; va } =
  match get_type doc va with
  | Str -> Some (get_string doc va)
  | _ -> None
;;

(* See yyjson_decimal.c for the layout and the rule for trailing zeros. *)
external get_packed_decimal_unsafe
  :  doc
  -> va
  -> int
  = "ml_yyjson_get_packed_decimal"
[@@noalloc]

let not_a_packed_decimal = Int.min_int

let packed_decimal { doc; va } =
  check doc;
  get_packed_decimal_unsafe doc va
;;

let bool_value { doc; va } =
  match get_type doc va with
  | Bool -> Some (get_bool doc va)
  | _ -> None
;;

(* Any JSON number, integer subtypes included: JSON Schema's "number"
   accepts them, and [get_num] converts all three subtypes correctly. *)
let float_value { doc; va } =
  match get_type doc va with
  | Num -> Some (get_num doc va)
  | _ -> None
;;

(* yyjson stores every non-negative integer with subtype UINT, so a UINT
   only overflows [int64] once its high bit is set; [yyjson_get_sint] would
   silently return it as a negative number. *)
let int64_value { doc; va } =
  match get_type doc va with
  | Num ->
    (match get_subtype doc va with
     | Real -> None
     | TrueSintNoesc -> Some (get_int64 doc va)
     | NoneFalseUint ->
       let i = get_uint64 doc va in
       if Int64.compare i 0L < 0 then None else Some i)
  | _ -> None
;;

(* The bit pattern of an unsigned 64-bit JSON integer. Values above
   [Int64.max_int] come back negative and must be read as unsigned. *)
let uint64_value { doc; va } =
  match get_type doc va with
  | Num ->
    (match get_subtype doc va with
     | Real -> None
     | NoneFalseUint -> Some (get_uint64 doc va)
     | TrueSintNoesc ->
       let i = get_int64 doc va in
       if Int64.compare i 0L < 0 then None else Some i)
  | _ -> None
;;

let array_values { doc; va } =
  match get_type doc va with
  | Arr -> Some (Array.map (arr_iter doc va) ~f:(fun va -> { doc; va }))
  | _ -> None
;;

type obj_iter

external obj_iter_init : doc -> va -> obj_iter = "ml_yyjson_obj_iter_init"
external obj_iter_getn : obj_iter -> string -> va option = "ml_yyjson_obj_iter_getn"

type obj_cursor =
  { cursor_doc : doc
  ; cursor_iter : obj_iter
  }

let obj_cursor { doc; va } =
  match get_type doc va with
  | Obj -> Some { cursor_doc = doc; cursor_iter = obj_iter_init doc va }
  | _ -> None
;;

let cursor_get { cursor_doc = doc; cursor_iter } key =
  match obj_iter_getn cursor_iter key with
  | None -> None
  | Some va -> Some { doc; va }
;;

external arr_size_unsafe : doc -> va -> int = "ml_yyjson_arr_size" [@@noalloc]
external arr_first_unsafe : doc -> va -> va = "ml_yyjson_arr_first" [@@noalloc]
external arr_next_unsafe : doc -> va -> va = "ml_yyjson_arr_next" [@@noalloc]

let arr_length { doc; va } =
  match get_type doc va with
  | Arr -> Some (arr_size_unsafe doc va)
  | _ -> None
;;

(* Steps the array in place rather than materialising [array_values]'
   intermediate array. The element pointer past the last element is
   computed but never dereferenced, which is why the loop counts. *)
let arr_fold { doc; va } ~init ~f =
  match get_type doc va with
  | Arr ->
    let n = arr_size_unsafe doc va in
    let rec go acc i cur =
      if i >= n
      then acc
      else go (f acc { doc; va = cur }) (i + 1) (arr_next_unsafe doc cur)
    in
    Some (go init 0 (arr_first_unsafe doc va))
  | _ -> None
;;

external arr_get_unsafe : doc -> va -> int -> va = "ml_yyjson_arr_get" [@@noalloc]

(* Bounds are checked here, so the stub never returns NULL. *)
let arr_get { doc; va } i =
  match get_type doc va with
  | Arr when i >= 0 && i < arr_size_unsafe doc va ->
    Some { doc; va = arr_get_unsafe doc va i }
  | _ -> None
;;

(* The type is checked once, when the cursor is made; each step after that
   is a counter test and one pointer step. The pointer past the last element
   is never computed, which is what the count is for. *)
type arr_cursor =
  { arr_doc : doc
  ; mutable next_va : va
  ; mutable remaining : int
  }

let arr_cursor { doc; va } =
  match get_type doc va with
  | Arr ->
    Some
      { arr_doc = doc
      ; next_va = arr_first_unsafe doc va
      ; remaining = arr_size_unsafe doc va
      }
  | _ -> None
;;

let arr_remaining c = c.remaining

let arr_cursor_next c =
  match c.remaining with
  | 0 -> None
  | remaining ->
    let va = c.next_va in
    c.remaining <- remaining - 1;
    if remaining > 1 then c.next_va <- arr_next_unsafe c.arr_doc va;
    Some { doc = c.arr_doc; va }
;;

external obj_size_unsafe : doc -> va -> int = "ml_yyjson_obj_size" [@@noalloc]
external obj_first_key_unsafe : doc -> va -> va = "ml_yyjson_obj_first_key" [@@noalloc]
external obj_key_value_unsafe : doc -> va -> va = "ml_yyjson_obj_key_value" [@@noalloc]
external obj_next_key_unsafe : doc -> va -> va = "ml_yyjson_obj_next_key" [@@noalloc]

let obj_length { doc; va } =
  match get_type doc va with
  | Obj -> Some (obj_size_unsafe doc va)
  | _ -> None
;;

(* For members whose names are not known ahead of time; when they are,
   [obj_cursor] is cheaper. Keys are read with [get_string], so they are
   length-correct and keep an embedded NUL like any other string here. *)
let obj_fold { doc; va } ~init ~f =
  match get_type doc va with
  | Obj ->
    let n = obj_size_unsafe doc va in
    let rec go acc i key =
      if i >= n
      then acc
      else (
        let name = get_string doc key in
        let value = { doc; va = obj_key_value_unsafe doc key } in
        go (f acc name value) (i + 1) (obj_next_key_unsafe doc key))
    in
    Some (go init 0 (obj_first_key_unsafe doc va))
  | _ -> None
;;

(* values created here have the same lifetime as doc. Make sure they
   are never GCed before doc in OCaml too. *)
let view { doc; va } =
  match get_type doc va with
  | (ErrInvalid | Raw) as typ -> raise (Unexpected_type typ)
  | Null -> `Null
  | Bool ->
    `Bool
      (match get_subtype doc va with
       | NoneFalseUint -> false
       | _ -> true)
  (* A [Json_repr] view can only carry a float, so integers above 2^53 lose
     precision here by construction: use [int64_value] when they matter.
     [get_num] at least keeps the value and its sign correct. *)
  | Num -> `Float (get_num doc va)
  | Str -> `String (get_string doc va)
  | Arr ->
    let a = arr_iter doc va in
    let a = Array.fold_right a ~init:[] ~f:(fun va a -> { doc; va } :: a) in
    `A a
  | Obj ->
    let o = obj_iter doc va in
    let o = Array.fold_right o ~init:[] ~f:(fun (k, va) a -> (k, { doc; va }) :: a) in
    `O o
;;

external read_file : string -> int -> doc = "ml_yyjson_read_file"
external read_opts : Bigstringaf.t -> int -> int -> int -> doc = "ml_yyjson_read_opts"

(* the binding can handle both string and bigstring! *)
external read_opts_string : string -> int -> int -> int -> doc = "ml_yyjson_read_opts"

let of_file ?(flags = []) fn = read_file fn (ReadFlag.to_int flags)

let of_bigstring ?(flags = []) ?(pos = 0) ?len src =
  let buflen = Bigstringaf.length src in
  let len = Option.value len ~default:(buflen - pos) in
  if pos < 0 || len < 0 || pos + len > buflen then invalid_arg "of_bigstring";
  read_opts src pos len (ReadFlag.to_int flags)
;;

let of_string ?(flags = []) ?(pos = 0) ?len src =
  let buflen = String.length src in
  let len = Option.value len ~default:(buflen - pos) in
  if pos < 0 || len < 0 || pos + len > buflen then invalid_arg "of_string";
  read_opts_string src pos len (ReadFlag.to_int flags)
;;

external write_opts_unsafe : doc -> int -> string = "ml_yyjson_write_opts"
external write_opts_val_unsafe : doc -> va -> int -> string = "ml_yyjson_val_write_opts"
external write_file_unsafe : doc -> string -> int -> unit = "ml_yyjson_write_file"

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
  write_opts_val_unsafe doc va (WriteFlag.to_int flags)
;;

module Mutable = Mutable
