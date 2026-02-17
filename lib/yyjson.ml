open StdLabels
include Common

type doc
type va

external free_doc : doc -> unit = "ml_yyjson_doc_free" [@@noalloc]
external doc_get_root : doc -> va = "ml_yyjson_doc_get_root"
external version : unit -> int = "ml_yyjson_version" [@@noalloc]

type version =
  { major : int
  ; minor : int
  ; patch : int
  }

let version =
  lazy
    (let v = version () in
     { major = v lsr 16; minor = (v lsr 8) land 0xff; patch = v land 0xff })
;;

type value =
  { doc : doc
  ; va : va
  }

let value_of_doc doc = { doc; va = doc_get_root doc }

external arr_iter : doc -> va -> va array = "ml_yyjson_array_iter"
external obj_iter : doc -> va -> (string * va) array = "ml_yyjson_obj_iter"
external get_type : doc -> va -> json_typ = "ml_yyjson_get_type"
external get_subtype : doc -> va -> json_subtyp = "ml_yyjson_get_subtype"
external _get_bool : doc -> va -> bool = "ml_yyjson_get_bool"
external get_int : doc -> va -> int = "ml_yyjson_get_sint_int"
external get_int64 : doc -> va -> int64 = "ml_yyjson_get_sint"
external get_float : doc -> va -> float = "ml_yyjson_get_real"
external get_string : doc -> va -> string = "ml_yyjson_get_str"

(* values created here have the same lifetime as doc. Make sure they
   are never GCed before doc in OCaml too. *)
let view { doc; va } =
  match get_type doc va with
  | ErrInvalid -> assert false
  | Raw -> assert false
  | Null -> `Null
  | Bool ->
    `Bool
      (match get_subtype doc va with
       | NoneFalseUint -> false
       | _ -> true)
  | Num ->
    (match get_subtype doc va, Sys.word_size with
     | Real, _ -> `Float (get_float doc va)
     | _, 64 -> `Float (get_int doc va |> Int.to_float)
     | _ -> `Float (get_int64 doc va |> Int64.to_float))
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

external write_opts : doc -> int -> string = "ml_yyjson_write_opts"
external write_file : doc -> string -> int -> unit = "ml_yyjson_write_file"

let to_file ?(flags = []) path doc = write_file path doc (WriteFlag.to_int flags)
let to_string ?(flags = []) doc = write_opts doc (WriteFlag.to_int flags)

module Mutable = Mutable
