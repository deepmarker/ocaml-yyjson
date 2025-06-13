open StdLabels
module Alc = Alc
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

type value = va

external arr_iter : va -> va array = "ml_yyjson_array_iter"
external obj_iter : va -> (string * va) array = "ml_yyjson_obj_iter"
external get_type : va -> json_typ = "ml_yyjson_get_type" [@@noalloc]
external get_subtype : va -> json_subtyp = "ml_yyjson_get_subtype" [@@noalloc]
external _get_bool : va -> bool = "ml_yyjson_get_bool" [@@noalloc]
external get_int : va -> int = "ml_yyjson_get_int" [@@noalloc]
external get_sint : va -> int64 = "ml_yyjson_get_sint"
external get_float : va -> float = "ml_yyjson_get_real"
external get_string : va -> string = "ml_yyjson_get_str"

(* values created here have the same lifetime as doc. Make sure they
   are never GCed before doc in OCaml too. *)
let view v =
  match get_type v with
  | ErrInvalid -> assert false
  | Raw -> assert false
  | Null -> `Null
  | Bool ->
    `Bool
      (match get_subtype v with
       | NoneFalseUint -> false
       | _ -> true)
  | Num ->
    (match get_subtype v, Sys.word_size with
     | Real, _ -> `Float (get_float v)
     | _, 64 -> `Float (get_int v |> Int.to_float)
     | _ -> `Float (get_sint v |> Int64.to_float))
  | Str -> `String (get_string v)
  | Arr -> `A (arr_iter v |> Array.to_list)
  | Obj -> `O (obj_iter v |> Array.to_list)
;;

external read_file : string -> int -> Alc.alc option -> doc = "ml_yyjson_read_file"

external read_opts
  :  Bigstringaf.t
  -> int
  -> int
  -> int
  -> Alc.alc option
  -> doc
  = "ml_yyjson_read_opts"

(* the binding can handle both string and bigstring! *)
external read_opts_string
  :  string
  -> int
  -> int
  -> int
  -> Alc.alc option
  -> doc
  = "ml_yyjson_read_opts"

let of_file ?alc ?(flags = []) fn =
  read_file fn (ReadFlag.to_int flags) (Option.map Alc.alc alc)
;;

let of_bigstring ?alc ?(flags = []) ?(pos = 0) ?len src =
  let buflen = Bigstringaf.length src in
  let len = Option.value len ~default:(buflen - pos) in
  if pos < 0 || len < 0 || pos + len > buflen then invalid_arg "of_bigstring";
  read_opts src pos len (ReadFlag.to_int flags) (Option.map Alc.alc alc)
;;

let of_string ?alc ?(flags = []) ?(pos = 0) ?len src =
  let buflen = String.length src in
  let len = Option.value len ~default:(buflen - pos) in
  if pos < 0 || len < 0 || pos + len > buflen then invalid_arg "of_string";
  read_opts_string src pos len (ReadFlag.to_int flags) (Option.map Alc.alc alc)
;;

external write_opts
  :  doc
  -> int
  -> Alc.alc option
  -> Bigstringaf.t
  = "ml_yyjson_write_opts"

external write_file
  :  string
  -> doc
  -> int
  -> Alc.alc option
  -> unit
  = "ml_yyjson_write_file"

let to_file ?alc ?(flags = []) path doc =
  write_file path doc (WriteFlag.to_int flags) (Option.map Alc.alc alc)
;;

let to_bigstring ?alc ?(flags = []) doc =
  write_opts doc (WriteFlag.to_int flags) (Option.map Alc.alc alc)
;;

module Mutable = Mutable
