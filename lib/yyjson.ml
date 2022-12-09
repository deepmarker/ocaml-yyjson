open StdLabels

type read_flag =
  | No_read_flag
  | Insitu
  | Stop_when_done
  | Allow_trailing_commas
  | Allow_inf_and_nan
  | Number_as_raw
  | Allow_invalid_unicode

type read_code =
  | Success
  | Invalid_param
  | Memory_alloc
  | Empty_content
  | Unexpected_content
  | Unexpected_end
  | Unexpected_char
  | Json_structure
  | Invalid_comment
  | Invalid_number
  | Invalid_string
  | Literal
  | File_open
  | File_read

type doc
type va
type mutdoc
type mutval

external create_mut_doc : Bigstringaf.t -> mutdoc = "ml_yyjson_mut_doc_new"
external free_mut_doc : mutdoc -> unit = "ml_yyjson_mut_doc_free" [@@noalloc]
external free_doc : doc -> unit = "ml_yyjson_doc_free" [@@noalloc]
external mut_null : mutdoc -> mutval = "ml_yyjson_mut_null"
external mut_bool : mutdoc -> bool -> mutval = "ml_yyjson_mut_bool"
external mut_float : mutdoc -> float -> mutval = "ml_yyjson_mut_real"
external mut_string : mutdoc -> string -> mutval = "ml_yyjson_mut_strcpy"
external doc_get_root : doc -> va = "ml_yyjson_doc_get_root"

external mut_doc_set_root : mutdoc -> mutval -> unit = "ml_yyjson_mut_doc_set_root"
  [@@noalloc]

external create_mut_obj : mutdoc -> mutval = "ml_yyjson_mut_obj"

external mut_obj_add : mutval -> mutval -> mutval -> bool = "ml_yyjson_mut_obj_add"
  [@@noalloc]

external create_mut_arr : mutdoc -> mutval = "ml_yyjson_mut_arr"

external mut_arr_add_val : mutval -> mutval -> bool = "ml_yyjson_mut_arr_add_val"
  [@@noalloc]

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

type arr_iter
type obj_iter
type mut_arr_iter
type mut_obj_iter

(* Store buffer in there to have it GCed at the same time as the rest. *)
type value =
  | Mutable :
      { doc : mutdoc
      ; v : mutval
      ; buf : Bigstringaf.t
      }
      -> value
  | Immutable :
      { doc : doc
      ; v : va
      ; buf : Bigstringaf.t
      }
      -> value

let value_of_doc buf doc = Immutable { doc; v = doc_get_root doc; buf }

let free_value = function
  | Mutable { doc; _ } -> free_mut_doc doc
  | Immutable { doc; _ } -> free_doc doc
;;

let repr_uid = Json_repr.repr_uid ()

let find_doc = function
  | `A (Mutable { doc; _ } :: _) -> Some doc
  | `O ((_, Mutable { doc; _ }) :: _) -> Some doc
  | #Json_repr.view -> None
;;

let repr v =
  let repr doc = function
    | `Null ->
      let v = mut_null doc in
      v
    | `Bool b ->
      let v = mut_bool doc b in
      v
    | `Float f ->
      let v = mut_float doc f in
      v
    | `String s ->
      let v = mut_string doc s in
      v
    | `A xs ->
      let arr = create_mut_arr doc in
      List.iter xs ~f:(function
        | Immutable _ -> assert false
        | Mutable { doc = _; v } ->
          let added = mut_arr_add_val arr v in
          assert added);
      arr
    | `O xs ->
      let obj = create_mut_obj doc in
      List.iter xs ~f:(fun (k, v) ->
        match v with
        | Immutable _ -> assert false
        | Mutable { doc = _; v } ->
          let k = mut_string doc k in
          let added = mut_obj_add obj k v in
          assert added);
      obj
  in
  (* If we cannot find doc: immediate value. Worse case scenario in
     terms of size, string. *)
  let len =
    match v with
    | `String s -> 10 + String.length s
    | #Json_repr.view -> assert false
  in
  let buf = Bigstringaf.create len in
  let doc = Option.value ~default:(create_mut_doc buf) (find_doc v) in
  let v = repr doc v in
  mut_doc_set_root doc v;
  Mutable { buf; doc; v }
;;

type json_typ =
  | ErrInvalid
  | Raw
  | Null
  | Bool
  | Num
  | Str
  | Arr
  | Obj

type json_subtyp =
  | NoneFalseUint
  | TrueSint
  | Real

external get_type : va -> json_typ = "ml_yyjson_get_type" [@@noalloc]
external get_subtype : va -> json_subtyp = "ml_yyjson_get_subtype" [@@noalloc]
external get_bool : va -> bool = "ml_yyjson_get_bool" [@@noalloc]
external get_int : va -> int = "ml_yyjson_get_int" [@@noalloc]
external get_sint : va -> int64 = "ml_yyjson_get_sint"
external get_float : va -> float = "ml_yyjson_get_real"
external get_string : va -> string = "ml_yyjson_get_str"
external arr_iter_init : va -> arr_iter = "ml_yyjson_arr_iter_init"
external arr_iter_next : arr_iter -> va = "ml_yyjson_arr_iter_next"
external obj_iter_init : va -> obj_iter = "ml_yyjson_obj_iter_init"
external obj_iter_next : obj_iter -> va = "ml_yyjson_obj_iter_next"
external obj_iter_get_val : va -> va = "ml_yyjson_obj_iter_get_val"
external mut_get_type : mutval -> json_typ = "ml_yyjson_mut_get_type" [@@noalloc]
external mut_get_subtype : mutval -> json_subtyp = "ml_yyjson_mut_get_subtype" [@@noalloc]
external mut_get_bool : mutval -> bool = "ml_yyjson_mut_get_bool" [@@noalloc]
external mut_get_int : mutval -> int = "ml_yyjson_mut_get_int" [@@noalloc]
external mut_get_sint : mutval -> int64 = "ml_yyjson_mut_get_sint"
external mut_get_float : mutval -> float = "ml_yyjson_mut_get_real"
external mut_get_string : mutval -> string = "ml_yyjson_mut_get_str"
external mut_arr_iter_init : mutval -> mut_arr_iter = "ml_yyjson_mut_arr_iter_init"
external mut_arr_iter_next : mut_arr_iter -> mutval = "ml_yyjson_mut_arr_iter_next"
external mut_obj_iter_init : mutval -> mut_obj_iter = "ml_yyjson_mut_obj_iter_init"
external mut_obj_iter_next : mut_obj_iter -> mutval = "ml_yyjson_mut_obj_iter_next"
external mut_obj_iter_get_val : mutval -> mutval = "ml_yyjson_mut_obj_iter_get_val"

let view v =
  match v with
  | Mutable { buf; v; doc } ->
    (match mut_get_type v with
     | ErrInvalid -> assert false
     | Raw -> assert false
     | Null -> `Null
     | Bool ->
       `Bool
         (match mut_get_subtype v with
          | NoneFalseUint -> false
          | _ -> true)
     | Num ->
       (match mut_get_subtype v, Sys.word_size with
        | Real, _ -> `Float (mut_get_float v)
        | _, 64 -> `Float (mut_get_int v |> Int.to_float)
        | _ -> `Float (mut_get_sint v |> Int64.to_float))
     | Str -> `String (mut_get_string v)
     | Arr ->
       let it = mut_arr_iter_init v in
       let rec loop a =
         match mut_arr_iter_next it with
         | exception _ -> List.rev a
         | v -> loop (Mutable { buf; v; doc } :: a)
       in
       `A (loop [])
     | Obj ->
       let it = mut_obj_iter_init v in
       let rec loop a =
         match mut_obj_iter_next it with
         | exception _ -> List.rev a
         | k ->
           let v = mut_obj_iter_get_val k in
           loop ((mut_get_string k, Mutable { buf; v; doc }) :: a)
       in
       `O (loop []))
  | Immutable { buf; v; doc } ->
    (match get_type v with
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
     | Arr ->
       let it = arr_iter_init v in
       let rec loop a =
         match arr_iter_next it with
         | exception _ -> List.rev a
         | v -> loop (Immutable { buf; v; doc } :: a)
       in
       `A (loop [])
     | Obj ->
       let it = obj_iter_init v in
       let rec loop a =
         match obj_iter_next it with
         | exception _ -> List.rev a
         | k ->
           let v = obj_iter_get_val k in
           loop ((get_string k, Immutable { buf; v; doc }) :: a)
       in
       `O (loop []))
;;

external read_file : string -> int -> Bigstringaf.t -> doc = "ml_yyjson_read_file"

external read_opts
  :  Bigstringaf.t
  -> int
  -> int
  -> int
  -> Bigstringaf.t
  -> doc
  = "ml_yyjson_read_opts"

external read_opts_string
  :  string
  -> int
  -> int
  -> int
  -> Bigstringaf.t
  -> doc
  = "ml_yyjson_read_opts_string"

let int_of_flags = List.fold_left ~init:0 ~f:(fun a (x : read_flag) -> a lor Obj.magic x)
let of_file ?(flags = []) ~buf fn = read_file fn (int_of_flags flags) buf

let of_bigstring ?(flags = []) ?(pos = 0) ?len ~buf src =
  let buflen = Bigstringaf.length buf in
  let len = Option.value len ~default:(buflen - pos) in
  if pos < 0 || len < 0 || pos + len > buflen then invalid_arg "of_bigstring";
  read_opts src pos len (int_of_flags flags) buf
;;

let of_string ?(flags = []) ?(pos = 0) ?len ~buf src =
  let buflen = String.length src in
  let len = Option.value len ~default:(buflen - pos) in
  if pos < 0 || len < 0 || pos + len > buflen then invalid_arg "of_string";
  read_opts_string src pos len (int_of_flags flags) buf
;;

external write_opts
  :  doc
  -> int
  -> Bigstringaf.t
  -> Bigstringaf.t
  = "ml_yyjson_write_opts"

external write_file
  :  string
  -> doc
  -> int
  -> Bigstringaf.t
  -> unit
  = "ml_yyjson_write_file"

external mut_write_opts
  :  mutdoc
  -> int
  -> Bigstringaf.t
  -> Bigstringaf.t
  = "ml_yyjson_mut_write_opts"

external mut_write_file
  :  string
  -> mutdoc
  -> int
  -> Bigstringaf.t
  -> unit
  = "ml_yyjson_mut_write_file"

type write_flag =
  | NoWriteFlag
  | Pretty
  | EscapeUnicode
  | EscapeSlashes
  | AllowInfAndNan
  | InfAndNanAsNull
  | AllowInvalidUnicode

let int_of_flags = List.fold_left ~init:0 ~f:(fun a (x : write_flag) -> a lor Obj.magic x)

let to_file ?(flags = []) ~buf path mutdoc =
  mut_write_file path mutdoc (int_of_flags flags) buf
;;

let file_of_value ?(flags = []) ~buf path = function
  | Mutable { doc; _ } -> mut_write_file path doc (int_of_flags flags) buf
  | Immutable { doc; _ } -> write_file path doc (int_of_flags flags) buf
;;

let value_of_string ~buf s =
  let doc = of_string s ~buf in
  value_of_doc buf doc
;;

let bigstring_of_value ?(flags = []) ~buf = function
  | Mutable { doc; _ } -> mut_write_opts doc (int_of_flags flags) buf
  | Immutable { doc; _ } -> write_opts doc (int_of_flags flags) buf
;;
