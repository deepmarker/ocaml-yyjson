open StdLabels
open Common

type doc
type va
type arr_iter
type obj_iter

external create : _ alc option -> doc = "ml_yyjson_mut_doc_new"
external free : doc -> unit = "ml_yyjson_mut_doc_free" [@@noalloc]
external null : doc -> va = "ml_yyjson_mut_null"
external bool : doc -> bool -> va = "ml_yyjson_mut_bool"
external float : doc -> float -> va = "ml_yyjson_mut_real"
external string : doc -> string -> va = "ml_yyjson_mut_strcpy"
external doc_set_root : doc -> va -> unit = "ml_yyjson_mut_doc_set_root" [@@noalloc]
external create_obj : doc -> va = "ml_yyjson_mut_obj"
external obj_add : va -> va -> va -> bool = "ml_yyjson_mut_obj_add" [@@noalloc]
external create_arr : doc -> va = "ml_yyjson_mut_arr"
external arr_add_val : va -> va -> bool = "ml_yyjson_mut_arr_add_val" [@@noalloc]
external get_type : va -> json_typ = "ml_yyjson_mut_get_type" [@@noalloc]
external get_subtype : va -> json_subtyp = "ml_yyjson_mut_get_subtype" [@@noalloc]
external _get_bool : va -> bool = "ml_yyjson_mut_get_bool" [@@noalloc]
external get_int : va -> int = "ml_yyjson_mut_get_int" [@@noalloc]
external get_sint : va -> int64 = "ml_yyjson_mut_get_sint"
external get_float : va -> float = "ml_yyjson_mut_get_real"
external get_string : va -> string = "ml_yyjson_mut_get_str"
external arr_iter_init : va -> arr_iter = "ml_yyjson_mut_arr_iter_init"
external arr_iter_next : arr_iter -> va = "ml_yyjson_mut_arr_iter_next"
external obj_iter_init : va -> obj_iter = "ml_yyjson_mut_obj_iter_init"
external obj_iter_next : obj_iter -> va = "ml_yyjson_mut_obj_iter_next"
external obj_iter_get_val : va -> va = "ml_yyjson_mut_obj_iter_get_val"

external write_opts
  :  doc
  -> int
  -> _ alc option
  -> Bigstringaf.t
  = "ml_yyjson_mut_write_opts"

external write_file
  :  string
  -> doc
  -> int
  -> _ alc option
  -> unit
  = "ml_yyjson_mut_write_file"

type value =
  { doc : doc
  ; v : va
  }

let free_value { doc; _ } = free doc

let find_doc = function
  | `A ({ doc; _ } :: _) -> Some doc
  | `O ((_, { doc; _ }) :: _) -> Some doc
  | _ -> None
;;

let repr_aux doc = function
  | `Null ->
    let v = null doc in
    v
  | `Bool b ->
    let v = bool doc b in
    v
  | `Float f ->
    let v = float doc f in
    v
  | `String s ->
    let v = string doc s in
    v
  | `A xs ->
    let arr = create_arr doc in
    List.iter xs ~f:(fun { doc = _; v } ->
      let added = arr_add_val arr v in
      assert added);
    arr
  | `O xs ->
    let obj = create_obj doc in
    List.iter xs ~f:(fun (k, v) ->
      match v with
      | { doc = _; v } ->
        let k = string doc k in
        let added = obj_add obj k v in
        assert added);
    obj
;;

let repr alc v =
  let doc = Option.value ~default:(create alc) (find_doc v) in
  let v = repr_aux doc v in
  doc_set_root doc v;
  { doc; v }
;;

let view { v; doc } =
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
  | Arr ->
    let it = arr_iter_init v in
    let rec loop a =
      match arr_iter_next it with
      | exception _ -> List.rev a
      | v -> loop ({ v; doc } :: a)
    in
    `A (loop [])
  | Obj ->
    let it = obj_iter_init v in
    let rec loop a =
      match obj_iter_next it with
      | exception _ -> List.rev a
      | k ->
        let v = obj_iter_get_val k in
        loop ((get_string k, { v; doc }) :: a)
    in
    `O (loop [])
;;

let to_file ?alc ?(flags = []) path doc = write_file path doc (WriteFlag.to_int flags) alc

let write_value ?alc ?(flags = []) path { doc; _ } =
  write_file path doc (WriteFlag.to_int flags) alc
;;

let bigstring_of_value ?alc ?(flags = []) { doc; _ } =
  write_opts doc (WriteFlag.to_int flags) alc
;;
