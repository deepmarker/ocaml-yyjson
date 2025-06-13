open StdLabels
open Common

type doc
type va

external create : Alc.alc option -> doc = "ml_yyjson_mut_doc_new"
external free : doc -> unit = "ml_yyjson_mut_doc_free" [@@noalloc]
external null : doc -> va = "ml_yyjson_mut_null"
external bool : doc -> bool -> va = "ml_yyjson_mut_bool"
external _uint : doc -> int -> va = "ml_yyjson_mut_uint"
external sint : doc -> int -> va = "ml_yyjson_mut_sint"
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
external arr_iter : va -> va array = "ml_yyjson_mut_array_iter"
external obj_iter : va -> (string * va) array = "ml_yyjson_mut_obj_iter"

let create ?alc () = create (Option.map Alc.alc alc)

external write_opts
  :  doc
  -> int
  -> Alc.alc option
  -> Bigstringaf.t
  = "ml_yyjson_mut_write_opts"

external write_file
  :  string
  -> doc
  -> int
  -> Alc.alc option
  -> unit
  = "ml_yyjson_mut_write_file"

type value = va

let doc = ref (lazy (create ()))

let new_doc ?alc () =
  doc := lazy (create ?alc ());
  !doc
;;

let current_doc () = !doc

let repr_aux doc = function
  | `Null ->
    let v = null doc in
    v
  | `Bool b ->
    let v = bool doc b in
    v
  | `Float f ->
    (match Float.is_integer f with
     | true -> sint doc (Float.to_int f)
     | false -> float doc f)
  | `String s ->
    let v = string doc s in
    v
  | `A xs ->
    let arr = create_arr doc in
    List.iter xs ~f:(fun v ->
      let added = arr_add_val arr v in
      assert added);
    arr
  | `O xs ->
    let obj = create_obj doc in
    List.iter xs ~f:(fun (k, v) ->
      match v with
      | v ->
        let k = string doc k in
        let added = obj_add obj k v in
        assert added);
    obj
;;

let repr v = repr_aux (Lazy.force !doc) v

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

let to_file ?alc ?(flags = []) path doc =
  write_file path doc (WriteFlag.to_int flags) (Option.map Alc.alc alc)
;;

let to_bigstring ?alc ?(flags = []) doc =
  write_opts doc (WriteFlag.to_int flags) (Option.map Alc.alc alc)
;;
