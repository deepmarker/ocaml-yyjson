open StdLabels
open Common

type doc
type va

external create : unit -> doc = "ml_yyjson_mut_doc_new"
external free : doc -> unit = "ml_yyjson_mut_doc_free" [@@noalloc]
external doc_set_root : doc -> va -> unit = "ml_yyjson_mut_doc_set_root" [@@noalloc]

(* atom functions functions *)
external null : doc -> va = "ml_yyjson_mut_null"
external bool : doc -> bool -> va = "ml_yyjson_mut_bool"
external _uint : doc -> int -> va = "ml_yyjson_mut_uint"
external sint : doc -> int -> va = "ml_yyjson_mut_sint"
external float : doc -> float -> va = "ml_yyjson_mut_real"
external string : doc -> string -> va = "ml_yyjson_mut_strcpy"

(* object functions *)
external create_obj : doc -> va = "ml_yyjson_mut_obj"
external obj_add : doc -> va -> va -> va -> bool = "ml_yyjson_mut_obj_add"

(* array functions *)
external create_arr : doc -> va = "ml_yyjson_mut_arr"
external arr_add_val : doc -> va -> va -> bool = "ml_yyjson_mut_arr_add_val"

(* get functions *)
external get_type : doc -> va -> json_typ = "ml_yyjson_mut_get_type"
external get_subtype : doc -> va -> json_subtyp = "ml_yyjson_mut_get_subtype"
external _get_bool : doc -> va -> bool = "ml_yyjson_mut_get_bool"
external get_int : doc -> va -> int = "ml_yyjson_mut_get_int"
external get_sint : doc -> va -> int64 = "ml_yyjson_mut_get_sint"
external get_float : doc -> va -> float = "ml_yyjson_mut_get_real"
external get_string : doc -> va -> string = "ml_yyjson_mut_get_str"

(* iterators *)
external arr_iter : doc -> va -> va array = "ml_yyjson_mut_array_iter"
external obj_iter : doc -> va -> (string * va) array = "ml_yyjson_mut_obj_iter"

(* write functions *)
external write_opts : doc -> int -> string = "ml_yyjson_mut_write_opts"
external write_file : doc -> string -> int -> unit = "ml_yyjson_mut_write_file"

type value =
  { doc : doc
  ; va : va
  }

let doc_of_value { doc; _ } = doc

let repr_aux doc v =
  match v with
  | `Null ->
    let va = null doc in
    { doc; va }
  | `Bool b ->
    let va = bool doc b in
    { doc; va }
  | `Float f ->
    let va =
      match Float.is_integer f with
      | true -> sint doc (Float.to_int f)
      | false -> float doc f
    in
    { doc; va }
  | `String s ->
    let va = string doc s in
    { doc; va }
  | `A xs ->
    let va = create_arr doc in
    List.iter xs ~f:(fun { doc; va = elt } ->
      let added = arr_add_val doc va elt in
      assert added);
    { doc; va }
  | `O xs ->
    let va = create_obj doc in
    List.iter xs ~f:(fun (k, { doc; va = elt }) ->
      let k = string doc k in
      let added = obj_add doc va k elt in
      assert added);
    { doc; va }
;;

let doc = ref (lazy (create ()))

let new_doc () =
  if Lazy.is_val !doc then free (Lazy.force !doc);
  doc := lazy (create ())
;;

let repr v =
  let doc = Lazy.force !doc in
  let value = repr_aux doc v in
  doc_set_root doc value.va;
  value
;;

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
     | _ -> `Float (get_sint doc va |> Int64.to_float))
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

let to_file ?(flags = []) doc path = write_file doc path (WriteFlag.to_int flags)
let to_string ?(flags = []) doc = write_opts doc (WriteFlag.to_int flags)
