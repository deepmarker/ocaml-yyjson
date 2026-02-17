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

external write_opts : doc -> int -> Alc.alc option -> string = "ml_yyjson_mut_write_opts"

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

let to_string ?alc ?(flags = []) doc =
  write_opts doc (WriteFlag.to_int flags) (Option.map Alc.alc alc)
;;

(* Do you understand why I coded this doc / new_doc / current_doc
   thing here? In the context of this yyjson mutable binding. I did
   not remember and it seems useless/fishy. *)

(* Looking at this code, I can see what's happening but I'm not
   entirely sure of the intended use case. *)

(* The `doc / new_doc / current_doc` pattern creates a **global
   mutable reference** to a lazy document. Here's what it does: *)

(* 1. `doc` holds a lazy document that's only created when forced *)
(* 2. `new_doc()` creates a new lazy document and stores it in the global `doc` ref *)
(* 3. `current_doc()` returns the current lazy document *)

(* **Why this might have been coded:** *)

(* In the yyjson mutable API, every value (`va`) must be created with
   a `doc` parameter - it ties the lifetime of values to their
   document. This pattern could be an attempt to provide a "default"
   document for convenience, so users don't have to thread a `doc`
   through all their code. *)

(* **Why it seems fishy/useless:** *)

(* 1. **Unsafe with concurrency** - global mutable state *)

(* 2. **The lazy wrapper seems pointless** - you're wrapping a `doc`
   in `lazy` but immediately forcing it in most use cases *)

(* 3. **Memory management unclear** - when does the old doc get freed?
   The `free` function exists but there's no cleanup when calling
   `new_doc()` *)

(* 4. **Not actually used** - none of your exported functions
   reference `current_doc()`, so users still need to pass `doc`
   explicitly anyway *)

(* **My guess:** This was either an incomplete experiment with an
   implicit context pattern, or leftover from refactoring. You
   probably realized users need explicit control over document
   lifetime and abandoned this approach, but forgot to remove it. *)

(* Probably because the same doc can be doc_set_root multiple times so
   it is useless to create one new each time? Is it the case? *)

(* **Yes, exactly!** That's the key insight I missed. *)

(* Looking at the yyjson mutable API: *)

(* - A `doc` is like a memory arena/allocator for JSON values *)

(* - You can call `doc_set_root` multiple times on the same `doc` to
   build different JSON trees *)

(* - Each `va` value created with `create_obj`, `create_arr`,
   `string`, etc. is allocated from the `doc` *)

(* So the pattern makes sense as: *)
(* 1. **Reuse the same document allocator** across multiple JSON constructions *)
(* 2. **Avoid overhead** of creating/destroying docs repeatedly *)
(* 3. The lazy wrapper ensures it's only created once when first needed *)

(* **However**, it's still problematic because: *)
(* - Without calling `free`, you'll leak memory from previous trees when you `doc_set_root` again (unless yyjson reuses that memory internally) *)
(* - The global mutable state means you can't safely build multiple independent JSON trees concurrently *)
(* - Users still need explicit `doc` parameters in all the constructor functions *)

(* So while the "reuse doc" intuition is correct, this particular
   implementation with a global lazy ref doesn't fully solve the
   problem. You'd probably want either: *)

(* - Explicit document management (create, use, free), or *)
(* - A better pooling/arena mechanism *)

(* Ok, giving more context, I think I was not able to change the
   signatures of view and repr so I had to hide the doc somewhere? *)

(* **Ah! That makes perfect sense now!** *)

(* Looking at the signatures: *)

(* ```ocaml *)
(* val repr : [< `Null | `Bool of bool | `Float of float | `String of string  *)
(*            | `A of va list | `O of (string * va) list ] -> va *)

(* val view : va -> [> `Null | `Bool of bool | `Float of float | `String of string *)
(*            | `A of va list | `O of (string * va) list ] *)
(* ``` *)

(* These functions need a `doc` to work (especially `repr` which
   creates values), but they don't take one as a parameter. So you
   needed to: *)

(* 1. **Hide the `doc` dependency** to match an existing interface
   (probably defined in a signature file or to match some
   abstraction) *)

(* 2. **Store it globally** so `repr` can access it without changing
   the function signature *)

(* This explains: *)
(* - Why it's lazy (create on first use) *)
(* - Why `new_doc()` exists (let users replace the arena if needed) *)

(* - Why it seems "fishy" (because global mutable state *is* fishy,
   but you were constrained) *)

(* **This is actually a reasonable workaround** when you're
   implementing an interface that doesn't thread through the necessary
   context. The alternative would be to use an explicit module functor
   or require users to call something like `with_doc doc (fun () ->
   repr ...)`, which might be more complex. *)

(* The only risk is if someone forgets that multiple `repr` calls
   share the same underlying document arena. *)
