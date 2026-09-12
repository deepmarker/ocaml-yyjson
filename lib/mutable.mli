open Common

(** Underlying memory. *)
type doc

(** Iterator on doc. *)
type va

(** Raised when calling a function on a doc that has been freed
    already. *)
exception Doc_is_null

(** [create ()] is a new mutable document. Its arena is reclaimed when the
    document is collected, or eagerly by [free]. *)
val create : unit -> doc

val free : doc -> unit
val doc_set_root : doc -> va -> unit

(** Create value functions *)

val null : doc -> va
val bool : doc -> bool -> va
val sint : doc -> int -> va
val float : doc -> float -> va
val string : doc -> string -> va

(** Create objects *)

val create_obj : doc -> va
val obj_add : doc -> va -> va -> va -> bool

(** Create arrays *)

val create_arr : doc -> va
val arr_add : doc -> va -> va -> bool

(** Get types *)

val get_type : doc -> va -> json_typ
val get_subtype : doc -> va -> json_subtyp

(** Get values *)

val get_int : doc -> va -> int
val get_sint : doc -> va -> int64
val get_float : doc -> va -> float

(** @raise Failure if the value is not a string. *)
val get_string : doc -> va -> string

(** Iter functions *)

val arr_iter : doc -> va -> va array
val obj_iter : doc -> va -> (string * va) array

(** Write functions *)

val to_file : ?flags:WriteFlag.t list -> doc -> string -> unit
val to_string : ?flags:WriteFlag.t list -> doc -> string
val to_string_val : ?flags:WriteFlag.t list -> doc -> va -> string
