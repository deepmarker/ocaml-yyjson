open Common

(* Hold the memory *)
type doc

(* Yyjson view of some part of a doc. *)
type va
type value = va

val create : ?alc:_ Alc.t -> unit -> doc
val free : doc -> unit

(* Create a new doc. The former doc must be closed with [free] by caller! *)
val new_doc : ?alc:_ Alc.t -> unit -> doc lazy_t
val current_doc : unit -> doc lazy_t
val doc_set_root : doc -> va -> unit

(* Write functions *)

val to_file : ?alc:_ Alc.t -> ?flags:WriteFlag.t list -> string -> doc -> unit
val to_bigstring : ?alc:_ Alc.t -> ?flags:WriteFlag.t list -> doc -> Bigstringaf.t

val view
  :  value
  -> [ `A of value list
     | `Bool of bool
     | `Float of float
     | `Null
     | `O of (string * value) list
     | `String of string
     ]

val repr
  :  [ `A of value list
     | `Bool of bool
     | `Float of float
     | `Null
     | `O of (string * value) list
     | `String of string
     ]
  -> value
