open Common

(* Hold the memory *)
type doc

(* Yyjson view of some part of a doc. *)
type va
type value = va

val create : ?alc:Alc.t -> unit -> doc
val free : doc -> unit
val new_doc : ?alc:Alc.t -> unit -> doc lazy_t
val current_doc : unit -> doc lazy_t
val doc_set_root : doc -> va -> unit

(* Write functions *)

val to_file : ?alc:Alc.t -> ?flags:WriteFlag.t list -> string -> doc -> unit
val to_string : ?alc:Alc.t -> ?flags:WriteFlag.t list -> doc -> string

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
