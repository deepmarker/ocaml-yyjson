open Common

(** Underlying memory. *)
type doc

(** Iterator on doc. *)
type va

(** [{doc; va}] required for ocplib-json-typed. *)
type value

val doc_of_value : value -> doc
val create : unit -> doc
val free : doc -> unit
val doc_set_root : doc -> va -> unit

(* Write functions *)

val to_file : ?flags:WriteFlag.t list -> doc -> string -> unit
val to_string : ?flags:WriteFlag.t list -> doc -> string

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
