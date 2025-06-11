open Common

(* Hold the memory *)
type doc

(* Yyjson view of some part of a doc. *)
type va

(* Internally, a doc and a va. *)
type value

(* Free the doc inside the value. All values depending on the same doc
   will be invalidated. *)
val free_value : value -> unit
val to_file : ?alc:_ alc -> ?flags:WriteFlag.t list -> string -> doc -> unit
val bigstring_of_value : ?alc:_ alc -> ?flags:WriteFlag.t list -> value -> Bigstringaf.t
val write_value : ?alc:_ alc -> ?flags:WriteFlag.t list -> string -> value -> unit

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
  :  _ alc option
  -> [ `A of value list
     | `Bool of bool
     | `Float of float
     | `Null
     | `O of (string * value) list
     | `String of string
     ]
  -> value
