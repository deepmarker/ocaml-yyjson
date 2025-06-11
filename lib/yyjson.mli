include module type of Common

type doc
type va
type value

val view
  :  value
  -> [ `A of value list
     | `Bool of bool
     | `Float of float
     | `Null
     | `O of (string * value) list
     | `String of string
     ]

type version =
  { major : int
  ; minor : int
  ; patch : int
  }

val version : version lazy_t
val free_doc : doc -> unit
val value_of_doc : doc -> value

(** Use it at most one time only in toplevel values *)
val free_value : value -> unit

val of_file : ?alc:_ alc -> ?flags:ReadFlag.t list -> string -> doc

val of_bigstring
  :  ?alc:_ alc
  -> ?flags:ReadFlag.t list
  -> ?pos:int
  -> ?len:int
  -> Bigstringaf.t
  -> doc

val of_string
  :  ?alc:_ alc
  -> ?flags:ReadFlag.t list
  -> ?pos:int
  -> ?len:int
  -> string
  -> doc

val file_of_value : ?alc:_ alc -> ?flags:WriteFlag.t list -> string -> value -> unit
val bigstring_of_value : ?alc:_ alc -> ?flags:WriteFlag.t list -> value -> Bigstringaf.t

module Mutable = Mutable
