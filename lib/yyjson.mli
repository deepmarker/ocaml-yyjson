module Alc : module type of Alc
include module type of Common

type doc
type va
type value = va

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
val doc_get_root : doc -> value
val free_doc : doc -> unit
val of_file : ?alc:Alc.t -> ?flags:ReadFlag.t list -> string -> doc

val of_bigstring
  :  ?alc:Alc.t
  -> ?flags:ReadFlag.t list
  -> ?pos:int
  -> ?len:int
  -> Bigstringaf.t
  -> doc

val of_string
  :  ?alc:Alc.t
  -> ?flags:ReadFlag.t list
  -> ?pos:int
  -> ?len:int
  -> string
  -> doc

val to_file : ?alc:Alc.t -> ?flags:WriteFlag.t list -> string -> doc -> unit
val to_bigstring : ?alc:Alc.t -> ?flags:WriteFlag.t list -> doc -> Bigstringaf.t

module Mutable = Mutable
