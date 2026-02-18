include module type of Common

(** Type of a doc, i.e. memory holding JSON data. *)
type doc

(** Type of a value, i.e. an iterator on [doc]. *)
type va

(** [value] is [{doc; va}]. Necessary for use with ocplib-json-typed. *)
type value =
  { doc : doc
  ; va : va
  }

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
val doc_get_root : doc -> va
val free_doc : doc -> unit
val value_of_doc : doc -> value
val doc_of_value : value -> doc
val of_file : ?flags:ReadFlag.t list -> string -> doc
val of_bigstring : ?flags:ReadFlag.t list -> ?pos:int -> ?len:int -> Bigstringaf.t -> doc
val of_string : ?flags:ReadFlag.t list -> ?pos:int -> ?len:int -> string -> doc
val to_file : ?flags:WriteFlag.t list -> doc -> string -> unit
val to_string : ?flags:WriteFlag.t list -> doc -> string
val to_string_val : ?flags:WriteFlag.t list -> doc -> va -> string

module Mutable = Mutable
