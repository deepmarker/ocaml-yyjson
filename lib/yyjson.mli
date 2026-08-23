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

(** [obj_get value key] is the object member named [key], or [None] if
    [value] is not an object or the member is absent. The returned value is
    valid only while the source document is alive. *)
val obj_get : value -> string -> value option

(** [obj_get_string value key] copies and returns the string member named
    [key]. It is [None] if the member is absent or is not a JSON string. *)
val obj_get_string : value -> string -> string option

(** [string_value value] copies and returns [value] when it is a JSON string. *)
val string_value : value -> string option

val of_file : ?flags:ReadFlag.t list -> string -> doc
val of_bigstring : ?flags:ReadFlag.t list -> ?pos:int -> ?len:int -> Bigstringaf.t -> doc
val of_string : ?flags:ReadFlag.t list -> ?pos:int -> ?len:int -> string -> doc
val to_file : ?flags:WriteFlag.t list -> doc -> string -> unit
val to_string : ?flags:WriteFlag.t list -> doc -> string
val to_string_val : ?flags:WriteFlag.t list -> doc -> va -> string

module Mutable = Mutable
