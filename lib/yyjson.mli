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

(** Raised by [view] on a value it cannot represent: a [Raw] value, produced
    by the [Number_as_raw] and [Bignum_as_raw] read flags, or an invalid
    one. *)
exception Unexpected_type of json_typ

(** [view value] is the generic JSON view used by [Json_encoding.Make].

    Numbers are returned as floats, so integers above 2^53 lose precision:
    use [int64_value] or [uint64_value] where exact integers matter. *)
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

(** Version of the shared library linked at run time. *)
val version : version lazy_t

(** Version of the [yyjson.h] this binding was compiled against. It must
    agree with [version]: a skew silently shifts flag values and struct
    layouts. *)
val compiled_version : version lazy_t
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

(** [int64_value value] returns an integral JSON number without converting it
    through a float. It is [None] for a real, and for an unsigned integer
    above [Int64.max_int] — use [uint64_value] for those. *)
val int64_value : value -> int64 option

(** [uint64_value value] returns the bit pattern of a non-negative integral
    JSON number. Values above [Int64.max_int] are returned negative and must
    be interpreted as unsigned. It is [None] for a real or a negative
    integer. *)
val uint64_value : value -> int64 option

(** [array_values value] returns the array elements without constructing a
    generic JSON view. *)
val array_values : value -> value array option

val of_file : ?flags:ReadFlag.t list -> string -> doc
val of_bigstring : ?flags:ReadFlag.t list -> ?pos:int -> ?len:int -> Bigstringaf.t -> doc
val of_string : ?flags:ReadFlag.t list -> ?pos:int -> ?len:int -> string -> doc
val to_file : ?flags:WriteFlag.t list -> doc -> string -> unit
val to_string : ?flags:WriteFlag.t list -> doc -> string
val to_string_val : ?flags:WriteFlag.t list -> doc -> va -> string

module Mutable = Mutable
