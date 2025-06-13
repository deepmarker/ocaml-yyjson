type alc
type t

val create_dyn : unit -> t
val create_static : Bigstringaf.t -> t
val create_caml : unit -> t
val free : t -> unit
val alc : t -> alc
val free_buf : t -> Bigstringaf.t -> unit
