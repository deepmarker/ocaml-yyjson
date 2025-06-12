type _ alc
type _ t

val create_dyn : unit -> [ `Dyn ] t
val create_static : Bigstringaf.t -> [ `Static ] t
val free : [ `Dyn ] t -> unit
val alc : 'a t -> 'a alc
val free_buf : _ t -> Bigstringaf.t -> unit
