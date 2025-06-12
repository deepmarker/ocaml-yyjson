(* This module exists mainly so that the backing array of alc does not
   get GCed. *)

type _ alc

type _ t =
  | Dyn : [ `Dyn ] alc -> [ `Dyn ] t
  | Static :
      { b : Bigstringaf.t
      ; alc : [ `Static ] alc
      }
      -> [ `Static ] t

external alc_init : Bigstringaf.t -> [ `Static ] alc = "ml_yyjson_alc_pool_init"
external alc_dyn_new : unit -> [ `Dyn ] alc = "ml_yyjson_alc_dyn_new"
external alc_dyn_free : [ `Dyn ] alc -> unit = "ml_yyjson_alc_dyn_free"
external alc_free_buf : _ alc -> Bigstringaf.t -> unit = "ml_yyjson_alc_free" [@@noalloc]

let create_dyn () = Dyn (alc_dyn_new ())
let create_static b = Static { b; alc = alc_init b }

let free = function
  | Dyn a -> alc_dyn_free a
;;

let alc : type a. a t -> a alc = function
  | Dyn a -> a
  | Static { alc; _ } -> alc
;;

let free_buf t buf =
  let alc = alc t in
  alc_free_buf alc buf
;;
