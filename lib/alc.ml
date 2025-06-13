(* This module exists mainly so that the backing array of alc does not
   get GCed. *)

type alc

type t =
  | Caml of alc
  | Dyn of alc
  | Static of
      { b : Bigstringaf.t
      ; alc : alc
      }

external alc_caml : unit -> alc = "ml_yyjson_alc_caml"
external alc_init : Bigstringaf.t -> alc = "ml_yyjson_alc_pool_init"
external alc_dyn_new : unit -> alc = "ml_yyjson_alc_dyn_new"
external alc_dyn_free : alc -> unit = "ml_yyjson_alc_dyn_free"
external alc_free_buf : alc -> Bigstringaf.t -> unit = "ml_yyjson_alc_free" [@@noalloc]

let create_dyn () = Dyn (alc_dyn_new ())
let create_static b = Static { b; alc = alc_init b }
let create_caml () = Caml (alc_caml ())

let free = function
  | Dyn a -> alc_dyn_free a
  | _ -> ()
;;

let alc = function
  | Dyn a -> a
  | Caml a -> a
  | Static { alc; _ } -> alc
;;

let free_buf t buf =
  let alc = alc t in
  alc_free_buf alc buf
;;
