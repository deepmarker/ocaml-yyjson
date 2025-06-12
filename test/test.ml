open Yyjson
open Alcotest

let ver = [| 0; 11; 1 |]

let version () =
  let x = Lazy.force version in
  check int "major" ver.(0) x.major;
  check int "minor" ver.(1) x.minor;
  check int "patch" ver.(2) x.patch
;;

module YY = Json_encoding.Make (struct
    include Yyjson

    let repr _ = assert false
    let repr_uid = Json_repr.repr_uid ()
  end)

let buf = Bigstringaf.create (1 lsl 20)
let alc = Alc.create_static buf

module YYMut = Json_encoding.Make (struct
    include Yyjson.Mutable

    let repr_uid = Json_repr.repr_uid ()
  end)

let roundtrip doc enc =
  (* Destruct from JSON *)
  let xx = YY.destruct enc (doc_get_root doc) in
  (* Create a new internal doc in the library. *)
  let mdoc = Mutable.new_doc ~alc () in
  (* Construct with Mut (uses memory), and set as document root. *)
  let v = YYMut.construct enc xx in
  Mutable.doc_set_root (Lazy.force mdoc) v;
  (* Serialize from Mut (uses memory) *)
  let va_json = Mutable.to_bigstring ~alc (Lazy.force mdoc) in
  (* starting from here we don't need mdoc anymore *)
  Mutable.free (Lazy.force mdoc);
  (* Read serialized value *)
  let doc = of_bigstring ~alc va_json in
  (* va_json not needed, need free. *)
  Alc.free_buf alc va_json;
  (* Destruct serialized JSON to value. *)
  let yy = YY.destruct enc (doc_get_root doc) in
  free_doc doc;
  xx, yy
;;

let rdtrip ?(n = 10000) str enc eq =
  let doc = of_string str in
  test_case str `Quick (fun () ->
    for _ = 0 to n - 1 do
      let xx, yy = roundtrip doc enc in
      check eq "" xx yy
    done)
;;

let rdtrip_gen ?(n = 100) enc v =
  let doc = Lazy.force (Mutable.current_doc ()) in
  let va = YYMut.construct enc v in
  Mutable.doc_set_root doc va;
  let bs = Mutable.to_bigstring doc in
  let doc = of_bigstring bs in
  test_case "gen" `Quick (fun () ->
    for _ = 0 to n - 1 do
      let _, _ = roundtrip doc enc in
      ()
    done)
;;

let rdtrip_name name str enc eq =
  let doc = of_string str in
  test_case name `Quick (fun () ->
    let xx, yy = roundtrip doc enc in
    check eq name xx yy)
;;

let rdtrip_file name str enc eq =
  let doc = of_file str in
  test_case name `Quick (fun () ->
    let xx, yy = roundtrip doc enc in
    check eq name xx yy)
;;

let int_or_string =
  let open Json_encoding in
  union
    [ case
        string
        (function
          | `String str -> Some str
          | `Int _ -> None)
        (fun str -> `String str)
    ; case
        int
        (function
          | `String _ -> None
          | `Int i -> Some i)
        (fun i -> `Int i)
    ]
;;

let der =
  {|{"exchange":"DER","symbol":"BTC-16DEC22-10000-C","product":{"listing":"2022-11-24T08:01:00.000000000Z","instrument":{"class":"option","underlying":{"exchange":"DER","symbol":"btc_usd"},"strike":10000,"put_or_call":"call","expiry":"2022-12-16T08:00:00.000000000Z"},"status":"active"},"tags":{"base":"BTC","quote":"USD"}}|}
;;

let future =
  {|{
        "description" : "1INCH Token Perpetual Futures",
        "enabled" : true,
        "expired" : false,
        "expiry" : null,
        "expiryDescription" : "Perpetual",
        "group" : "perpetual",
        "imfFactor" : 0.0005,
        "imfWeight" : 1,
        "moveStart" : null,
        "name" : "1INCH-PERP",
        "perpetual" : true,
        "positionLimitWeight" : 20,
        "postOnly" : false,
        "type" : "perpetual",
        "underlying" : "1INCH",
        "underlyingDescription" : "1INCH Token"
    }|}
;;

let product =
  {|{
    "baseCurrency" : null,
    "enabled" : true,
    "future" : {
        "description" : "1INCH Token Perpetual Futures",
        "enabled" : true,
        "expired" : false,
        "expiry" : null,
        "expiryDescription" : "Perpetual",
        "group" : "perpetual",
        "imfFactor" : 0.0005,
        "imfWeight" : 1,
        "moveStart" : null,
        "name" : "1INCH-PERP",
        "perpetual" : true,
        "positionLimitWeight" : 20,
        "postOnly" : false,
        "type" : "perpetual",
        "underlying" : "1INCH",
        "underlyingDescription" : "1INCH Token"
    },
    "highLeverageFeeExempt" : false,
    "largeOrderThreshold" : 350,
    "name" : "1INCH-PERP",
    "postOnly" : false,
    "priceIncrement" : 0.0001,
    "quoteCurrency" : null,
    "restricted" : false,
    "sizeIncrement" : 1,
    "type" : "future",
    "underlying" : "1INCH"
}|}
;;

(* let product_testable = Alcotest.testable Product.pp Product.equal *)

let gen_int_arr n = Array.init n (fun _ -> 0)

let basic =
  let open Json_encoding in
  [ test_case "version" `Quick version
  ; rdtrip "3" int Alcotest.int
  ; rdtrip "true" bool Alcotest.bool
  ; rdtrip "false" bool Alcotest.bool
  ; rdtrip "3.0" float (Alcotest.float 0.1)
  ; rdtrip "{}" unit Alcotest.unit
  ; rdtrip "[1,2,3]" (array int) Alcotest.(array int)
  ; rdtrip
      {|{"a": 1, "b": "truc"}|}
      (obj2 (req "a" int) (req "b" string))
      Alcotest.(pair int string)
    (* ; rdtrip {|3|} int_or_string *)
    (* ; rdtrip {|"truc"|} int_or_string *)
  ; rdtrip {|{"a":1, "b":2, "c":3}|} (assoc int) Alcotest.(list (pair string int))
  ; rdtrip_gen (array int) (gen_int_arr 10000)
    (* ; rdtrip *)
    (*     {|{"exchange":"CFX","symbol":"USDT-USD-REPO-LIN","product":{"instrument":{"class":"swap","kind":"repo"},"status":"active"},"tags":{"base":"USDT","quote":"USD"}}|} *)
    (*     Product.encoding *)
    (*     product_testable *)
    (* ; rdtrip_name "der" der Product.encoding product_testable *)
    (* ; rdtrip_name *)
    (*     "future" *)
    (*     future *)
    (*     Ftx_ws.Future.encoding *)
    (*     (Alcotest.testable (fun _ -> assert false) Ftx_ws.Future.equal) *)
    (* ; rdtrip_name "market" product Ftx_ws.Market.encoding *)
    (* ; rdtrip_file "market_file" "market.json" Ftx_ws.Market.encoding *)
    (* ; rdtrip_file "ftx" "../../../src/instrhandler/bin/ftx.json" Ftx_ws.markets_encoding *)
  ]
;;

let () = run "yyjson" [ "basic", basic ]
