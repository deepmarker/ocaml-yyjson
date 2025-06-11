open Yyjson
open Alcotest

let version () =
  let x = Lazy.force version in
  check int "major" 0 x.major;
  check int "minor" 11 x.minor;
  check int "patch" 1 x.patch
;;

module EncodingYY = Json_encoding.Make (struct
    include Yyjson

    let repr _ = assert false
    let repr_uid = Json_repr.repr_uid ()
  end)

let buf = Bigstringaf.create (1 lsl 20)

module EncodingYYMut = Json_encoding.Make (struct
    include Yyjson.Mutable

    let alc = alc_init buf
    let repr = repr (Some alc)
    let repr_uid = Json_repr.repr_uid ()
  end)

let alc = alc_init buf

(* Toplevel doc will be freed *)
let roundtrip doc enc testable () =
  let xx = EncodingYY.destruct enc (value_of_doc doc) in
  let v = EncodingYYMut.construct enc xx in
  let va_json = Mutable.bigstring_of_value ~alc v in
  (* Printf.printf "%s\n" (Bigstringaf.to_string va_json); *)
  let doc = of_bigstring ~alc va_json in
  let yy = EncodingYY.destruct enc (value_of_doc doc) in
  free_doc doc;
  check testable "" xx yy
;;

let rdtrip str enc eq =
  let doc = of_string ~alc str in
  test_case str `Quick (fun () -> roundtrip doc enc eq ())
;;

let rdtrip_name name str enc eq =
  let doc = of_string ~alc str in
  test_case name `Quick (fun () -> roundtrip doc enc eq ())
;;

let rdtrip_file name str enc eq =
  let doc = of_file ~alc str in
  test_case name `Quick (fun () -> roundtrip doc enc eq ())
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
