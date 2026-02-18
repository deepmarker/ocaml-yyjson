open Base
open Sexplib.Std
open Yyjson
open Alcotest

let ver = [| 0; 12; 0 |]

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

module YYMut = Json_encoding.Make (struct
    include Yyjson.Mutable

    let repr_uid = Json_repr.repr_uid ()
  end)

let roundtrip doc enc =
  (* XXX *)
  let xx =
    try YY.destruct enc (value_of_doc doc) with
    | exn ->
      Stdlib.Format.printf "%a@." (Json_encoding.print_error ?print_unknown:None) exn;
      raise exn
  in
  Yyjson.Mutable.new_doc ();
  let v = YYMut.construct enc xx in
  let va_json = Mutable.to_string (Mutable.doc_of_value v) in
  let doc = of_string va_json in
  let yy =
    try YY.destruct enc (value_of_doc doc) with
    | exn ->
      Stdlib.Format.printf "%a@." (Json_encoding.print_error ?print_unknown:None) exn;
      raise exn
  in
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
  Yyjson.Mutable.new_doc ();
  let va = YYMut.construct enc v in
  let json = Mutable.to_string (Mutable.doc_of_value va) in
  let doc = of_string json in
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

module Snap = struct
  type price_level =
    { price : string
    ; quantity : string
    }
  [@@deriving sexp, equal]

  type depth_update =
    { e : string
    ; e_caps : int64
    ; t_caps : int64
    ; s : string
    ; u_caps : int64
    ; u : int64
    ; pu : int64
    ; b : price_level list
    ; a : price_level list
    }
  [@@deriving sexp, equal]

  type 'a gen =
    { stream : string
    ; data : 'a
    }
  [@@deriving sexp, equal]

  type t =
    { stream : string
    ; data : depth_update
    }
  [@@deriving sexp, equal]

  let pp ppf t = Sexp.pp ppf (sexp_of_t t)

  let price_level_enc =
    let open Json_encoding in
    conv
      (fun { price; quantity } -> price, quantity)
      (fun (price, quantity) -> { price; quantity })
      (tup2 string string)
  ;;

  let depth_update_enc =
    let open Json_encoding in
    conv
      (fun { e; e_caps; t_caps; s; u_caps; u; pu; b; a } ->
         e, e_caps, t_caps, s, u_caps, u, pu, b, a)
      (fun (e, e_caps, t_caps, s, u_caps, u, pu, b, a) ->
         { e; e_caps; t_caps; s; u_caps; u; pu; b; a })
      (obj9
         (req "e" string)
         (req "E" int53)
         (req "T" int53)
         (req "s" string)
         (req "U" int53)
         (req "u" int53)
         (req "pu" int53)
         (req "b" (list price_level_enc))
         (req "a" (list price_level_enc)))
  ;;

  let enc =
    let open Json_encoding in
    conv
      (fun { stream; data } -> stream, data)
      (fun (stream, data) -> { stream; data })
      (obj2 (req "stream" string) (req "data" depth_update_enc))
  ;;

  let enc_2step =
    let open Json_encoding in
    conv
      (fun ({ stream; data } : Json_repr.any gen) -> stream, data)
      (fun (stream, data) -> { stream; data })
      (obj2 (req "stream" string) (req "data" any_document))
  ;;

  let payload =
    {|{"stream":"suiusdt@depth@100ms","data":{"e":"depthUpdate","E":1769040135865,"T":1769040135864,"s":"SUIUSDT","U":9715935278415,"u":9715935286029,"pu":9715935278413,"b":[["1.469500","8017.6"],["1.472500","187.6"],["1.501200","913.4"],["1.501400","8452.7"],["1.503400","22152.2"],["1.504200","3642.8"],["1.504600","48241.4"],["1.505800","3114.4"],["1.506000","20476.8"],["1.506200","11153.1"],["1.506800","2999.4"],["1.507000","40386.9"],["1.507100","17308.0"],["1.507200","8137.6"],["1.507300","18741.0"],["1.508200","12539.5"],["1.508300","10016.0"],["1.508400","5890.3"],["1.508500","3508.2"],["1.508600","11535.4"],["1.508700","4634.4"],["1.508800","12832.2"],["1.508900","5528.3"],["1.509000","4586.9"],["1.509100","7098.2"],["1.509400","6382.4"],["1.509800","14048.2"],["1.510100","14417.1"],["1.510200","18240.5"],["1.510300","9189.3"],["1.510400","9708.7"],["1.510500","5556.1"],["1.510700","13314.1"],["1.510800","12106.8"],["1.510900","15119.1"],["1.511000","9965.2"],["1.511100","11233.8"],["1.511200","6884.4"],["1.511300","8104.2"],["1.511400","1866.1"],["1.511500","1438.4"],["1.511600","34.9"],["1.511700","122.0"],["1.511800","0.0"]],"a":[["1.511800","10318.0"],["1.511900","5968.1"],["1.512000","11042.8"],["1.512100","5779.0"],["1.512200","4961.6"],["1.512300","6253.8"],["1.512400","9525.0"],["1.512500","16309.7"],["1.512600","15049.4"],["1.512700","11611.9"],["1.512800","6056.6"],["1.512900","7038.3"],["1.513000","10370.5"],["1.513100","6513.9"],["1.513200","10294.2"],["1.513300","13154.4"],["1.513400","14817.2"],["1.513500","4753.4"],["1.514200","12788.6"],["1.514300","10145.4"],["1.515000","14376.7"],["1.515100","19069.8"],["1.515200","3454.4"],["1.515600","5793.8"],["1.515800","7500.2"],["1.516400","16264.0"],["1.517500","613.3"],["1.517700","6259.3"],["1.517900","4945.1"],["1.518400","4712.5"],["1.518900","8870.8"],["1.519000","8429.8"],["1.519100","38954.2"],["1.519400","7669.4"],["1.520100","3074.4"],["1.520300","2129.8"],["1.521200","4613.8"],["1.521300","6321.0"],["1.521500","10197.9"],["1.522400","972.4"],["1.522500","12.2"],["1.528200","1836.9"],["1.528300","151.8"],["1.534300","2444.2"],["1.536000","4397.3"],["1.543700","75998.6"],["1.546900","6.4"],["1.548700","714.8"]]}}|}
  ;;

  let payload2 =
    {|{"stream":"btcusdt@depth@0ms","data":{"e":"depthUpdate","E":1771241917587,"T":1771241917586,"s":"BTCUSDT","U":9922090280476,"u":9922090283949,"pu":9922090280150,"b":[["1000.00","33.811"],["5000.00","44.996"],["18115.80","0.176"],["18215.80","0.968"],["18315.80","0.132"],["18415.80","1.056"],["18515.80","0.968"],["33000.00","3.873"],["38415.80","0.168"],["38515.80","0.132"],["67737.50","0.011"],["67929.60","0.932"],["68343.00","0.003"],["68442.40","0.000"],["68442.50","1.140"],["68485.40","0.087"],["68564.30","0.024"],["68581.70","0.314"],["68615.80","0.828"]],"a":[["68636.30","0.008"],["68637.40","0.009"],["68657.60","0.112"],["68659.90","0.005"],["68785.50","0.796"],["69302.10","0.743"],["69302.50","0.007"],["69439.30","1.293"],["69645.60","0.000"],["69713.80","0.753"],["69748.10","9.018"]]}}|}
  ;;

  let payload3 =
    {|{"stream":"btcusdt@depth@0ms","data":{"e":"depthUpdate","E":1771241917899,"T":1771241917898,"s":"BTCUSDT","U":9922090297375,"u":9922090299939,"pu":9922090297219,"b":[["1000.00","33.811"],["5000.00","44.996"],["18115.80","0.154"],["18215.80","0.946"],["18315.80","0.242"],["18415.80","0.968"],["18515.80","0.990"],["38415.80","0.204"],["38515.80","0.096"],["66557.30","0.005"],["68551.40","0.172"],["68552.10","0.030"],["68568.00","0.019"],["68568.10","0.272"],["68575.70","0.002"],["68575.80","0.044"],["68580.00","0.028"],["68582.70","0.006"],["68590.00","3.466"],["68602.10","0.000"],["68604.90","0.186"],["68605.60","0.034"],["68612.60","0.000"],["68615.80","0.887"]],"a":[["68615.90","3.611"],["68617.20","0.002"],["68626.10","0.185"],["68626.80","0.252"],["68627.10","0.004"],["68635.20","0.034"],["68648.90","0.010"],["68650.50","1.614"],["68652.90","0.070"],["68653.60","0.232"],["68663.80","2.099"],["68664.00","0.010"],["68679.70","0.070"],["68680.30","0.185"],["69302.10","0.743"]]}}|}
  ;;

  let payload_v payload =
    let doc = Yyjson.of_string payload in
    let v = Yyjson.value_of_doc doc in
    YY.destruct enc v
  ;;

  let twostep n =
    test_case "2step" `Quick (fun () ->
      for _ = 0 to n - 1 do
        List.iter [ payload; payload2; payload3 ] ~f:(fun payload ->
          let doc = Yyjson.of_string payload in
          let v = Yyjson.value_of_doc doc in
          let first = YY.destruct enc_2step v in
          let _ =
            Json_encoding.destruct depth_update_enc (Json_repr.from_any first.data)
          in
          ())
      done)
  ;;
end

(* let product_testable = Alcotest.testable Product.pp Product.equal *)

let gen_int_arr n = Array.init n ~f:(fun _ -> 0)

let gen_string_arr n =
  Array.init n ~f:(fun _i -> Array.init 2 ~f:(fun _i -> String.make 10 ' '))
;;

let equal_int64 ints =
  test_case "test_int" `Quick (fun () ->
    List.iter ints ~f:(fun i_str ->
      let doc = Yyjson.of_string i_str in
      let v = Yyjson.value_of_doc doc in
      let open Json_encoding in
      let i' = YY.destruct int53 v in
      check int64 i_str (Int64.of_string i_str) i'))
;;

let basic =
  let open Json_encoding in
  [ test_case "version" `Quick version
  ; rdtrip ~n:1 "3" int Alcotest.int
  ; rdtrip "true" bool Alcotest.bool
  ; rdtrip "false" bool Alcotest.bool
  ; rdtrip "8499394102736" float (Alcotest.float 8499394102736.)
  ; rdtrip "8499394102736" int53 Alcotest.int64
  ; equal_int64 [ "8499394102736"; "8499394110956" ]
    (* ; rdtrip {|"machin"|} string Alcotest.string *)
  ; rdtrip "3.0" float (Alcotest.float 0.1)
  ; rdtrip "{}" unit Alcotest.unit
  ; rdtrip "[]" (array int) Alcotest.(array int)
  ; rdtrip {|{"a": 1}|} (obj1 (req "a" int)) Alcotest.int
  ; rdtrip "[1]" (array int) Alcotest.(array int)
  ; rdtrip ~n:1 "[1,2,3]" (array int) Alcotest.(array int)
  ; rdtrip
      {|{"a": 1, "b": "truc"}|}
      (obj2 (req "a" int) (req "b" string))
      Alcotest.(pair int string)
    (* ; rdtrip {|3|} int_or_string *)
    (* ; rdtrip {|"truc"|} int_or_string *)
  ; rdtrip {|{"a":1, "b":2, "c":3}|} (assoc int) Alcotest.(list (pair string int))
  ; rdtrip ~n:1 Snap.payload Snap.enc (module Snap)
  ; rdtrip ~n:1 Snap.payload2 Snap.enc (module Snap)
  ; rdtrip ~n:1 Snap.payload3 Snap.enc (module Snap)
  ; Snap.twostep 10000
    (* ; rdtrip_gen ~n:1000 Snap.enc (Snap.payload_v Snap.payload) *)
    (* ; rdtrip_gen (array int) (gen_int_arr 10000) *)
    (* ; rdtrip_gen (array (array string)) (gen_string_arr 5000) *)
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
