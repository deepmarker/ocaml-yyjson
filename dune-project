(lang dune 3.5)

(name yyjson)

(generate_opam_files true)

(source
 (github deepmarker/ocaml-yyjson))

(authors "Vincent Bernardoff <vbmithr@gmail.com>")

(maintainers "Vincent Bernardoff <vbmithr@gmail.com>")

(license ISC)

(documentation https://github.com/deepmarker/ocaml-yyjson)

(package
 (name yyjson)
 (synopsis "OCaml bindings to the yyjson C JSON parser")
 (description
  "Bindings to yyjson, a fast C JSON parser. Values are unboxed pointers into the parsed document, so reading fields allocates nothing beyond what is extracted. Provides direct accessors for reading individual members, exact 64-bit integer accessors, and a generic Json_repr-compatible view for json-data-encoding interop. Requires yyjson >= 0.13.0 as a system library.")
 (depends
  ocaml
  dune
  bigstringaf
  (alcotest :with-test)
  (base :with-test)
  (sexplib :with-test)
  (json-data-encoding :with-test)
  (ppx_sexp_conv :with-test)
  (ppx_compare :with-test)
  (ezjsonm :with-test))
 (tags
  ("json" "yyjson" "parser" "bindings")))
