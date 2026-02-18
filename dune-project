(lang dune 3.5)

(name yyjson)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name yyjson)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
   ocaml
   dune
   bigstringaf
  (alcotest :with-test)
  (base :with-test)
  (ppx_sexp_conv :with-test)
  (ppx_compare :with-test)
  (json-data-encoding :with-test)
 )
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
