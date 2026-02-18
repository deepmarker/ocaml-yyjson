all:
	dune build @install @runtest

dep:
	dune install base sexplib alcotest json-data-enoding ppx_sexp_conv ppx_compare
