.PHONY: all test clean deps

all:
	dune build @install @runtest

test:
	dune runtest

clean:
	dune clean

# Test-only dependencies; yyjson itself must be installed as a system
# library (headers + -lyyjson), which opam does not provide.
deps:
	opam install alcotest base sexplib json-data-encoding ppx_sexp_conv \
	  ppx_compare ezjsonm
