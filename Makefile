all: interface code test

interface: graph_coloring.mli
	ocamlc -c $<

code: graph_coloring.ml
	ocamlc $<

test: test_coloriage.ml
	ocaml test_coloriage.ml
