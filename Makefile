all: interface code

interface: graph_coloring.mli
	ocamlc -c $<

code: graph_coloring.ml
	ocamlc $<
