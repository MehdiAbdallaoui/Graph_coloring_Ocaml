all: graph_coloring.cmi graph_coloring.cmo

%.cmo: %.ml
	ocamlc $<

%.cmi: %.mli
	ocamlc -c $<

graph_coloring.cmi: graph_coloring.mli
	ocamlc -c $<

graph_coloring.cmo: graph_coloring.ml
	ocamlc $<
