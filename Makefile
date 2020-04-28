all: graph_coloring_interface graph_coloring projet_exec

%.cmo: %.ml
	@echo "\n"
	@echo "********************************"
	@echo "* Building .cmo file from .ml  *"
	@echo "********************************"
	ocamlc -c $<

%.cmi: %.mli
	@echo "\n"
	@echo "********************************"
	@echo "* Building .cmi file from .mli *"
	@echo "********************************"
	ocamlc -c $<

graph_coloring_interface: graph_coloring_interface.cmi

graph_coloring: graph_coloring.cmo

projet_exec: graph_coloring.ml graph_coloring_interface.mli
	@echo "\n"
	@echo "********************************"
	@echo "*          Exec file           *"
	@echo "********************************"
	ocamlc $^ -o $@
