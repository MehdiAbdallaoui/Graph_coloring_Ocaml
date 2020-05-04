all: graph_coloring_interface graph_coloring lexer_exec projet_exec test_lexer_exec

%.cmo: %.ml
	@echo "\n"
	@echo "********************************"
	@echo "*      Compiling .ml files     *"
	@echo "********************************"
	ocamlc $<

%.cmi: %.mli
	@echo "\n"
	@echo "********************************"
	@echo "*     Compiling .mli files     *"
	@echo "********************************"
	ocamlc -c $<

%.ml: %.mll
	@echo "\n"
	@echo "********************************"
	@echo "* Building .ml file from .mll  *"
	@echo "********************************"
	ocamllex $<

graph_coloring_interface: graph_coloring_interface.cmi

graph_coloring: graph_coloring.cmo

lexer_exec: projet_lexer.ml
	@echo "\n"
	@echo "**********************************"
	@echo "* Building the exec of the lexer *"
	@echo "**********************************"
	ocamlc $< -o $@

test_lexer_exec: lexer_exec dot_file_input.dot
	@echo "\n"
	@echo "**********************************"
	@echo "* 	Testing the lexer	 *"
	@echo "**********************************"
	./lexer_exec < dot_file_input.dot


projet_exec: graph_coloring.ml graph_coloring_interface.mli
	@echo "\n"
	@echo "********************************"
	@echo "*          Exec file           *"
	@echo "********************************"
	ocamlc $^ -o $@
