LEX=ocamllex
YACC=ocamlyacc
COMP=ocamlc

OPT = -c
OUTPUT=-o

parser:
	$(YACC) parser.mly

all:
	$(LEX) scanner.mll	# generate scanner.ml
	$(YACC) parser.mly	# generate parser.ml and parser.mli
	$(COMP) $(OPT) ast.mli	# compile AST types
	$(COMP) $(OPT) parser.mli	# compile parser types
	$(COMP) $(OPT) scanner.ml   # compile the scanner
	$(COMP) $(OPT) parser.ml   # compile the parser
	$(COMP) $(OPT) interpreter.ml # compile the interpreter
	$(COMP) $(OUTPUT) interpreter parser.cmo scanner.cmo interpreter.cmo # generate interpreter 

.PHONY:	clean

clean:
	rm -rf *.out scanner.ml parser.ml parser.mli *.cmo *.cmi interpreter


