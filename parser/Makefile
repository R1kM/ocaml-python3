CMO=  lexer.cmo parser.cmo main.cmo 
GENERATED=lexer.ml parser.mli parser.ml

CFLAGS=-annot -g
COMPILER=ocamlc str.cma $(CFLAGS)

LFLAGS=
LEXER=ocamllex $(LFLAGS)

PFLAGS=-v --dump --explain --graph
PARSER=menhir $(PFLAGS)


ocaml-python3: $(CMO)
	$(COMPILER) -o $@ $(CMO)


.SUFFIXES: .mli .ml .cmi .cmo .mll .mly
.mli.cmi:
	$(COMPILER) -c  $<

.ml.cmo:
	$(COMPILER) -c $<

.mll.ml:
	$(LEXER) $<

parser.ml parser.mli: parser.mly
	$(PARSER) parser.mly 

.PHONY: clean clean-all
clean:
	-rm $(GENERATED)
	-rm -f *.cm[io] *.o *.annot *~ *.tgz *.tar *.swp
	-rm -f *.automaton *.conflicts *.dot # menhir generated files
	-rm .depend

clean-all:
	make clean
	-rm ocaml-python3

.depend depend:$(GENERATED)
	-rm -f .depend
	-ocamldep *.ml *.mli > .depend

include .depend
