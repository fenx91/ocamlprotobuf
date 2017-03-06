OCAMLOPT  = ocamlopt
OCAMLYACC = ocamlyacc
OCAMLLEX  = ocamllex

all : clean imp

IMP_OBJS = \
        imp.cmx \
        parse.cmx \
        lex.cmx \
        main.cmx 

clean : 
	$(RM) -f *.cmi *.cmx *.o *.cmo *~ lex.ml parse.ml parse.mli imp imp.exe test-result test-answer hello hello.exe

%.cmi: %.mli
	$(OCAMLOPT) -c $<

%.cmx: %.ml 
	$(OCAMLOPT) -c $<

%.ml %.mli: %.mly
	$(OCAMLYACC) $< 

%.ml: %.mll
	$(OCAMLLEX) $< 

imp: $(IMP_OBJS)
	$(OCAMLOPT) -o imp $(IMP_OBJS)

parse.cmx : parse.cmi parse.ml
main.cmx : parse.cmi
