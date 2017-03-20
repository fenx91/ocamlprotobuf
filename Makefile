OCAMLOPT  = ocamlopt
OCAMLYACC = ocamlyacc
OCAMLLEX  = ocamllex

all : clean PF

PF_OBJS = \
        pf.cmx \
        analyzer.cmx \
	parse.cmx \
        lex.cmx \
        main.cmx 

clean : 
	$(RM) -f *.cmi *.cmx *.o *.cmo *~ lex.ml parse.ml parse.mli pf pf.exe

%.cmi: %.mli
	$(OCAMLOPT) -c $<

%.cmx: %.ml 
	$(OCAMLOPT) -c $<

%.ml %.mli: %.mly
	$(OCAMLYACC) $< 

%.ml: %.mll
	$(OCAMLLEX) $< 

PF: $(PF_OBJS)
	$(OCAMLOPT) -o pf $(PF_OBJS)

parse.cmx : parse.cmi parse.ml
main.cmx : parse.cmi
