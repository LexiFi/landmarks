OCAMLC=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt
OCAMLDEP=ocamlfind ocamldep
OCAMLMKLIB=ocamlfind ocamlmklib
OCAMLMKTOP=ocamlfind ocamlmktop

INCLUDES=-I .
OCAMLFLAGS=-w +A-30-42-41-48-40 -safe-string -strict-sequence -bin-annot
OCAMLCFLAGS=$(INCLUDES) $(OCAMLFLAGS)
OCAMLOPTFLAGS=$(INCLUDES) $(OCAMLFLAGS)

NAME=landmarks
MODULES=landmark_misc landmark_graph landmark

OBJS=clock.o

CMOS=$(patsubst %,%.cmo,$(MODULES))
CMXS=$(patsubst %,%.cmx,$(MODULES))

all: $(NAME).cma $(NAME).cmxa $(NAME)-noc.cma $(NAME).cmxs $(NAME).top

$(NAME).cma: $(OBJS) $(CMOS)
		$(OCAMLMKLIB) -o $(NAME) $^

$(NAME)-noc.cma: $(CMOS)
		$(OCAMLC) -a -o $@ $^

$(NAME).cmxa: $(OBJS) $(CMXS)
		$(OCAMLMKLIB) -o $(NAME) $^

$(NAME).cmxs: $(OBJS) $(CMXS)
		$(OCAMLOPT) -shared -o $(NAME).cmxs $^


$(NAME).top: $(OBJS) $(CMOS)
		$(OCAMLMKTOP) -custom -o $@ $^

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.c.o:
	        $(OCAMLOPT) -ccopt -fPIC -c $<

.ml.cmo:
	        $(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	        $(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	        $(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

.depend: $(wildcard *.ml) $(wildcard *.mli)
	        $(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

clean:
	rm -f *.cm* *.o *.so *.a .depend *.top

-include .depend
