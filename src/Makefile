OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLMKLIB=ocamlmklib
OCAMLMKTOP=ocamlmktop

INCLUDES=-I .
OCAMLFLAGS=-w +A-30-42-41-48-40 -safe-string -strict-sequence
OCAMLCFLAGS=$(INCLUDES) $(OCAMLFLAGS)
OCAMLOPTFLAGS=$(INCLUDES) $(OCAMLFLAGS)

NAME=landmarks
MODULES=landmark_misc landmark_graph landmark

OBJS=clock.o

CMOS=$(patsubst %,%.cmo,$(MODULES))
CMXS=$(patsubst %,%.cmx,$(MODULES))

all: $(NAME).cma $(NAME).cmxa $(NAME)-noc.cma $(NAME).top

$(NAME).top: $(NAME).cma
	$(OCAMLMKTOP) -I . -custom -o $@ $^

$(NAME).cma: $(OBJS) $(CMOS)
		$(OCAMLMKLIB) -custom -o $(NAME) $^

$(NAME)-noc.cma: $(CMOS)
		$(OCAMLC) -a -o $@ $^

$(NAME).cmxa: $(OBJS) $(CMXS)
		$(OCAMLMKLIB) -custom -o $(NAME) $^

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
	rm -f *.cm* *.o *.so *.a .depend

-include .depend
