OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLMKLIB=ocamlmklib

INCLUDES=-I .
OCAMLFLAGS=-w +A-30-42-41-48 -safe-string -strict-sequence -ccopt -fPIC
OCAMLCFLAGS=$(INCLUDES) $(OCAMLFLAGS)
OCAMLOPTFLAGS=$(INCLUDES) $(OCAMLFLAGS)

NAME=landmarks
MODULES=misc callgraph landmark

OBJS=clock.o

CMOS=$(patsubst %,%.cmo,$(MODULES))
CMXS=$(patsubst %,%.cmx,$(MODULES))

all: $(NAME).cma $(NAME).cmxa

$(NAME).cma: $(OBJS) $(CMOS)
		$(OCAMLMKLIB) -o $(NAME) $^

$(NAME).cmxa: $(OBJS) $(CMXS)
		$(OCAMLMKLIB) -o $(NAME) $^

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
