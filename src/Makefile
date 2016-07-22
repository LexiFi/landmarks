OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLMKLIB=ocamlmklib
OCAMLMKTOP=ocamlmktop
OCAMLDOC=ocamldoc

INCLUDES=-I .
OCAMLFLAGS=-w +A-30-42-41-48-40-4 -safe-string -strict-sequence -bin-annot -g
OCAMLCFLAGS=$(INCLUDES) $(OCAMLFLAGS)
OCAMLOPTFLAGS=$(INCLUDES) $(OCAMLFLAGS)

NAME=landmarks
MODULES=landmark_misc landmark_graph landmark

config=$(shell ocamlopt -config | grep $(1) | sed "s/[^:]*: //")

OS=$(shell echo "print_endline Sys.os_type;;" | ocaml "")
ARCH=$(call config, architecture)
OBJ_EXT=$(call config, ext_obj)
SYSTEM=$(call config, system)
NATIVE_C_COMPILER=$(firstword $(call config, native_c_compiler))

ifeq ($(NATIVE_C_COMPILER), gcc)
CCOPT=-ccopt -fPIC
endif

OBJS=clock$(OBJ_EXT)

CMOS=$(patsubst %,%.cmo,$(MODULES))
CMXS=$(patsubst %,%.cmx,$(MODULES))

DOCDIR=../doc

all: $(NAME).cma $(NAME).cmxa $(NAME)-noc.cma $(NAME)-threads.cmxa $(NAME)-threads.cma $(NAME).cmxs $(NAME).top

$(NAME).cma: $(OBJS) $(CMOS)
		$(OCAMLMKLIB) -o $(NAME) $^

$(NAME)-threads.cma: $(OBJS) $(CMOS) landmark_threads.cmo
		$(OCAMLMKLIB) -o $(NAME)-threads $^

$(NAME)-noc.cma: $(CMOS)
		$(OCAMLC) -a -o $@ $^

$(NAME).cmxa: $(OBJS) $(CMXS)
		$(OCAMLMKLIB) -o $(NAME) $^

$(NAME)-threads.cmxa: $(OBJS) $(CMXS) landmark_threads.cmx
		$(OCAMLMKLIB) -o $(NAME)-threads $^

$(NAME).cmxs: $(OBJS) $(CMXS)
		$(OCAMLOPT) -shared -o $@ $^


$(NAME).top: $(OBJS) $(CMOS)
		$(OCAMLMKTOP) -custom -o $@ $^

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.c$(OBJ_EXT):
	        $(OCAMLOPT) $(CCOPT) -c $<


landmark_threads.cmo: landmark_threads.ml
	        $(OCAMLC) $(OCAMLFLAGS) -thread -c $<

landmark_threads.cmx: landmark_threads.ml
	        $(OCAMLOPT) $(OCAMLFLAGS) -thread -c $<

.ml.cmo:
	        $(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	        $(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	        $(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

.depend: $(wildcard *.ml) $(wildcard *.mli)
	        $(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

$(DOCDIR):$(wildcard *.mli)
	mkdir -p $(DOCDIR)
	ocamldoc -d $(DOCDIR) -html -I +threads *.mli

.PHONY: clean doc

doc: $(DOCDIR)

clean:
	rm -f *.cm* *.o *.so *.a .depend *.top
	rm -rf $(DOCDIR)

-include .depend
