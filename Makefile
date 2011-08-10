OC=ocamlc
OFLAGS=
MODS=backpack.ml parser_combinators.ml
DEPS=$(MODS:.ml=.cmo)
STD_DEPS=

.PHONY: test clean

all:

test/test: $(DEPS) test/test.cmo
	$(OC) $(OFLAGS) -o test/test $(STD_DEPS) $(DEPS) test/test.cmo

test: OFLAGS=-g
test: test/test
	@export OCAMLRUNPARAM=b
	@test/test

%.cmi: %.mli
	$(OC) $(OFLAGS) $<

%.cmo: %.ml
	$(OC) $(OFLAGS) -c $<

%.cmx: OC=ocamlopt
%.cmx: %.ml
	$(OC) $(OFLAGS) -c $<

clean:
	rm -f *.cm* */*.cm*
	rm -f *.o */*.o
