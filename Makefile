OC=ocamlc
OFLAGS=

STD_DEPS=str.cma
DEPS=backpack.cmo parser_combinators.cmo

.PHONY: test clean

all:

test/test: $(DEPS) test/test.cmo
	$(OC) $(OFLAGS) -o test/test $(STD_DEPS) $(DEPS) test/test.cmo

test: OFLAGS=-g
test: test/test
	@export OCAMLRUNPARAM=b
	@test/test

%.cmi: %.mli
	$(OC) $(OFLAGS) -c $<

%.cmo: %.ml
	$(OC) $(OFLAGS) -c $<

# TODO: use ocamlopt
#%.cmx: %.ml
#	$(OC) $(OFLAGS) $<

clean:
	rm -f *.cm* */*.cm*
	rm -f *.o */*.o
