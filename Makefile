OC=ocamlc
STD_DEPS=str.cma
DEPS=backpack.cmo parser_combinators.cmo

.PHONY: test clean

all:

test/test: $(DEPS) test/test.cmo
	$(OC) -o test/test $(STD_DEPS) $(DEPS) test/test.cmo

test: test/test
	@test/test

%.cmi: %.mli
	$(OC) -c $<

%.cmo: %.ml
	$(OC) -c $<

# TODO: use ocamlopt
#%.cmx: %.ml
#	$(OC) $<

clean:
	rm -f *.cm* */*.cm*
	rm -f *.o */*.o
