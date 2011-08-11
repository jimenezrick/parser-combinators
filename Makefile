OC=ocamlc
OFLAGS=

MODS=backpack.ml parser_combinators.ml
DEPS=$(MODS:.ml=.cmo)
STD_DEPS=

TESTS_MODS=$(wildcard test/*.ml)
TESTS_DEPS=$(TESTS_MODS:.ml=.cmo)
TESTS=$(TESTS_MODS:.ml=)

.PHONY: all test clean

all:

test: OFLAGS=-g
test: $(TESTS)
	@export OCAMLRUNPARAM=b
	@for t in $?; do $$t; done

$(TESTS): $(DEPS) $(TESTS_DEPS)
	$(OC) $(OFLAGS) -o $@ $(STD_DEPS) $(DEPS) $@.cmo

%.cmi: %.mli
	$(OC) $(OFLAGS) $<

%.cmo: %.ml
	$(OC) $(OFLAGS) -c $<

%.cmx: OC=ocamlopt
%.cmx: %.ml
	$(OC) $(OFLAGS) -c $<

clean:
	rm -f *.cm* test/*.cm*
	rm -f *.o test/*.o
	rm -f $(TESTS)
