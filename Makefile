OC=ocamlc
OFLAGS=

OEXT=cmo
STD_DEPS=

MODS=backpack.ml parser_combinators.ml
DEPS=$(MODS:.ml=.$(OEXT))

TESTS_MODS=$(wildcard test/*.ml)
TESTS_DEPS=$(TESTS_MODS:.ml=.$(OEXT))
TESTS=$(TESTS_MODS:.ml=)

.PHONY: all all-native test test-native clean

all:

all-native: OC=ocamlopt
all-native: OEXT=cmx
all-native: all

test: OFLAGS=-g
test: $(TESTS)
	@export OCAMLRUNPARAM=b
	@for t in $?; do $$t; done

test-native: OC=ocamlopt
test-native: OEXT=cmx
test-native: STD_DEPS:=$(STD_DEPS:.cma=.cmxa)
test-native: DEPS=$(MODS:.ml=.$(OEXT))
test-native: TESTS_DEPS=$(TESTS_MODS:.ml=.$(OEXT))
test-native: test

$(TESTS): $(DEPS) $(TESTS_DEPS)
	$(OC) $(OFLAGS) -o $@ $(STD_DEPS) $(DEPS) $@.$(OEXT)

%.cmi: %.mli
	$(OC) $(OFLAGS) $<

%.$(OEXT): %.ml
	$(OC) $(OFLAGS) -c $<

clean:
	rm -f *.cm* test/*.cm*
	rm -f *.o test/*.o
	rm -f $(TESTS)
