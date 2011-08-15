OC=ocamlc
OFLAGS=
OEXT=cmo

#OC=ocamlopt
#OFLAGS=
#OEXT=cmx

STD_DEPS=
MODS=backpack.ml parser.ml combinators.ml
DEPS=$(MODS:.ml=.$(OEXT))
IFS=$(MODS:.ml=.cmi)

TESTS_MODS=$(wildcard test/*.ml)
TESTS_DEPS=$(TESTS_MODS:.ml=.$(OEXT))
TESTS_IFS=$(TESTS_MODS:.ml=.cmi)
TESTS=$(TESTS_MODS:.ml=)

.PHONY: all test clean

all: $(IFS) $(DEPS)

test: $(TESTS)
	@export OCAMLRUNPARAM=b; for t in $?; do $$t; done

$(TESTS): OFLAGS=-g
$(TESTS): $(IFS) $(DEPS) $(TESTS_DEPS)
	$(OC) $(OFLAGS) -o $@ $(STD_DEPS) $(DEPS) $@.$(OEXT)

%.cmi: %.ml
	$(OC) $(OFLAGS) -i $< > $<i
	$(OC) $(OFLAGS) $<i

%.$(OEXT): %.ml
	$(OC) $(OFLAGS) -c $<

clean:
	rm -f *.mli test/*.mli
	rm -f *.cm* test/*.cm*
	rm -f *.o test/*.o
	rm -f $(TESTS)
