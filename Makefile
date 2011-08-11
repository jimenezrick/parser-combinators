OC=ocamlc
OFLAGS=
OEXT=cmo

#OC=ocamlopt
#OFLAGS=
#OEXT=cmx

STD_DEPS=
MODS=backpack.ml parser_combinators.ml
DEPS=$(MODS:.ml=.$(OEXT))

TESTS_MODS=$(wildcard test/*.ml)
TESTS_DEPS=$(TESTS_MODS:.ml=.$(OEXT))
TESTS=$(TESTS_MODS:.ml=)

.PHONY: all test clean

all:
	# TODO: Noting yet!

test: $(TESTS)
	@export OCAMLRUNPARAM=b
	@for t in $?; do $$t; done

$(TESTS): OFLAGS=-g
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
