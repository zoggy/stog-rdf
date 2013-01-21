
VERSION=0.6

MKDIR=mkdir -p
CP=cp -f

OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDOC=ocamldoc
OCAMLLEX=ocamllex
OCAMLFIND=ocamlfind

PACKAGES=netstring,stog,rdf,config-file

INCLUDES=-I +ocamldoc
COMPFLAGS=$(INCLUDES) -g -annot -package $(PACKAGES) -rectypes
LINKFLAGS=$(INCLUDES)
LINKFLAGS_BYTE=$(INCLUDES)

PLUGIN=stog_rdf.cmxs
PLUGIN_BYTE=$(PLUGIN:.cmxs=.cma)

all: byte opt
opt: $(PLUGIN)
byte: $(PLUGIN_BYTE)

stog_rdf.cmxs: stog_rdf.cmx
	$(OCAMLFIND) ocamlopt -package rdf -linkpkg -shared -o $@ \
	$(LINKFLAGS) $^

stog_rdf.cma: stog_rdf.cmo
	$(OCAMLFIND) ocamlc -a -package rdf -linkpkg -o $@ \
	$(LINKFLAGS_BYTE) $^

install:
	$(OCAMLFIND) install stog-rdf META \
	$(PLUGIN) $(PLUGIN_BYTE)

uninstall:
	$(OCAMLFIND) remove stog-rdf

distclean: clean

clean:
	rm -f *.cm* *.o *.annot

test:
	stog.byte --package stog-rdf -v -v -d /tmp/rdftest test/
testopt:
	stog --package stog-rdf -v -v -d /tmp/rdftest test/

###########
archive:
	git archive --prefix=stog-rdf-$(VERSION)/ HEAD | gzip > /tmp/stog-rdf-$(VERSION).tar.gz

# headers :
###########
HEADFILES= Makefile stog_rdf.ml
headers:
	echo $(HEADFILES)
	headache -h header -c .headache_config `ls $(HEADFILES) | grep -v plugin_example`

noheaders:
	headache -r -c .headache_config `ls $(HEADFILES)`

# Rules
.SUFFIXES: .mli .ml .cmi .cmo .cmx

%.cmi:%.mli
	$(OCAMLFIND) ocamlc $(COMPFLAGS) -c $<

%.cmo:%.ml
	if test -f `dirname $<`/`basename $< .ml`.mli && test ! -f `dirname $<`/`basename $< .ml`.cmi ; then \
	$(OCAMLFIND) ocamlc $(COMPFLAGS) -c `dirname $<`/`basename $< .ml`.mli; fi
	$(OCAMLFIND) ocamlc $(COMPFLAGS) -c $<

%.cmi %.cmo:%.ml
	if test -f `dirname $<`/`basename $< .ml`.mli && test ! -f `dirname $<`/`basename $< .ml`.cmi ; then \
	$(OCAMLFIND) ocamlc $(COMPFLAGS) -c `dirname $<`/`basename $< .ml`.mli; fi
	$(OCAMLFIND) ocamlc $(COMPFLAGS) -c $<

%.cmx %.o:%.ml
	$(OCAMLFIND) ocamlopt $(COMPFLAGS) -c $<

.PHONY: clean depend test testopt

.depend depend:
	ocamldep *.ml > .depend


include .depend
