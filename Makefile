#################################################################################
#                Stog-rdf                                                       #
#                                                                               #
#    Copyright (C) 2012-2013 Maxence Guesdon. All rights reserved.              #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU General Public License as                    #
#    published by the Free Software Foundation, version 3 of the License.       #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               #
#    GNU Library General Public License for more details.                       #
#                                                                               #
#    You should have received a copy of the GNU General Public                  #
#    License along with this program; if not, write to the Free Software        #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    As a special exception, you have permission to link this program           #
#    with the OCaml compiler and distribute executables, as long as you         #
#    follow the requirements of the GNU GPL in regard to all of the             #
#    software in the executable aside from the OCaml compiler.                  #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#                                                                               #
#################################################################################

VERSION=0.10.0

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
PLUGIN_LIB=stog_rdf.cmxa
PLUGIN_BYTE=$(PLUGIN:.cmxs=.cma)

all: byte opt
opt: $(PLUGIN) $(PLUGIN_LIB)
byte: $(PLUGIN_BYTE)

# Hack by now, since there is no .cmxs for netstring libs
SHARED_CMXAS=#`$(OCAMLFIND) query -predicates native rdf -a-format -r | grep -v netstring`
SHARED_INCS=`$(OCAMLFIND) query -predicates native rdf -i-format -r | grep -v netstring`
stog_rdf.cmxs: stog_rdf.cmx
	$(OCAMLFIND) ocamlopt $(SHARED_INCS) -shared -o $@ $(SHARED_CMXAS) \
	$(LINKFLAGS) $^

stog_rdf.cmxa: stog_rdf.cmx
	$(OCAMLFIND) ocamlopt -a -package rdf -o $@ \
	$(LINKFLAGS) $^

stog_rdf.cma: stog_rdf.cmo
	$(OCAMLFIND) ocamlc -a -package rdf -o $@ \
	$(LINKFLAGS_BYTE) $^

install:
	$(OCAMLFIND) install stog-rdf META \
	$(PLUGIN) $(PLUGIN_BYTE) $(PLUGIN_LIB) $(PLUGIN_LIB:.cmxa=.a) $(PLUGIN:.cmxs=.cmi)

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
