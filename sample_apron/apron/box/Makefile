include ../Makefile.config

PREFIX = $(APRON_PREFIX)

SRCDIR = $(shell pwd)

ifneq ($(OCAMLFIND),)
MLGMPIDL_INCLUDE = $(shell $(OCAMLFIND) query gmp)
MLGMPIDL_LIB = $(MLGMPIDL_INCLUDE)
else
MLGMPIDL_INCLUDE = $(MLGMPIDL_PREFIX)/include
MLGMPIDL_LIB = $(MLGMPIDL_PREFIX)/lib
endif

#---------------------------------------
# Programs
#---------------------------------------

#---------------------------------------
# Flags
#---------------------------------------

# Use ICFLAGS to specify machine-independent compilation flags.
ICFLAGS = \
-I$(MLGMPIDL_INCLUDE) \
-I../num \
-I../itv \
-I../apron \
-I../mlapronidl \
-I$(GMP_PREFIX)/include -I$(MPFR_PREFIX)/include \
-I$(CAMLIDL_PREFIX) -I$(CAML_PREFIX)

# Caml
OCAMLINC = -I $(MLGMPIDL_LIB) -I ../mlapronidl

#---------------------------------------
# Files
#---------------------------------------

CCMODULES = box_internal box_representation box_constructor box_meetjoin box_assign box_resize box_otherops box_policy
CCSRC = box_config.h box.h $(CCMODULES:%=%.h) $(CCMODULES:%=%.c)

CCINC_TO_INSTALL = box.h
CCBIN_TO_INSTALL =
CCLIB_TO_INSTALL = \
libboxMPQ.a libboxD.a libboxMPFR.a \
libboxMPQ_debug.a libboxD_debug.a libboxMPFR_debug.a
ifneq ($(HAS_SHARED),)
CCLIB_TO_INSTALL := $(CCLIB_TO_INSTALL) \
libboxMPQ.so libboxD.so libboxMPFR.so \
libboxMPQ_debug.so libboxD_debug.so libboxMPFR_debug.so
endif

ifneq ($(HAS_OCAML),)
CAML_TO_INSTALL = \
box.idl box.ml box.mli box.cmi \
boxMPQ.cma \
boxD.cma \
boxMPFR.cma \
libboxMPQ_caml.a libboxD_caml.a libboxMPFR_caml.a \
libboxMPQ_caml_debug.a libboxD_caml_debug.a libboxMPFR_caml_debug.a
ifneq ($(HAS_OCAMLOPT),)
CAML_TO_INSTALL := $(CAML_TO_INSTALL) \
boxMPQ.cmxa boxMPQ.a \
boxD.cmxa boxD.a \
boxMPFR.cmxa boxMPFR.a
endif
ifneq ($(HAS_SHARED),)
CAML_TO_INSTALL += dllboxMPQ_caml.so dllboxD_caml.so dllboxMPFR_caml.so 
endif
endif

LIBS = -L$(APRON_PREFIX) -lapron -L$(MPFR_PREFIX)/lib -lmpfr -L$(GMP_PREFIX)/lib -lgmp -lm 
LIBS_DEBUG = -L$(APRON_PREFIX) -lapron_debug -L$(MPFR_PREFIX)/lib -lmpfr -L$(GMP_PREFIX)/lib -lgmp -lm 

#---------------------------------------
# Rules
#---------------------------------------

# Possible goals:
# depend doc install
# and the following one

all: allMPQ allD allMPFR

allMPQ: libboxMPQ.a libboxMPQ_debug.a
allD: libboxD.a libboxD_debug.a
allMPFR: libboxMPFR.a libboxMPFR_debug.a
ifneq ($(HAS_SHARED),)
allMPQ: libboxMPQ.so libboxMPQ_debug.so
allD: libboxD.so libboxD_debug.so
allMPFR: libboxMPFR.so libboxMPFR_debug.so
endif

ml: box.mli box.ml box.cmi mlMPQ mlD mlMPFR

mlMPQ: boxMPQ.cma libboxMPQ_caml.a libboxMPQ_caml_debug.a 
mlD: boxD.cma libboxD_caml.a libboxD_caml_debug.a
mlMPFR: boxMPFR.cma libboxMPFR_caml.a libboxMPFR_caml_debug.a
ifneq ($(HAS_OCAMLOPT),)
mlMPQ: boxMPQ.cmxa boxMPQ.a
mlD: boxD.cmxa boxD.a
mlMPFR: boxMPFR.cmxa boxMPFR.a
endif
ifneq ($(HAS_SHARED),)
mlMPQ: dllboxMPQ_caml.so 
mlD: dllboxD_caml.so
mlMPFR: dllboxMPFR_caml.so
endif

mlexample%.byte: mlexample.ml box%.cma
	$(OCAMLC) $(OCAMLFLAGS) -I $(MLGMPIDL_LIB) -I $(APRON_PREFIX)/lib -o $@ bigarray.cma gmp.cma apron.cma box$*.cma $<

mlexample%.opt: mlexample.ml box%.cmxa
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -I $(MLGMPIDL_LIB) -I $(APRON_PREFIX)/lib -o $@ bigarray.cmxa gmp.cmxa apron.cmxa box$*.cmxa $<

clean:
	/bin/rm -f *.[ao] *.so *.annot *.cm[ioax] *.cmxa *.byte *.opt
	/bin/rm -f *.?.tex *.log *.aux *.bbl *.blg *.toc *.dvi *.ps *.pstex*
	/bin/rm -fr boxpolkarung boxpolkatopg tmp
	/bin/rm -f box.ml box.mli box_caml.c
	/bin/rm -f Makefile.depend

mostlyclean: clean
distclean: clean

install: $(CCINC_TO_INSTALL) $(CCLIB_TO_INSTALL)
	$(INSTALLd) $(PREFIX)/include $(PREFIX)/lib
	$(INSTALL) $(CCINC_TO_INSTALL) $(PREFIX)/include
	for i in $(CCLIB_TO_INSTALL); do \
		if test -f $$i; then $(INSTALL) $$i $(PREFIX)/lib; fi; \
	done
ifeq ($(OCAMLFIND),)
	for i in $(CAML_TO_INSTALL); do \
		if test -f $$i; then $(INSTALL) $$i $(PREFIX)/lib; fi; \
	done
endif

uninstall:
	for i in $(CCINC_TO_INSTALL); do /bin/rm -f $(PREFIX)/include/$$i; done
	/bin/rm -f $(PREFIX)/lib/libbox*.* $(PREFIX)/lib/liboct*_debug.*
	/bin/rm -f $(PREFIX)/lib/dllbox*.so $(PREFIX)/lib/dlloct*_debug.so
	/bin/rm -f $(PREFIX)/lib/box.mli $(PREFIX)/lib/box.ml $(PREFIX)/lib/box.cm[ix] $(PREFIX)/lib/box.idl $(PREFIX)/lib/box*.cma $(PREFIX)/lib/box*.cmxa $(PREFIX)/lib/box*.a


dist: $(CCSRC) Makefile perlscript_caml.pl box.texi box.idl box.ml box.mli box_caml.c COPYING README
	(cd ..; tar zcvf box.tgz $(^:%=box/%))

#---------------------------------------
# IMPLICIT RULES AND DEPENDENCIES
#---------------------------------------

.SUFFIXES: .tex .c .h .a .o

.PRECIOUS: libbox%.a libbox%_debug.a libbox%.so libbox%_debug.so
.PRECIOUS: libbox%_caml.a libbox%_caml_debug.a dllbox%_caml.so
.PRECIOUS: %MPQ.o %D.o %MPFR.o
.PRECIOUS: %MPQ_debug.o %D_debug.o %MPFR_debug.o
.PRECIOUS: %.cmo %.cmx

#-----------------------------------
# C part
#-----------------------------------

libbox%.a: $(subst .c,%.o,$(CCMODULES:%=%.c))
	$(AR) rcs $@ $^
	$(RANLIB) $@
libbox%_debug.a: $(subst .c,%_debug.o,$(CCMODULES:%=%.c))
	$(AR) rcs $@ $^
	$(RANLIB) $@
libbox%.so:  $(subst .c,%.o,$(CCMODULES:%=%.c))
	$(CC) $(CFLAGS) -shared -o $@ $^ $(LIBS) -L../apron
libbox%_debug.so: $(subst .c,%_debug.o,$(CCMODULES:%=%.c))
	$(CC) $(CFLAGS_DEBUG) -shared -o $@ $^ $(LIBS_DEBUG) -L../apron

%MPQ.o: %.c
	$(CC) $(CFLAGS) $(ICFLAGS) -DNUM_MPQ -c -o $@ $<
%MPQ_debug.o: %.c
	$(CC) $(CFLAGS_DEBUG) $(ICFLAGS) -DNUM_MPQ -c -o $@ $<
%Rll.o: %.c
	$(CC) $(CFLAGS) $(ICFLAGS) -DNUM_LONGLONGRAT -c -o $@ $<
%Rll_debug.o: %.c
	$(CC) $(CFLAGS_DEBUG) $(ICFLAGS) -DNUM_LONGLONGRAT -c -o $@ $<
%D.o: %.c
	$(CC) $(CFLAGS) $(ICFLAGS) -DNUM_DOUBLE -c -o $@ $<
%D_debug.o: %.c
	$(CC) $(CFLAGS_DEBUG) $(ICFLAGS) -DNUM_DOUBLE -c -o $@ $<
%MPFR.o: %.c
	$(CC) $(CFLAGS) $(ICFLAGS) -DNUM_MPFR -c -o $@ $<
%MPFR_debug.o: %.c
	$(CC) $(CFLAGS_DEBUG) $(ICFLAGS) -DNUM_MPFR -c -o $@ $<

#---------------------------------------
# OCaml binding
#---------------------------------------

libbox%_caml.a: box_caml.o libbox%.a
	$(AR) rcs $@ $<
	$(RANLIB) $@
libbox%_caml_debug.a: box_caml_debug.o libbox%_debug.a
	$(AR) rcs $@ $<
	$(RANLIB) $@

dllbox%_caml.so: box_caml.o libbox%.so
	$(CC) $(CFLAGS) -shared -o $@ box_caml.o -L. -lbox$* $(LIBS) -L../apron

box_caml.o: box_caml.c
	$(CC) $(CFLAGS) $(ICFLAGS) -I../apron -DNUM_MPQ -c -o $@ $<
box_caml_debug.o: box_caml.c
	$(CC) $(CFLAGS_DEBUG) $(ICFLAGS) -I../apron -DNUM_MPQ -c -o $@ $<

#---------------------------------------
# OCaml rules
#---------------------------------------

box%.cma: box.cmo libbox%_caml.a libbox%.a
	$(OCAMLMKLIB) -ocamlc "$(OCAMLC)" -verbose -o box$* -oc box$*_caml box.cmo -L. -lbox$* $(LIBS) -L../apron

box%.cmxa box%.a: box.cmx libbox%_caml.a libbox%.a
	$(OCAMLMKLIB) -ocamlopt "$(OCAMLOPT)" -verbose -o box$* -oc box$*_caml box.cmx -L$(APRON_PREFIX)/lib -L. -lbox$* $(LIBS) -L../apron

#---------------------------------------
# IDL rules
#---------------------------------------

box.ml box.mli box_caml.c: box.idl ../mlapronidl/perlscript_c.pl perlscript_caml.pl ../mlapronidl/*.idl
	mkdir -p tmp
	cp box.idl ../mlapronidl/*.idl tmp/
	cd tmp && $(CAMLIDL) -no-include -nocpp -I . box.idl
	$(PERL) ../mlapronidl/perlscript_c.pl < tmp/box_stubs.c >box_caml.c
	$(PERL) perlscript_caml.pl < tmp/box.ml >box.ml
	$(PERL) perlscript_caml.pl < tmp/box.mli >box.mli

rebuild:
	@echo "make rebuild is no longer necessary"

#---------------------------------------
# ML generic rules
#---------------------------------------

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $<

%.cmo: %.ml %.cmi
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLINC) -c $<

%.cmx: %.ml %.cmi
	$(OCAMLOPT) $(OCAMLOPTFLAGS) $(OCAMLINC) -c $<

#-----------------------------------
# DEPENDENCIES
#-----------------------------------
