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
# Flags
#---------------------------------------

# Use ICFLAGS to specify machine-independent compilation flags.
ICFLAGS = \
-I$(MLGMPIDL_INCLUDE) \
-I../apron \
-I../num -I../itv \
-I../newpolka \
-I../ppl \
-I../mlapronidl \
-I$(GMP_PREFIX)/include -I$(MPFR_PREFIX)/include \
-I$(CAMLIDL_PREFIX) -I$(CAML_PREFIX)

# Caml
OCAMLINC = -I $(MLGMPIDL_LIB) -I ../mlapronidl -I ../newpolka -I ../ppl

#---------------------------------------
# Files
#---------------------------------------

CCMODULES = ap_pkgrid 
CCSRC = $(CCMODULES:%=%.h) $(CCMODULES:%=%.c)

CCINC_TO_INSTALL = ap_pkgrid.h
CCBIN_TO_INSTALL =
CCLIB_TO_INSTALL = libap_pkgrid.a libap_pkgrid_debug.a
ifneq ($(HAS_SHARED),)
CCLIB_TO_INSTALL += libap_pkgrid.so libap_pkgrid_debug.so
endif

ifneq ($(HAS_OCAML),)
CAML_TO_INSTALL := polkaGrid.mli polkaGrid.cmi polkaGrid.cma libpolkaGrid_caml.a libpolkaGrid_caml_debug.a 
ifneq ($(HAS_OCAMLOPT),)
CAML_TO_INSTALL += polkaGrid.cmxa polkaGrid.a
endif
ifneq ($(HAS_SHARED),)
CAML_TO_INSTALL += dllpolkaGrid_caml.so
endif
endif

LIBS = -L../ppl -lap_ppl -L$(PPL_PREFIX)/lib -lppl -L$(APRON_PREFIX) -lapron -L$(GMP_PREFIX)/lib -lgmpxx -lgmp -L$(MPFR_PREFIX)/lib -lmpfr -lstdc++ -lm
LIBS_DEBUG = -L../ppl -lap_ppl_debug -L$(PPL_PREFIX)/lib -lppl -L$(APRON_PREFIX) -lapron_debug -L$(GMP_PREFIX)/lib -lgmpxx -lgmp -L$(MPFR_PREFIX)/lib -lmpfr -lstdc++ -lm

#---------------------------------------
# Rules
#---------------------------------------

all: libap_pkgrid.a libap_pkgrid_debug.a 
ifneq ($(HAS_SHARED),)
all: libap_pkgrid.so libap_pkgrid_debug.so 
endif

ml: polkaGrid.mli polkaGrid.ml polkaGrid.cmi polkaGrid.cma libpolkaGrid_caml.a libpolkaGrid_caml_debug.a 
ifneq ($(HAS_OCAMLOPT),)
ml: polkaGrid.cmxa polkaGrid.a
endif
ifneq ($(HAS_SHARED),)
ml: dllpolkaGrid_caml.so
endif

mlexample%.byte: mlexample.ml $(APRON_PREFIX)/lib/polka%.cma $(APRON_PREFIX)/lib/ppl.cma polkaGrid.cma
	$(OCAMLC) $(OCAMLFLAGS) -I $(MLGMPIDL_LIB) -I $(APRON_PREFIX)/lib -o $@ bigarray.cma gmp.cma apron.cma polka%.cma ppl.cma polkaGrid.cma $<

mlexample%.opt: mlexample.ml $(APRON_PREFIX)/lib/polka%.cmxa $(APRON_PREFIX)/lib/ppl.cmxa polkaGrid.cmxa
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -I $(MLGMPIDL_LIB) -I $(APRON_PREFIX)/lib -o $@ bigarray.cmxa gmp.cmxa apron.cmxa polka%.cmxa ppl.cmxa polkaGrid.cmxa $<

clean:
	/bin/rm -f *.[ao] *.so *.annot *.cm[xiao] *.cmxa
	/bin/rm -f *.?.tex *.log *.aux *.bbl *.blg *.toc *.dvi *.ps *.pstex*
	/bin/rm -fr tmp
	/bin/rm -f polkaGrid.ml polkaGrid.mli polkaGrid_caml.c Makefile.depend

distclean: clean

install:
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
	for i in $(CCLIB_TO_INSTALL); do /bin/rm -f $(PREFIX)/lib/$$i; done
	for i in dllpolkaGrid_caml*.so; do /bin/rm -f $(PREFIX)/lib/$$i; done
	for i in $(CCBIN_TO_INSTALL); do /bin/rm -f $(PREFIX)/bin/$$i; done

dist: $(CCSRC) ap_pkgrid.texi perlscript_caml.pl polkaGrid.idl polkaGrid.ml polkaGrid.mli polkaGrid_caml.c Makefile COPYING README
	(cd ..; tar zcvf products.tgz $(^:%=products/%))

#---------------------------------------
# IMPLICIT RULES AND DEPENDENCIES
#---------------------------------------

.SUFFIXES: .tex .c .h .a .o

#-----------------------------------
# C part
#-----------------------------------

libap_pkgrid.a: ap_pkgrid.o
	$(AR) rcs $@ $^
	$(RANLIB) $@
libap_pkgrid_debug.a: ap_pkgrid_debug.o
	$(AR) rcs $@ $^
	$(RANLIB) $@
libap_pkgrid.so: ap_pkgrid.o
	$(CXX) $(CXXFLAGS) -shared -o $@ $^
libap_pkgrid_debug.so: ap_pkgrid_debug.o
	$(CXX) $(CXXFLAGS_DEBUG) -shared -o $@ $^

#---------------------------------------
# C rules
#---------------------------------------

libpolkaGrid_caml.a: polkaGrid_caml.o
	$(AR) rcs $@ $^
	$(RANLIB) $@
libpolkaGrid_caml_debug.a: polkaGrid_caml_debug.o
	$(AR) rcs $@ $^
	$(RANLIB) $@
dllpolkaGrid_caml.so: polkaGrid_caml.o libap_pkgrid.so
	$(CXX) $(CXXFLAGS) -shared -o $@ $< -L. -lap_pkgrid

#---------------------------------------
# ML rules
#---------------------------------------

polkaGrid.cma: polkaGrid.cmo libpolkaGrid_caml.a libap_pkgrid.a
	$(OCAMLMKLIB) -ocamlc "$(OCAMLC) -cc $(CXX)" -verbose -o polkaGrid -oc polkaGrid_caml polkaGrid.cmo -L$(APRON_PREFIX)/lib -lap_pkgrid $(LIBS) 

polkaGrid.cmxa polkaGrid.a: polkaGrid.cmx libpolkaGrid_caml.a libap_pkgrid.a
	$(OCAMLMKLIB) -ocamlopt "$(OCAMLOPT) -cc $(CXX)" -verbose -o polkaGrid -oc polkaGrid_caml polkaGrid.cmx -L$(APRON_PREFIX)/lib -lap_pkgrid $(LIBS)

#---------------------------------------
# IDL rules
#---------------------------------------

# generates polkaGrid.ml, polkaGrid.mli, polkaGrid_caml.c from polkaGrid.idl
polkaGrid.mli polkaGrid.ml polkaGrid_caml.c: polkaGrid.idl ../mlapronidl/macros.pl ../mlapronidl/perlscript_c.pl perlscript_caml.pl ../mlapronidl/*.idl
	mkdir -p tmp
	cp polkaGrid.idl ../mlapronidl/*.idl tmp/
	cd tmp && $(CAMLIDL) -no-include -prepro "$(PERL) ../../mlapronidl/macros.pl" polkaGrid.idl
	$(PERL) ../mlapronidl/perlscript_c.pl < tmp/polkaGrid_stubs.c >polkaGrid_caml.c
	$(PERL) perlscript_caml.pl < tmp/polkaGrid.ml >polkaGrid.ml
	$(PERL) perlscript_caml.pl < tmp/polkaGrid.mli >polkaGrid.mli

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

#---------------------------------------
# C generic rules
#---------------------------------------

%.o: %.c
	$(CC) $(CFLAGS) $(ICFLAGS) -DNUM_MPQ -c -o $@ $<
%_debug.o: %.c
	$(CC) $(CFLAGS_DEBUG) $(ICFLAGS) -DNUM_MPQ -c -o $@ $<
