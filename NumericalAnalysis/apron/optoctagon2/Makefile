include ../Makefile.config


AFLAGS := -D_GNU_SOURCE -pthread -fno-tree-vectorize -m64 -march=native #corei7-avx

IS_SPARSE = -DSPARSE

DFLAGS := -g -DNUM_DOUBLE $(AFLAGS) $(IS_SPARSE) #-DVECTOR



PREFIX = $(APRON_PREFIX)
#PREFIX = /usr/local

LIBDIR = $(PREFIX)/lib

INCLDIR = $(PREFIX)/include
#SOBJS = $(LIBDIR)/liboptoct.so 

ifeq ($(IS_SPARSE),)
CLOSURE_OBJS = opt_oct_closure_dense.o opt_oct_incr_closure_dense.o
CLOSURE_C = opt_oct_closure_dense.c opt_oct_incr_closure_dense.c
CLOSUREH = opt_oct_closure_dense.h opt_oct_incr_closure_dense.h 
else
CLOSURE_OBJS = opt_oct_closure_sparse.o  opt_oct_incr_closure_sparse.o
CLOSURE_C = opt_oct_closure_sparse.c  opt_oct_incr_closure_sparse.c
CLOSUREH =  opt_oct_closure_sparse.h  opt_oct_incr_closure_sparse.h
endif

OBJS = $(CLOSURE_OBJS) opt_oct_nary.o opt_oct_resize.o opt_oct_predicate.o opt_oct_representation.o opt_oct_transfer.o opt_oct_hmat.o

INCLUDES = \
-I$(MLGMPIDL_INCLUDE) \
-I../newpolka \
-I../apron \
-I../mlapronidl \
-I../num \
-I../itv \
-I$(MPFR_PREFIX)/include -I$(GMP_PREFIX)/include \
-I$(CAML_PREFIX)/lib/ocaml -I$(CAMLIDL_PREFIX)/lib/ocaml

LIBS = -L../apron -L$(APRON_PREFIX) -lapron -L$(MPFR_PREFIX)/lib -lmpfr -L$(GMP_PREFIX)/lib -lgmp -lm 

INSTALL = install
INSTALLd = install -d

SOINST = liboptoct.so

OPTOCTH = opt_oct.h opt_oct_internal.h opt_oct_hmat.h $(CLOSUREH)

all : liboptoct.so


perf : perf.c liboptoct.so
	$(CC) -Wno-write-strings -pthread $(CFLAGS) $(DFLAGS) $(INCLUDES) -o perf perf.c $(LIBS) -loptoct

#ifneq ($(IS_SPARSE),)
opt_oct_closure_sparse.o : opt_oct_closure_sparse.h opt_oct_closure_sparse.c
	$(CC) -c $(CFLAGS) $(DFLAGS) $(INCLUDES) -o opt_oct_closure_sparse.o opt_oct_closure_sparse.c $(LIBS)

opt_oct_incr_closure_sparse.o : opt_oct_incr_closure_sparse.h opt_oct_incr_closure_sparse.c opt_oct_closure_sparse.o
	$(CC) -c $(CFLAGS) $(DFLAGS) $(INCLUDES) -o opt_oct_incr_closure_sparse.o opt_oct_incr_closure_sparse.c $(LIBS) opt_oct_closure_sparse.o

#else
opt_oct_closure_dense.o : opt_oct_closure_dense.h opt_oct_closure_dense.c
	$(CC) -c $(CFLAGS) $(DFLAGS) $(INCLUDES) -o opt_oct_closure_dense.o opt_oct_closure_dense.c $(LIBS)

opt_oct_incr_closure_dense.o : opt_oct_incr_closure_dense.h opt_oct_incr_closure_dense.c opt_oct_closure_dense.o
	$(CC) -c $(CFLAGS) $(DFLAGS) $(INCLUDES) -o opt_oct_incr_closure_dense.o opt_oct_incr_closure_dense.c $(LIBS) opt_oct_closure_dense.o

#endif

opt_oct_hmat.o : opt_oct_hmat.h opt_oct_hmat.c 
	$(CC) -c $(CFLAGS) $(DFLAGS) $(INCLUDES) -o opt_oct_hmat.o opt_oct_hmat.c $(LIBS) 

opt_oct_nary.o : opt_oct_nary.c opt_oct_hmat.o
	$(CC) -c $(CFLAGS) $(DFLAGS) $(INCLUDES) -o opt_oct_nary.o opt_oct_nary.c $(LIBS) opt_oct_hmat.o

opt_oct_resize.o : opt_oct_resize.c opt_oct_hmat.o
	$(CC) -c $(CFLAGS) $(DFLAGS) $(INCLUDES) -o opt_oct_resize.o opt_oct_resize.c $(LIBS) opt_oct_hmat.o

opt_oct_predicate.o : opt_oct_predicate.c opt_oct_hmat.o
	$(CC) -c $(CFLAGS) $(DFLAGS) $(INCLUDES) -o opt_oct_predicate.o opt_oct_predicate.c $(LIBS) opt_oct_hmat.o

opt_oct_representation.o : opt_oct_representation.c opt_oct_hmat.o
	$(CC) -c $(CFLAGS) $(DFLAGS) $(INCLUDES) -o opt_oct_representation.o opt_oct_representation.c $(LIBS) opt_oct_hmat.o

opt_oct_transfer.o : opt_oct_transfer.c opt_oct_hmat.o
	$(CC) -c $(CFLAGS) $(DFLAGS) $(INCLUDES) -o opt_oct_transfer.o opt_oct_transfer.c $(LIBS) opt_oct_hmat.o
 
liboptoct.so : $(OBJS)
	$(CC) -shared $(CFLAGS) $(DFLAGS) $(INCLUDES) -o $(SOINST) $(OBJS) $(LIBS)

install:
	$(INSTALLd) $(LIBDIR); \
	for i in $(SOINST); do \
		$(INSTALL) $$i $(LIBDIR); \
	done; 
	
	$(INSTALLd) $(INCLDIR); \
	for i in $(OPTOCTH); do \
		$(INSTALL) $$i $(INCLDIR); \
	done; 

clean:
	-rm $(OBJS) $(SOINST)
