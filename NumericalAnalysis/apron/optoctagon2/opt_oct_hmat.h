#ifndef __OPT_OCT_HMAT_H
#define __OPT_OCT_HMAT_H

#ifdef __cplusplus
extern "C" {
#endif

#include "opt_oct_internal.h"
#include "opt_oct_closure_sparse.h"
#include "opt_oct_incr_closure_sparse.h"


#if defined(VECTOR)

#include "opt_oct_closure_dense.h"
#include "opt_oct_incr_closure_dense.h"

#else

#include "opt_oct_closure_dense_scalar.h"
#include "opt_oct_incr_closure_dense_scalar.h"

#endif

#define min fmin
#define max fmax

void opt_hmat_free(opt_oct_mat_t *m);
opt_oct_mat_t * opt_hmat_alloc_top(int dim);
opt_oct_mat_t *opt_hmat_copy(opt_oct_mat_t * src, int size);
void opt_hmat_set_array(double *dest, double *src, int size);
bool opt_hmat_strong_closure(opt_oct_mat_t *m, int dim);
bool is_top_avx_half_double(opt_oct_mat_t *m, int dim);
bool is_equal_avx_half_double(opt_oct_mat_t *m1, opt_oct_mat_t *m2, int dim);
bool is_lequal_avx_half_double(opt_oct_mat_t *m1, opt_oct_mat_t *m2, int dim);
void meet_avx_half(opt_oct_mat_t *m, opt_oct_mat_t *m1, opt_oct_mat_t *m2, int dim);
void forget_array_avx_half(opt_oct_mat_t *m, ap_dim_t *arr,int dim, int arr_dim, bool project);
void join_avx_half(opt_oct_mat_t *m, opt_oct_mat_t *m1, opt_oct_mat_t *m2, int dim);
void opt_hmat_addrem_dimensions(opt_oct_mat_t * dst, opt_oct_mat_t* src,ap_dim_t* pos, int nb_pos,int mult, int dim, bool add);
void opt_hmat_permute(opt_oct_mat_t* dst, opt_oct_mat_t* src,int dst_dim, int src_dim,ap_dim_t* permutation);
opt_oct_t* opt_oct_expand(ap_manager_t* man, bool destructive, opt_oct_t* o, ap_dim_t dim, size_t n);
opt_oct_t* opt_oct_fold(ap_manager_t* man,bool destructive, opt_oct_t* o,ap_dim_t* tdim,size_t size);
void widening_half(opt_oct_mat_t *m, opt_oct_mat_t *m1, opt_oct_mat_t *m2, int size);
opt_uexpr opt_oct_uexpr_of_linexpr(opt_oct_internal_t* pr, double* dst, ap_linexpr0_t* e, int intdim, int dim);
bool opt_hmat_add_lincons(opt_oct_internal_t* pr, opt_oct_mat_t* oo, int intdim, int dim, ap_lincons0_array_t* ar, bool* exact, bool* respect_closure);

static inline opt_oct_mat_t* opt_hmat_alloc(int size){
	double *m = (double *)calloc(size,sizeof(double));
	opt_oct_mat_t *oo= (opt_oct_mat_t *)malloc(sizeof(opt_oct_mat_t));
	oo->mat = m;
	oo->nni = 0;
	return oo;
}


static inline void print_opt_hmat(double* d, int dim){
  
  if (!d) {
    printf("0\n");
    return;
  }
  printf("%d\n",dim);

 for (int i=0;i<2*dim;i++) {
    for (int j=0;j<2*dim;j++) {
     if (j) fprintf(stdout,"\t");
     fprintf(stdout,"%g",d[opt_matpos2(i,j)]);
    }
    fprintf(stdout,"\n");
  }
  //fprintf(stdout,"\n");
}


#ifdef __cplusplus
}
#endif

#endif
