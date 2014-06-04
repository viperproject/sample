#ifndef __OPT_OCT_HMAT_H
#define __OPT_OCT_HMAT_H

#ifdef __cplusplus
extern "C" {
#endif

#include "opt_oct_internal.h"
#if defined(SPARSE)

#include "opt_oct_closure_sparse.h"
#include "opt_oct_incr_closure_sparse.h"

#else

#include "opt_oct_closure_dense.h"
#include "opt_oct_incr_closure_dense.h"

#endif

#define min fmin
#define max fmax

void opt_hmat_free(double *m);
double * opt_hmat_alloc_top(int dim);
double *opt_hmat_copy(double * src, int size);
void opt_hmat_set_array(double *dest, double *src, int size);
bool opt_hmat_strong_closure(double *m, int dim);
bool is_top_avx_half_double(double *m, int dim);
bool is_equal_avx_half_double(double *m1, double *m2, int dim);
bool is_lequal_avx_half_double(double *m1, double *m2, int dim);
void meet_avx_half(double *m, double *m1, double *m2, int dim);
void forget_array_avx_half(double *m, ap_dim_t *arr,int dim, int arr_dim, bool project);
void join_avx_half(double *m, double *m1, double *m2, int dim);
void opt_hmat_addrem_dimensions(double * dst, double* src,ap_dim_t* pos, int nb_pos,int mult, int dim, bool add);
void opt_hmat_permute(double* dst, double* src,int dst_dim, int src_dim,ap_dim_t* permutation);
opt_oct_t* opt_oct_expand(ap_manager_t* man, bool destructive, opt_oct_t* o, ap_dim_t dim, size_t n);
opt_oct_t* opt_oct_fold(ap_manager_t* man,bool destructive, opt_oct_t* o,ap_dim_t* tdim,size_t size);
void widening_half(double *m, double *m1, double *m2, int size);
opt_uexpr opt_oct_uexpr_of_linexpr(opt_oct_internal_t* pr, double* dst, ap_linexpr0_t* e, int intdim, int dim);
bool opt_hmat_add_lincons(opt_oct_internal_t* pr, double* m, int intdim, int dim, ap_lincons0_array_t* ar, bool* exact, bool* respect_closure);


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
