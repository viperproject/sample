#ifndef __OPT_OCT_CLOSURE_SPARSE_H_INCLUDED__
#define __OPT_OCT_CLOSURE_SPARSE_H_INCLUDED__

#ifdef __cplusplus
extern "C" {
#endif

#include "opt_oct_hmat.h"

void print_sparse(double *m, int dim);

double strong_closure_calc_perf_sparse(double cycles, int dim);
bool strong_closure_sparse(opt_oct_mat_t *m, double * temp1, double *temp2, int * ind1, int *ind2, int dim, bool is_int);
bool strengthning_int_sparse(opt_oct_mat_t * result, int * ind1, double *temp, int n);
bool strengthning_sparse(opt_oct_mat_t * result, int * ind1, double *temp, int n);
void compute_index_sparse(double *result, int *index1, int *index2, int k, int dim);

#ifdef __cplusplus
}
#endif

#endif 
