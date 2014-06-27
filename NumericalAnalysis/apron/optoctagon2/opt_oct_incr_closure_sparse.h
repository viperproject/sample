#ifndef __OPT_OCT_INCR_CLOSURE_SPARSE_H_INCLUDED__
#define __OPT_OCT_INCR_CLOSURE_SPARSE_H_INCLUDED__

#ifdef __cplusplus
extern "C" {
#endif

#include "opt_oct_hmat.h"

int incremental_closure_opt_sparse(opt_oct_mat_t *oo, double *temp1, double *temp2, int dim, int v, bool is_int);
double incremental_closure_calc_perf_sparse(double cycles, int dim);


#ifdef __cplusplus
}
#endif

#endif
