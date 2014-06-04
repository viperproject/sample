#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include "opt_oct_hmat.h"


opt_oct_t * opt_oct_alloc_internal(opt_oct_internal_t *pr, int dim, int intdim){
	opt_oct_t *o = (opt_oct_t *)malloc(sizeof(opt_oct_t));
	o->intdim = intdim;
	o->dim = dim;
	o->closed = NULL;
	o->m = NULL;
	return o;
}


int opt_oct_size(ap_manager_t* man, opt_oct_t* o)
{
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_ASIZE,0);
  if (!o->m) return 1;
  int size = 2*(o->dim)*(o->dim + 1);
  return size;
}

//Allocate Top Element
opt_oct_t * opt_oct_alloc_top(opt_oct_internal_t *pr, int dim, int intdim){
	opt_oct_t *o = opt_oct_alloc_internal(pr, dim, intdim);
	int size = 2*dim*(dim + 1);
	o->closed = opt_hmat_alloc_top(dim);
	return o;
}

//Free memory
void opt_oct_free_internal(opt_oct_internal_t *pr, opt_oct_t *o){
	if(o->m){
		opt_hmat_free(o->m);
	}
	if(o->closed){
		opt_hmat_free(o->closed);
	}
	o->m = NULL;
	o->closed = NULL;
	free(o);
}


//Function for copying
opt_oct_t * opt_oct_copy_internal(opt_oct_internal_t *pr, opt_oct_t *o){
	opt_oct_t *r = 	opt_oct_alloc_internal(pr,o->dim, o->intdim);
	int size = 2*(o->dim)*(o->dim + 1);
	r->m = opt_hmat_copy(o->m,size);
	//r->m = o->m;
	r->closed = opt_hmat_copy(o->closed,size);
	//r->closed = o->closed;
	return r;	
}


/*****
	Basic Functions
***/

opt_oct_t* opt_oct_set_mat(opt_oct_internal_t* pr, opt_oct_t* o, double* m, double* closed,
		   bool destructive)
{
  opt_oct_t* r;
  if (destructive) {
    /* free non-aliased matrices */
    if (o->m && o->m!=m && o->m!=closed){
      opt_hmat_free(o->m);
      o->m = NULL;
    }
    if (o->closed && o->closed!=m && o->closed!=closed){
      opt_hmat_free(o->closed);
      o->closed = NULL;
    }
    r = o;
  }
  else {
    /* copy aliased matrices */
    
    int size = 2*(o->dim)*(o->dim + 1);
    r = opt_oct_alloc_internal(pr,o->dim,o->intdim);
    if (m && (o->m==m || o->closed==m))
    {    
	
	 m = opt_hmat_copy(m,size);
    }
    if (closed && (o->m==closed || o->closed==closed)){
      
      closed = opt_hmat_copy(closed,size);
    }
  }
  r->m = m;
  r->closed = closed;
  return r;
}


opt_oct_t* opt_oct_copy(ap_manager_t* man, opt_oct_t* o)
{
  //fprintf(stdout,"opt_oct_copy\n");
  //fflush(stdout);
  
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_COPY,0);
  
  opt_oct_t *res = opt_oct_copy_internal(pr,o);
  return res;
}

/*void opt_oct_set(ap_manager_t *man, opt_oct_t *dest, opt_oct_t *src){
	tsc_counter start,end;
  	double cycles;
  	CPUID();
  	RDTSC(start);
	opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_SET,0);
	int dim = src->dim;
	int intdim = src->intdim;
	//dest->dim = src->dim;
	//dest->intdim = src->intdim;
	dest = 	opt_oct_alloc_internal(pr,dim, intdim);
	int size = 2*dim*(dim + 1);
	if(src->m==NULL){
		if(src->closed==NULL){
			opt_hmat_free(dest->m);
			opt_hmat_free(dest->closed);
			dest->m = NULL;
			dest->closed = NULL;
		}
		else{
			if(dest->closed == NULL){
				if(dest->m==NULL){
					dest->closed = (double *) calloc(size,sizeof(double));
					opt_hmat_set_array(dest->closed,src->closed,size);
				}
				else{
					dest->closed = dest->m;
					dest->m = NULL;
					opt_hmat_set_array(dest->closed,src->closed,size);
				}
			}
			else{		
				opt_hmat_set_array(dest->closed,src->closed,size);
				opt_hmat_free(dest->m);
				dest->m = NULL;
			}
			//opt_hmat_free(dest->m);
			//dest->m = NULL;
		}
	}
	else{
		if(src->closed==NULL){
			if(dest->m==NULL){
				if(dest->closed==NULL){
					dest->m = (double *) calloc(size,sizeof(double));
					opt_hmat_set_array(dest->m,src->m,size);
				}
				else{
					dest->m = dest->closed;
					dest->closed = NULL;
					opt_hmat_set_array(dest->m,src->m,size);
				}
			}
			else{
				opt_hmat_set_array(dest->m,src->m,size);
				opt_hmat_free(dest->closed);
				dest->closed = NULL;
			}

		}
		else{
			if(dest->closed==NULL){
				dest->closed = (double *) calloc(size,sizeof(double));
				opt_hmat_set_array(dest->closed,src->closed,size);
			}
			else{
				opt_hmat_set_array(dest->closed,src->closed,size);
			}
			if(dest->m==NULL){
				dest->m = (double *) calloc(size,sizeof(double));
				opt_hmat_set_array(dest->m,src->m,size);
			}
			else{
				opt_hmat_set_array(dest->m,src->m,size);
			}
		}
	}
	RDTSC(end);
  	CPUID();
  	cycles = (double)(COUNTER_DIFF(end, start));
  	set_mat_time += cycles;
	//opt_hmat_set_array(dest->m,src->m,size);
	//opt_hmat_set_array(dest->closed,src->closed,size);
}*/

void opt_oct_free(ap_manager_t* man, opt_oct_t* o)
{
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_FREE,0);
  opt_oct_free_internal(pr,o);
  
}

opt_oct_t* opt_oct_bottom(ap_manager_t* man, int intdim, int realdim)
{
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_BOTTOM,0);
  opt_oct_t* o = opt_oct_alloc_internal(pr,intdim+realdim,intdim);
  return o;
}

opt_oct_t* opt_oct_top(ap_manager_t* man, int intdim, int realdim)
{
  //fprintf(stdout,"opt_oct_top\n");
  //fflush(stdout);
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_TOP,0);
  opt_oct_t* r = opt_oct_alloc_internal(pr,intdim+realdim,intdim);
  int size = 2*(r->dim)*(r->dim + 1);
  r->closed = opt_hmat_alloc_top(r->dim);
  return r;
}


ap_dimension_t opt_oct_dimension(ap_manager_t* man, opt_oct_t* o)
{
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_DIMENSION,0);
  ap_dimension_t r;
  r.intdim = o->intdim;
  r.realdim = o->dim-o->intdim;
  return r;
}
/******
closure

****/
void opt_oct_cache_closure(opt_oct_internal_t *pr, opt_oct_t *o){
	if(o->closed || !o->m){
		return;
	}
	int size = 2*o->dim*(o->dim + 1);
	o->closed = opt_hmat_copy(o->m,size);
	if(opt_hmat_strong_closure(o->closed,o->dim)){
		opt_hmat_free(o->closed);
		opt_hmat_free(o->m);
		o->closed = NULL;
		o->m = NULL;
	}
}

void opt_oct_close(opt_oct_internal_t *pr, opt_oct_t *o){
	if(!o->m){
		return;
	}
	if(o->closed){
		opt_hmat_free(o->m);
		o->m = NULL;
		return;
	}
	o->closed = o->m;
	o->m = NULL;
	if(opt_hmat_strong_closure(o->closed,o->dim)){
		opt_hmat_free(o->closed);
		o->closed = NULL;
		return;
	}
}


/****

Topological closure
****/
opt_oct_t* opt_oct_closure(ap_manager_t *man, bool destructive, opt_oct_t *o){
	opt_oct_internal_t *pr = (opt_oct_internal_t *)opt_oct_init_from_manager(man, AP_FUNID_CLOSURE,0);
	if(destructive)return o;
	return opt_oct_copy_internal(pr,o);
}

/*****
Manager specific

***/

void opt_oct_internal_free(opt_oct_internal_t *pr){
	opt_hmat_free(pr->tmp);
	opt_hmat_free(pr->tmp2);
	pr->tmp = NULL;
	pr->tmp2 = NULL;
	free(pr);
}

/*****
Print Timing Information

****/



ap_manager_t* opt_oct_manager_alloc(void)
{
  size_t i;
  ap_manager_t* man;
  opt_oct_internal_t* pr;

  if (!ap_fpu_init()) {
    ////fprintf(stderr,"opt_oct_manager_alloc cannot change the FPU rounding mode\n");
  }

  pr = (opt_oct_internal_t*)malloc(sizeof(opt_oct_internal_t));
  assert(pr);
  pr->tmp_size = 10;
  pr->tmp = (double *)calloc(pr->tmp_size,sizeof(double));
  assert(pr->tmp);
  init_array(pr->tmp,pr->tmp_size);
  pr->tmp2 = calloc(pr->tmp_size,sizeof(long));
  assert(pr->tmp2);

  man = ap_manager_alloc("opt_oct","1.0 with double", pr,
			 (void (*)(void*))opt_oct_internal_free);

  pr->man = man;

  man->funptr[AP_FUNID_COPY] = &opt_oct_copy;
  man->funptr[AP_FUNID_FREE] = &opt_oct_free;
  man->funptr[AP_FUNID_ASIZE] = &opt_oct_size;
  //man->funptr[AP_FUNID_MINIMIZE] = &oct_minimize;
  //man->funptr[AP_FUNID_CANONICALIZE] = &oct_canonicalize;
  //man->funptr[AP_FUNID_HASH] = &oct_hash;
  //man->funptr[AP_FUNID_APPROXIMATE] = &oct_approximate;
  //man->funptr[AP_FUNID_FPRINT] = &opt_oct_fprint;
  //man->funptr[AP_FUNID_FPRINTDIFF] = &oct_fprintdiff;
  //man->funptr[AP_FUNID_FDUMP] = &oct_fdump;
  //man->funptr[AP_FUNID_SERIALIZE_RAW] = &oct_serialize_raw;
  //man->funptr[AP_FUNID_DESERIALIZE_RAW] = &oct_deserialize_raw;
  man->funptr[AP_FUNID_BOTTOM] = &opt_oct_bottom;
  man->funptr[AP_FUNID_TOP] = &opt_oct_top;
  //man->funptr[AP_FUNID_OF_BOX] = &oct_of_box;
  man->funptr[AP_FUNID_DIMENSION] = &opt_oct_dimension;
  man->funptr[AP_FUNID_IS_BOTTOM] = &opt_oct_is_bottom;
  man->funptr[AP_FUNID_IS_TOP] = &opt_oct_is_top;
  man->funptr[AP_FUNID_IS_LEQ] = &opt_oct_is_leq;
  man->funptr[AP_FUNID_IS_EQ] = &opt_oct_is_eq;
  //man->funptr[AP_FUNID_IS_DIMENSION_UNCONSTRAINED] = &oct_is_dimension_unconstrained;
  //man->funptr[AP_FUNID_SAT_INTERVAL] = &oct_sat_interval;
  man->funptr[AP_FUNID_SAT_LINCONS] = &opt_oct_sat_lincons;
  man->funptr[AP_FUNID_SAT_TCONS] = &opt_oct_sat_tcons;
  //man->funptr[AP_FUNID_BOUND_DIMENSION] = &oct_bound_dimension;
  //man->funptr[AP_FUNID_BOUND_LINEXPR] = &oct_bound_linexpr;
  //man->funptr[AP_FUNID_BOUND_TEXPR] = &oct_bound_texpr;
  man->funptr[AP_FUNID_TO_BOX] = &opt_oct_to_box;
  man->funptr[AP_FUNID_TO_LINCONS_ARRAY] = &opt_oct_to_lincons_array;
  //man->funptr[AP_FUNID_TO_TCONS_ARRAY] = &oct_to_tcons_array;
  //man->funptr[AP_FUNID_TO_GENERATOR_ARRAY] = &oct_to_generator_array;
  man->funptr[AP_FUNID_MEET] = &opt_oct_meet;
  //man->funptr[AP_FUNID_MEET_ARRAY] = &oct_meet_array;
  man->funptr[AP_FUNID_MEET_LINCONS_ARRAY] = &opt_oct_meet_lincons_array;
  man->funptr[AP_FUNID_MEET_TCONS_ARRAY] = &opt_oct_meet_tcons_array;
  man->funptr[AP_FUNID_JOIN] = &opt_oct_join;
  //man->funptr[AP_FUNID_JOIN_ARRAY] = &oct_join_array;
  //man->funptr[AP_FUNID_ADD_RAY_ARRAY] = &oct_add_ray_array;
  //man->funptr[AP_FUNID_ASSIGN_LINEXPR_ARRAY] = &oct_assign_linexpr_array;
  //man->funptr[AP_FUNID_SUBSTITUTE_LINEXPR_ARRAY] = &oct_substitute_linexpr_array;
    man->funptr[AP_FUNID_ASSIGN_TEXPR_ARRAY] = &opt_oct_assign_texpr_array;
  //man->funptr[AP_FUNID_SUBSTITUTE_TEXPR_ARRAY] = &oct_substitute_texpr_array;
    man->funptr[AP_FUNID_ADD_DIMENSIONS] = &opt_oct_add_dimensions;
    man->funptr[AP_FUNID_REMOVE_DIMENSIONS] = &opt_oct_remove_dimensions;
    man->funptr[AP_FUNID_PERMUTE_DIMENSIONS] = &opt_oct_permute_dimensions;
    man->funptr[AP_FUNID_FORGET_ARRAY] = &opt_oct_forget_array;
    man->funptr[AP_FUNID_EXPAND] = &opt_oct_expand;
    man->funptr[AP_FUNID_FOLD] = &opt_oct_fold;
    man->funptr[AP_FUNID_WIDENING] = &opt_oct_widening;
    man->funptr[AP_FUNID_CLOSURE] = &opt_oct_closure;
    //man->funptr[AP_FUNID_SET] = &opt_oct_set;
    //man->funptr[AP_FUNID_PRINT_TIMING] = &opt_oct_print_timing;
	
  for (i=0;i<AP_EXC_SIZE;i++)
    ap_manager_set_abort_if_exception(man,i,false);

  return man;
}

