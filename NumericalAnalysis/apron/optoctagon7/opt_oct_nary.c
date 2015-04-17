#include "opt_oct_hmat.h"

/*******
Standard Meet
****/

opt_oct_t* opt_oct_meet(ap_manager_t* man, bool destructive, opt_oct_t* o1, opt_oct_t* o2)
{
  
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_MEET,0);
  opt_oct_mat_t* oo;
  /***** TODO: handle arg_assert
  ****/
   if((o1->dim != o2->dim) || (o1->intdim != o2->intdim))return NULL;
  //arg_assert(a1->dim==a2->dim && a1->intdim==a2->intdim,return NULL;);
  if ((!o1->closed && !o1->m) || (!o2->closed && !o2->m))
    /* one argument is empty */
    return opt_oct_set_mat(pr,o1,NULL,NULL,destructive);
  else {
    int size = 2*(o1->dim)*(o1->dim + 1);
    opt_oct_mat_t * oo1 = o1->closed ? o1->closed : o1->m;
    opt_oct_mat_t * oo2 = o2->closed ? o2->closed : o2->m;
    oo = destructive ? oo1 : opt_hmat_alloc(size);
   /* if(destructive){
	m = m1;
    }
    else{
	posix_memalign((void **)&m,32,size*sizeof(double));
    }*/
    meet_avx_half(oo,oo1,oo2,o1->dim,destructive);
    /* optimal, but not closed */
    return opt_oct_set_mat(pr,o1,oo,NULL,destructive);
  }
}

/*****
Standard Join
***/
opt_oct_t* opt_oct_join(ap_manager_t* man, bool destructive, opt_oct_t* o1, opt_oct_t* o2)
{
 opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_JOIN,0);
 /**** TODO: handle arg_assert
 //arg_assert(a1->dim==a2->dim && a1->intdim==a2->intdim,return NULL;);
 ***/
 if((o1->dim != o2->dim) || (o1->intdim != o2->intdim))return NULL;
 int size = 2*(o1->dim)*(o1->dim + 1);
 if (pr->funopt->algorithm>=0) {
   opt_oct_cache_closure(pr,o1);
   opt_oct_cache_closure(pr,o2);
 }
 if (!o1->closed && !o1->m) {
   if (!o2->closed && !o2->m)
     /* both empty */
     return opt_oct_set_mat(pr,o1,NULL,NULL,destructive);
   else{
     
     /* a1 empty, a2 not empty */
     return opt_oct_set_mat(pr,o1,opt_hmat_copy(o2->m,o1->dim),
			opt_hmat_copy(o2->closed,o1->dim),destructive);
     }
 }
 else if (!o2->closed && !o2->m){
   /* a1 not empty, a2 empty */
   return opt_oct_set_mat(pr,o1,o1->m,o1->closed,destructive);
  }
 else {
   /* not empty */
   opt_oct_mat_t* oo1 = o1->closed ? o1->closed : o1->m;
   opt_oct_mat_t* oo2 = o2->closed ? o2->closed : o2->m;
   
   opt_oct_mat_t * oo = destructive ? oo1 : opt_hmat_alloc(size);
   /*double *m;
   if(destructive){
	m = m1;
   }
   else{
	posix_memalign((void **)&m,32,size*sizeof(double));
   }*/
   size_t i;
   man->result.flag_exact = false;
   join_avx_half(oo,oo1,oo2,o1->dim,destructive);
   if (o1->closed && o2->closed) {
     /* result is closed and optimal on Q */
     if (num_incomplete || o1->intdim) flag_incomplete;
     return opt_oct_set_mat(pr,o1,NULL,oo,destructive);
   }
   else {
     /* not optimal, not closed */
     flag_algo;
     return opt_oct_set_mat(pr,o1,oo,NULL,destructive); 
   }
 }
}

/****
	Standard Widening
***/

opt_oct_t* opt_oct_widening(ap_manager_t* man, opt_oct_t* o1, opt_oct_t* o2)
{
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_WIDENING,0);
  int algo = pr->funopt->algorithm;
  opt_oct_t* r;
  /*******
  TODO: handle arg_assert
  arg_assert(a1->dim==a2->dim && a1->intdim==a2->intdim,return NULL;);
  ********/
  if((o1->dim != o2->dim) || (o1->intdim != o2->intdim))return NULL;
  if (algo>=0) opt_oct_cache_closure(pr,o2);
  if (!o1->closed && !o1->m){
    /* o1 definitively closed */
    r = opt_oct_copy_internal(pr,o2);
    
  }
  else if (!o2->closed && !o2->m){
   /* o2 definitively closed */
    r = opt_oct_copy_internal(pr,o1);
    
  }
  else {
    /* work on the origial left matrix, not the closed cache! */
    opt_oct_mat_t * oo1 = o1->m ? o1->m : o1->closed;
    opt_oct_mat_t * oo2 = o2->closed ? o2->closed : o2->m;
    size_t i;
    r = opt_oct_alloc_internal(pr,o1->dim,o1->intdim);
    int size = 2*(r->dim)*(r->dim + 1);
    r->m = opt_hmat_alloc(size);
    //posix_memalign((void **)&(r->m),32,size*sizeof(double));
    if (algo==opt_oct_pre_widening || algo==-opt_oct_pre_widening) {
      /* degenerate hull: NOT A PROPER WIDENING, use with care */
     /* for (i=0;i<opt_matsize(r->dim);i++)
	bound_max(r->m[i],m1[i],m2[i]);*/
	join_avx_half(r->m,oo1,oo2,o1->dim,false);
    }
    else {
      /* standard widening */
        widening_half(r->m,oo1,oo2,o1->dim);
    }
  }
  return r;
}


