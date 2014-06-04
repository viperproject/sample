#include "opt_oct_hmat.h"

/****************
Projection:
*****************/

opt_oct_t* opt_oct_forget_array(ap_manager_t* man,
			bool destructive, opt_oct_t* o,
			ap_dim_t* tdim, int size,
			bool project)
{
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_FORGET_ARRAY,0);
  if (pr->funopt->algorithm>=0) opt_oct_cache_closure(pr,o);
  if (!o->closed && !o->m)
    /* definitively empty */
    return opt_oct_set_mat(pr,o,NULL,NULL,destructive);
  else {
    double* m = o->closed ? o->closed : o->m;
    int mat_size = 2*o->dim*(o->dim + 1);
    if (!destructive) m = opt_hmat_copy(m,mat_size);
    forget_array_avx_half(m,tdim,o->dim,size,project);
    if (o->closed) {
      /* result is exact on Q, and closed if forget, not project */
      if (num_incomplete || o->intdim) flag_incomplete;
      if (project) return opt_oct_set_mat(pr,o,m,NULL,destructive);
      else return opt_oct_set_mat(pr,o,NULL,m,destructive);
    }
    else {
      /* not exact, not closed */
      flag_algo;
      return opt_oct_set_mat(pr,o,m,NULL,destructive);
    }
  }
}



/******
Add Dimensions

*****/
opt_oct_t* opt_oct_add_dimensions(ap_manager_t* man,
			  bool destructive, opt_oct_t* o,
			  ap_dimchange_t* dimchange,
			  bool project)
{
  //fprintf(stdout,"opt_oct_add_dimensions\n");
  ////fflush(stdout);
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_ADD_DIMENSIONS,0);
  double* m = o->closed ? o->closed : o->m;
  double* mm;
  size_t i, nb = dimchange->intdim+dimchange->realdim;
  opt_oct_t* r;
  if (!m) mm = NULL;
  else {
    /* check */
    /**** TODO: Handle arg_assert
    for (i=0;i<nb;i++) {
      arg_assert(dimchange->dim[i]<=a->dim,return NULL;);
      arg_assert(!i || dimchange->dim[i-1]<=dimchange->dim[i],return NULL;);
    }
    ****/
  for (i=0;i<nb;i++) {
    if(dimchange->dim[i] > o->dim) return NULL;
    if(i &&(dimchange->dim[i-1]> dimchange->dim[i])) return NULL;
   }
    /* insert variables */
    int dim = o->dim + nb;
    int size = 2*dim*(dim + 1); 
    mm = opt_hmat_alloc_top(dim);
    opt_hmat_addrem_dimensions(mm,m,dimchange->dim,
			   nb,1,o->dim,true);

    /* set new variables to 0, if necessary */
    if (project) {
      for (i=0;i<nb;i++) {
	size_t v = 2*(i+dimchange->dim[i]);
	mm[opt_matpos(v+1,v)] = 0;
	mm[opt_matpos(v,v+1)]  = 0;
      }
    }
  }
  /* always exact, respect closure if embedding, not projecting */
  if (o->closed && !project) r = opt_oct_set_mat(pr,o,NULL,mm,destructive);
  else r = opt_oct_set_mat(pr,o,mm,NULL,destructive);
  r->dim += nb;
  r->intdim += dimchange->intdim;
  return r;
}


/********
Remove dimensions
********/

opt_oct_t* opt_oct_remove_dimensions(ap_manager_t* man,
			     bool destructive, opt_oct_t* o,
			     ap_dimchange_t* dimchange)
{
  //fprintf(stdout,"opt_oct_remove_dimensions\n");
  ////fflush(stdout);
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_REMOVE_DIMENSIONS,0);
  double *m, *mm;
  size_t i, nb = dimchange->intdim+dimchange->realdim;
  opt_oct_t* r;
  if (pr->funopt->algorithm>=0) opt_oct_cache_closure(pr,o);
  m = o->closed ? o->closed : o->m;
  if (!m) mm = NULL;
  else {
    /* check */
     for (i=0;i<nb;i++) {
		if(dimchange->dim[i] >= o->dim) return NULL;
		if(i && (dimchange->dim[i-1]>=dimchange->dim[i]))return NULL;
     }
    /* remove variables */
    int dim = o->dim - nb;
    int size = 2*dim*(dim + 1);
    mm = (double *)calloc(size,sizeof(double));
    //posix_memalign((void **)&mm,32,size*sizeof(double));
    opt_hmat_addrem_dimensions(mm,m,dimchange->dim,
			   nb,1,o->dim,false);
  }

  if (o->closed) {
    /* result is exact on Q, and closed */
    if (num_incomplete || o->intdim) flag_incomplete;
    r = opt_oct_set_mat(pr,o,NULL,mm,destructive);
  }
  else {
    /* not exact, not closed */
    flag_algo;
    r = opt_oct_set_mat(pr,o,mm,NULL,destructive);
  }
  r->dim -= nb;
  r->intdim -= dimchange->intdim;
  return r;
}


/***********
Permute Dimensions
***********/

opt_oct_t* opt_oct_permute_dimensions(ap_manager_t* man,
			      bool destructive, opt_oct_t* o,
			      ap_dimperm_t* permutation)
{
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_ADD_DIMENSIONS,0);
  double* m = o->closed ? o->closed : o->m;
  double* mm;
  if(permutation->size!=o->dim)return NULL;
  if (!m) mm = NULL;
  else {
    /* check (only bounds, not injectivity) */
    int i,j;    
    /* permuted copy */
     for (i=0;i<o->dim;i++){
          if(permutation->dim[i]>=o->dim)return NULL;
     }

    int size = 2*o->dim*(o->dim + 1);
    mm = (double *)calloc(size,sizeof(double));
    //posix_memalign((void **)&mm,32,size*sizeof(double));
    opt_hmat_permute(mm,m,o->dim,o->dim,permutation->dim);
  }
  /* always exact, respects closure */
  if (o->closed) return opt_oct_set_mat(pr,o,NULL,mm,destructive);
  else return opt_oct_set_mat(pr,o,mm,NULL,destructive);
}

opt_oct_t* opt_oct_expand(ap_manager_t* man,
		  bool destructive, opt_oct_t* o,
		  ap_dim_t dim,
		  size_t n)
{
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_EXPAND,0);
  double* m = o->closed ? o->closed : o->m;
  size_t i, j;
  ap_dim_t pos = (dim < o->intdim) ? o->intdim : o->dim;
  double* mm;
  opt_oct_t* r;
  if(dim >= o->dim){
	return NULL;
  }
  if (!m) mm = NULL;
  else {
    /* insert n variables at pos */
    mm = opt_hmat_alloc_top(o->dim+n);
    opt_hmat_addrem_dimensions(mm,m,&pos,1,n,o->dim,true);

    for (i=0;i<n;i++) {
	int src_ind,dest_ind;
	 #if defined(VECTOR)
		__m256d src;
      /* copy binary constraints */
	      for (j=0;j<(2*dim)/4;j++) {
		//mm[opt_matpos(2*(pos+i),j)] = mm[opt_matpos(2*dim,j)];
		//int op = 2*(pos+i);
		//op = ((op+1)*(op+1))/2;
		dest_ind = opt_matpos(2*(pos+i),j*4);
		src_ind = opt_matpos(2*dim,j*4);
		src = _mm256_loadu_pd(mm+src_ind);
		_mm256_storeu_pd(mm+dest_ind,src); 
		dest_ind = opt_matpos(2*(pos+i)+1,j*4);
		src_ind = opt_matpos(2*dim+1,j*4);
		src = _mm256_loadu_pd(mm+src_ind);
		_mm256_storeu_pd(mm+dest_ind,src); 
		//mm[opt_matpos(2*(pos+i)+1,j)] = mm[opt_matpos(2*dim+1,j)];
	      }
      #else
	   for(j= 0; j < ((2*dim)/4)*4;j++ ){
		mm[opt_matpos(2*(pos+i),j)] = mm[opt_matpos(2*dim,j)];
		mm[opt_matpos(2*(pos+i)+1,j)] = mm[opt_matpos(2*dim+1,j)];
	   }
      #endif
      for(j = ((2*dim)/4)*4;j < 2*dim; j++){
	mm[opt_matpos(2*(pos+i),j)] = mm[opt_matpos(2*dim,j)];
	mm[opt_matpos(2*(pos+i)+1,j)] = mm[opt_matpos(2*dim+1,j)];
      }	

      for (j=2*dim+2;j<2*(o->dim+n);j++) {
	mm[opt_matpos2(2*(pos+i),j)] = mm[opt_matpos(j^1,2*dim+1)];
	mm[opt_matpos2(2*(pos+i)+1,j)] = mm[opt_matpos(j^1,2*dim)];
      }

      /* copy unary constraints */
      mm[opt_matpos2(2*(pos+i),2*(pos+i)+1)] = mm[opt_matpos2(2*dim,2*dim+1)];
      mm[opt_matpos2(2*(pos+i)+1,2*(pos+i))] = mm[opt_matpos2(2*dim+1,2*dim)];
    }
  }
  
  /*  exact, generally not closed */
  r = opt_oct_set_mat(pr,o,mm,NULL,destructive);
  r->dim += n;
  if (dim<o->intdim) r->intdim += n;
  return r;
}

opt_oct_t* opt_oct_fold(ap_manager_t* man,
		bool destructive, opt_oct_t* o,
		ap_dim_t* tdim,
		size_t size)
{
  opt_oct_internal_t* pr = opt_oct_init_from_manager(man,AP_FUNID_FOLD,2*(o->dim)*(o->dim+1));
  double* m;
  double* mm;
  opt_oct_t* r;
  if (pr->funopt->algorithm>=0) opt_oct_cache_closure(pr,o);
  m = o->closed ? o->closed : o->m;
  if (!m) mm = NULL;
  else {
    /* check, assuming tdim[0..(size-1)] is strictly increasing */
    size_t i,j;
    //arg_assert(size>0,return NULL;);
    if(size <= 0){
	return NULL;
    }
    //for (i=1;i<size;i++) {
    //  arg_assert(tdim[i-1]<tdim[i],return NULL;);
    //}
    for (i=1;i<size;i++) {
	if(tdim[i-1]>=tdim[i]){
		return NULL;	
	}
    }
    //arg_assert(tdim[size-1]<a->dim,return NULL;);
    if(tdim[size-1]>=o->dim){
	return NULL;
    }
    
    opt_hmat_set_array(pr->tmp,m,2*(o->dim)*(o->dim+1));

    /* merge binary constraints */
    for (j=0;j<2*tdim[0];j++) {
      double* mm1 = pr->tmp+opt_matpos2(tdim[0]*2,j);
      double* mm2 = pr->tmp+opt_matpos2(tdim[0]*2+1,j);
      for (i=1;i<size;i++) {
	*mm1 =  max(*mm1,m[opt_matpos2(tdim[i]*2,j)]);
	*mm2 = max(*mm2,m[opt_matpos2(tdim[i]*2+1,j)]);
      }
    }
    for (j=2*(tdim[0]+1);j<2*o->dim;j++) {
      double* mm1 = pr->tmp+opt_matpos2(tdim[0]*2,j);
      double* mm2 = pr->tmp+opt_matpos2(tdim[0]*2+1,j);
      for (i=1;i<size;i++) {
	*mm1 = max(*mm1,m[opt_matpos2(tdim[i]*2,j)]);
	*mm2 = max(*mm2,m[opt_matpos2(tdim[i]*2+1,j)]);
      }
    }

    /* merge unary constraints */
    {
      double* mm1 = pr->tmp+opt_matpos2(tdim[0]*2,tdim[0]*2+1);
      double* mm2 = pr->tmp+opt_matpos2(tdim[0]*2+1,tdim[0]*2);
      for (i=1;i<size;i++) {
	*mm1 = max(*mm1,m[opt_matpos2(tdim[i]*2,tdim[i]*2+1)]);
	*mm2 = max(*mm2,m[opt_matpos2(tdim[i]*2+1,tdim[i]*2)]);
      }
    }

    /* destroy all dimensions in tdim except the first one */
    mm = opt_hmat_alloc_top(o->dim-size+1);
    opt_hmat_addrem_dimensions(mm,pr->tmp,tdim+1,size-1,1,o->dim,false);

    /* reset diagonal elements */
     mm[opt_matpos(tdim[0]*2  ,tdim[0]*2  )] = 0;
     mm[opt_matpos(tdim[0]*2+1,tdim[0]*2+1)] = 0;

    man->result.flag_exact = false;
  }

  if (o->closed) {
    /* result is optimal on Q, not closed */
    if (num_incomplete || o->intdim) flag_incomplete;
    r = opt_oct_set_mat(pr,o,mm,NULL,destructive);
  }
  else {
    /* not exact, not closed */
    flag_algo;
    r = opt_oct_set_mat(pr,o,mm,NULL,destructive);
  }
  r->dim -= size-1;
  if (tdim[0]<r->intdim) r->intdim -= size-1;
  return r;
}

