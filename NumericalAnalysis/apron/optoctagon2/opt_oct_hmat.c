#include <stdio.h>
#include <stdlib.h>
#include <immintrin.h>
#include "opt_oct_hmat.h"


void opt_hmat_free(opt_oct_mat_t *oo){
        free(oo->mat);
	free(oo);
}

opt_oct_mat_t * opt_hmat_alloc_top(int dim){
	double *m;
	int size = 2*dim*(dim + 1);
	//posix_memalign((void **)&m,32,size*sizeof(double));
	m = (double *)calloc(size, sizeof(double));
	assert(m);
	//double inf = 1.0/0.0;
	#if defined(VECTOR)
		__m256d infty = _mm256_set1_pd(INFINITY);
		//__m256d infty = _mm256_set1_pd(inf);
		for(int i = 0; i < size/8; i++){
			_mm256_storeu_pd(m + i*8, infty);
			_mm256_storeu_pd(m + i*8 + 4, infty);
		}
	//}
	#else
		for(int i = 0; i < (size/8)*8; i++){
			m[i] = INFINITY;
		}
	#endif
	for(int i = (size/8)*8; i < size; i++){
		m[i] = INFINITY;
	}
	for(int i = 0; i < 2*dim; i++){
		int ind = i + (((i + 1)*(i + 1))/2);	
		m[ind] = 0.0;
	}
	opt_oct_mat_t * oo = (opt_oct_mat_t *)malloc(sizeof(opt_oct_mat_t));
	oo->mat = m;
	oo->nni = 2*dim;
	
	return oo;
}

opt_oct_mat_t *opt_hmat_copy(opt_oct_mat_t * src_mat, int size){
	if(!src_mat){
		return NULL;
	}
	
	double *src = src_mat->mat;
	double *dest;
	//posix_memalign((void **)&dest,32,size*sizeof(double));
	dest = (double *)calloc(size, sizeof(double));
	#if defined(VECTOR)
		for(int i = 0; i < size/8; i++){
			__m256d val = _mm256_loadu_pd(src + i*8);
			_mm256_storeu_pd(dest + i*8, val);
			val = _mm256_loadu_pd(src + i*8 + 4);
			_mm256_storeu_pd(dest + i*8 + 4, val);
		}
	
	#else
		int s = (size/8)*8;
		memcpy(dest,src,s*sizeof(double));
	#endif
	for(int i = (size/8)*8; i < size; i++){
		dest[i] = src[i];
	}
	//memcpy(dest,src,size*sizeof(double));
	opt_oct_mat_t * dst_mat = (opt_oct_mat_t *)malloc(sizeof(opt_oct_mat_t));
	dst_mat->mat = dest;
	dst_mat->nni = src_mat->nni;
	return dst_mat;
}

void opt_hmat_set_array(double *dest, double *src, int size){
	//double *src = src_mat->mat;
	if(!src){
		return;
	}
	//double *dest = dest_mat->mat;
	#if defined(VECTOR)
		for(int i = 0; i < size/8; i++){
			__m256d t1 = _mm256_loadu_pd(src + i*8);
			_mm256_storeu_pd(dest + i*8, t1);
			t1 = _mm256_loadu_pd(src + i*8 + 4);
			_mm256_storeu_pd(dest + i*8 + 4, t1);
		}
	#else
		int s = (size/8)*8;
		memcpy(dest,src,s*sizeof(double));
	#endif
	for(int i = (size/8)*8; i <size; i++){
		dest[i] = src[i];
	}
	//dest_mat->nni += src_mat->nni;
}

bool opt_hmat_strong_closure(opt_oct_mat_t *oo, int dim){
	double *temp1, *temp2;
	int *ind1, *ind2;
	temp1 = (double *)calloc(2*dim, sizeof(double));
	temp2 = (double *)calloc(2*dim, sizeof(double));
	bool flag = is_int_flag ? true : false;
        //
	bool res;
	double size = 2*dim*(dim+1);
	double sparsity = 1- ((double)(oo->nni/size));
       // fprintf(stdout,"Input sparsity is\t%d\t%d\t%g\n",oo->nni,dim,sparsity);
	//print_opt_hmat(oo->mat,dim);
	fflush(stdout);
	if(sparsity >= sparse_threshold){
		//posix_memalign((void **)&ind1, 32, 2*(2*dim + 1)*sizeof(int));double sparsity = 1- ((double)(oo->nni/size));
		//posix_memalign((void **)&ind2, 32, 2*(2*dim + 1)*sizeof(int));
		ind1 = (int *)calloc(2*(2*dim + 1), sizeof(int));
		ind2 = (int *)calloc(2*(2*dim + 1), sizeof(int));
		res = strong_closure_sparse(oo,temp1,temp2,ind1, ind2,dim, flag);
		free(ind1);
		ind1 = NULL;
		free(ind2);
		ind2 = NULL;
	//}#if defined(SPARSE)
	}
	else{
		#if defined(VECTOR)
			res = strong_closure_dense(oo,temp1,temp2,dim, flag);
		#else
        		res = strong_closure_dense_scalar(oo,temp1,temp2,dim, flag);
		#endif
	}
        free(temp1);
	temp1 = NULL;
	free(temp2);
        temp2 = NULL;
	sparsity = 1- ((double)(oo->nni/size));
        //fprintf(stdout,"Output sparsity is\t%d\t%g\n",oo->nni,sparsity);
	//print_opt_hmat(oo->mat,dim);
	fflush(stdout);
	return res;
	//return strong_closure_sparse(m,temp1,temp2,ind1,ind2,dim, flag);

}

bool is_top_avx_half_double(opt_oct_mat_t *oo, int dim){
	double *m = oo->mat;
	int size = 2*dim*(dim + 1);
	int n = 2*dim;
	bool flag = true;
	/*small trick just replace 0 at diagonal with infinity temporarily*/
	for(int i = 0; i < n; i++){
		int ind = i + (((i + 1)*(i + 1))/2);
		m[ind] = INFINITY;
	}
	#if defined(VECTOR)
		__m256d infty = _mm256_set1_pd(INFINITY);
		__m256i one = _mm256_set1_epi64x(1);
		for(int i = 0; i < size/4; i++){
			__m256d t1 = _mm256_loadu_pd(m + i*4);
			__m256d res = _mm256_cmp_pd(t1,infty, _CMP_EQ_OQ);
			__m256i op = _mm256_castpd_si256(res);
			if(!_mm256_testc_si256(op,one)){
				flag = false;
				break;
			}
		
		}
	
	#else
		for(int i = 0; i < (size/4)*4; i++){
			if(m[i]!=INFINITY){
				flag = false;
				break;
			}
		}
	#endif
	for(int i = (size/4)*4; i <size; i++){
		if(m[i] != INFINITY){
			flag = false;
			break;
		}
	}
		
	/* now make diagonal elements OPT_ZERO again*/	
	for(int i = 0; i < n; i++){
		int ind = i + (((i + 1)*(i + 1))/2);
		m[ind] = 0;
	}
	return flag;
}


bool is_equal_avx_half_double(opt_oct_mat_t *oo1, opt_oct_mat_t *oo2, int dim){
	double *m1= oo1->mat;
	double *m2 = oo2->mat;
	int size = 2*dim*(dim + 1);
	#if defined(VECTOR)
		__m256i one = _mm256_set1_epi64x(1);
		for(int i = 0; i < size/4; i++){
			__m256d t1 = _mm256_loadu_pd(m1 + i*4);
			__m256d t2 = _mm256_loadu_pd(m2 + i*4);
			__m256d res = _mm256_cmp_pd(t1,t2, _CMP_EQ_OQ);
			__m256i op = _mm256_castpd_si256(res);
			if(!_mm256_testc_si256(op,one)){
				return false;
			}
		}
	
	#else
		for(int i = 0; i < (size/4)*4;i++){
			if(m1[i] != m2[i]){
				return false;
			}
		}
	#endif
	for(int i = (size/4)*4; i < size; i++){
		if(m1[i] != m2[i]){
			return false;
		}
	}
	return true;
}

bool is_lequal_avx_half_double(opt_oct_mat_t *oo1, opt_oct_mat_t *oo2, int dim){
	double *m1 = oo1->mat;
	double *m2 = oo2->mat;
	int size = 2*dim*(dim + 1);
	#if defined(VECTOR)
		__m256i one = _mm256_set1_epi64x(1);
	
		for(int i = 0; i < size/4; i++){
			__m256d t1 = _mm256_loadu_pd(m1 + i*4);
			__m256d t2 = _mm256_loadu_pd(m2 + i*4);
			__m256d res = _mm256_cmp_pd(t1,t2, _CMP_LE_OQ);
			__m256i op = _mm256_castpd_si256(res);
			if(!_mm256_testc_si256(op,one)){
				return false;
			}
		}
	#else
		for(int i = 0; i < (size/4)*4;i++){
			if(m1[i] > m2[i]){
				return false;
			}
		}
	#endif
	for(int i = (size/4)*4; i < size; i++){
		if(m1[i] > m2[i]){
			return false;
		}
	}
	return true;
}

void meet_avx_half(opt_oct_mat_t *oo, opt_oct_mat_t *oo1, opt_oct_mat_t *oo2, int dim){
	double *m = oo->mat;
	double *m1 = oo1->mat;
	double *m2 = oo2->mat;
	int size = 2*dim*(dim + 1);
	#if defined(VECTOR)
		for(int i = 0; i < size/4; i++){
			__m256d t1 = _mm256_loadu_pd(m1 + i*4);
			__m256d t2 = _mm256_loadu_pd(m2 + i*4);
			__m256d t3 = _mm256_min_pd(t1,t2);
			_mm256_storeu_pd(m + i*4,t3);	
			//count = count+4;
		}
	#else
		for(int i = 0; i < (size/4)*4;i++){
			/*if(m1[i]== INFINITY){
				if(m2[i]==INFINITY){
					m[i] = INFINITY;
				}
				else{
					m[i] = m2[i];
					count++;
				}
			}
			else if(m2[i]==INFINITY){
				m[i] = m1[i];
				count++;
			}
			else{*/
				m[i] = min(m1[i],m2[i]);
				//count++;
			//}
		}
	#endif
	for(int i = (size/4)*4; i < size; i++){
		/*if(m1[i]== INFINITY){
			if(m2[i]==INFINITY){
				m[i] = INFINITY;
			}
			else{
				m[i] = m2[i];
				count++;
			}
		}
		else if(m2[i]==INFINITY){
			m[i] = m1[i];
			count++;
		}
		else{*/
			m[i] = min(m1[i],m2[i]);
			//count++;
		//}
	}
	int max_nni = 2*dim*(dim+1);
	oo->nni = oo1->nni + oo2->nni - 2*dim;
        oo->nni = min(max_nni,oo->nni);
	oo->nni = max(2*dim,oo->nni);
}

void forget_array_avx_half(opt_oct_mat_t *oo, ap_dim_t *arr,int dim, int arr_dim, bool project){
	double *m = oo->mat;
	//int count = oo->nni;
	for(int i = 0; i < arr_dim; i++){
		ap_dim_t d = 2*arr[i];
		int d1 = (((d + 1)*(d + 1))/2);
		int d2 = (((d + 2)*(d + 2))/2);
		#if defined(VECTOR)
			__m256d infty = _mm256_set1_pd(INFINITY);	
			for(int j = 0; j < d/4; j++){
				_mm256_storeu_pd(m + d1 + j*4,infty);
				_mm256_storeu_pd(m + d2 + j*4,infty);
				//count = count-8;
			}
		#else
			for(int j = 0; j < (d/4)*4;j++){
				m[d1 + j] = INFINITY;
				m[d2 + j] = INFINITY;
				//count = count - 2;
			}
		#endif
		for(int j = (d/4)*4; j < d; j++){
			m[d1 + j] = INFINITY;
			m[d2 + j] = INFINITY;
			//count=count-2;
		}
		for(int j = d + 2; j < 2*dim; j++){
			int ind1 = d + (((j + 1)*(j + 1))/2);
			int ind2 = (d + 1) + (((j + 1)*(j + 1))/2);
			m[ind1] = INFINITY;
			m[ind2] = INFINITY;
			//count=count-2;;
		}
		if(project){
			m[d1 + d2] = 0;
			m[d2 + d1] = 0;
			//count = count+2;
			oo->nni = oo->nni + 2;
		}
		else{
			m[d1 + (d + 1)] = INFINITY;
			m[d + d2] = INFINITY;
			//count = count - 2;
		}
	}
	
}

void join_avx_half(opt_oct_mat_t *oo, opt_oct_mat_t *oo1, opt_oct_mat_t *oo2, int dim){
	double *m = oo->mat;
	double *m1 = oo1->mat;
	double *m2 = oo2->mat;
	//int count = 0;
	int size = 2*dim*(dim + 1);
	#if defined(VECTOR)
		for(int i = 0; i < size/4; i++){
			__m256d t1 = _mm256_loadu_pd(m1 + i*4);
			__m256d t2 = _mm256_loadu_pd(m2 + i*4);
			__m256d t3 = _mm256_max_pd(t1,t2);
			_mm256_storeu_pd(m + i*4,t3);
			//count = count+4;
		}
	#else
		for(int i = 0; i < (size/4)*4;i++){
			/*if(m1[i]==INFINITY || m2[i]==INFINITY){
				m[i] = INFINITY;
			}
			else{*/
				m[i] = max(m1[i],m2[i]);
				//count++;
			//}
		}
	#endif
	for(int i = (size/4)*4; i <size; i++){
		/*if(m1[i]==INFINITY || m2[i]==INFINITY){
			m[i] = INFINITY;
		}
		else{*/
			m[i] = max(m1[i],m2[i]);
			//count++;
		//}
	}
	oo->nni = min(oo1->nni,oo2->nni);
}

void opt_hmat_addrem_dimensions(opt_oct_mat_t * dst_mat, opt_oct_mat_t* src_mat,
			    ap_dim_t* pos, int nb_pos,
			    int mult, int dim, bool add)
{
  
  int i,j,new_j,org_j;
  new_j = org_j = pos[0]*2;
  	//fflush(stdout);
  double * dst = dst_mat->mat;
  double * src = src_mat->mat;
  opt_hmat_set_array(dst,src,org_j*(org_j/2 + 1));
  for (j=0;j<nb_pos;j++) {
    /* skip lines */
    if (add) new_j += 2*mult; else org_j += 2*mult;
    
    /* copy lines */
    {
      //double* org_c = src + opt_matsize(org_j/2);
	double* org_c = src + org_j*(org_j/2 + 1);
      //double* new_c = dst + opt_matsize(new_j/2);
	double* new_c = dst + new_j*(new_j/2 + 1);
      int last_org_j = ((j<nb_pos-1) ? pos[j+1] : dim)*2;
      for (;org_j<last_org_j;org_j++,new_j++) {
	int size_org_line = org_j+2-(org_j&1);
	int size_new_line = new_j+2-(new_j&1);
	int org_i = 0;
	int new_i = 0;
	for (i=0;i<nb_pos;i++) {
	  /* copy elems */
	  int last_org_i = pos[i]*2;
	  if (last_org_i>=size_org_line) break; /* partial block */
	  opt_hmat_set_array(new_c+new_i,org_c+org_i,last_org_i-org_i);
	  new_i += last_org_i-org_i;
	  org_i = last_org_i;
	  
	  /* skip elems */
	  if (add) new_i += 2*mult; else org_i += 2*mult;
	}
	
	/* copy remaining elems */
	opt_hmat_set_array(new_c+new_i,org_c+org_i,size_org_line-org_i);
  	//fflush(stdout);
	/* next line */
	org_c += size_org_line;
	new_c += size_new_line;
      }
    }
  }
  if(add){
	/****
		Exact number of non infinities for add 
	****/
	int new_dim = dim + nb_pos;
	int max_nni = 2*new_dim*(new_dim +1);
	dst_mat->nni = min(max_nni,src_mat->nni + 2*nb_pos);
  }
  else{
	/**** 
	Approximation of number of non infinities for remove 
	****/
	int new_dim = dim - nb_pos;
	int new_size = 2*new_dim*(new_dim+1);
	dst_mat->nni = min(src_mat->nni - 2*nb_pos,new_size);
	dst_mat->nni = max(2*new_dim,dst_mat->nni);
  }
  //double sparsity = 1 - ((double)(src_mat->nni)/size);
}


void opt_hmat_permute(opt_oct_mat_t* dest_mat, opt_oct_mat_t* src_mat,
		  int dst_dim, int src_dim,
		  ap_dim_t* permutation)
{
  double *dst = dest_mat->mat;
  double *src = src_mat->mat;
  int i,j;
  for (i=0;i<src_dim;i++) {
    int new_ii = 2*permutation[i];
    if (new_ii >= 2*dst_dim)  { src+=4*(i+1); continue; }
    for (j=0;j<=i;j++,src+=2) {
      int new_jj = 2*permutation[j];
      if (new_jj >= 2*dst_dim) continue;
      int ind1,ind2,ind3,ind4;
      if(new_ii >= new_jj){
	 	ind1 = new_jj + (((new_ii + 1)*(new_ii + 1))/2);
		ind3 = new_jj + (((new_ii + 2)*(new_ii + 2))/2);
		ind4 = ind3 + 1;
		if(new_ii >= (new_jj + 1)){
			ind2 = ind1 + 1; 
		}
		else{
			ind2 = (new_ii^1) + (((((new_jj + 1)^1) + 1)*(((new_jj + 1)^1) + 1))/2);
		}
      	}
	else{
		ind1 = (new_ii^1) + ((((new_jj^1) + 1)*((new_jj^1) + 1))/2);
		ind2 = (new_ii^1) + (((((new_jj + 1)^1) + 1)*(((new_jj + 1)^1) + 1))/2);
		ind3 = ((new_ii + 1)^1) + ((((new_jj^1) + 1)*((new_jj^1) + 1))/2);
		ind4 = ((new_ii + 1)^1) + (((((new_jj + 1)^1) + 1)*(((new_jj + 1)^1) + 1))/2);
	}
	
      /*dst[opt_matpos2(new_ii,new_jj)] = src[0];
      dst[opt_matpos2(new_ii,new_jj+1)] = src[1];oo->nni = 2*dim*(dim+1);
      dst[opt_matpos2(new_ii+1,new_jj)] = src[2*(i+1)];
      dst[opt_matpos2(new_ii+1,new_jj+1)] = src[2*(i+1)+1];*/
	dst[ind1] = src[0];
	dst[ind2] = src[1];
	dst[ind3] = src[2*(i + 1)];
	dst[ind4] = src[2*(i + 1) + 1];
    }
    src+=2*(i+1);
  }
  dest_mat->nni = src_mat->nni;
}

void widening_half(opt_oct_mat_t *oo, opt_oct_mat_t *oo1, opt_oct_mat_t *oo2, int size){
	double *m = oo->mat;
	double *m1 = oo1->mat;
	double *m2 = oo2->mat;
	int count = 0;
	//fprintf(stdout,"Forget\t%d\n",oo->nni);
	for(int i = 0; i < size; i++){
		if(m1[i] >=m2[i]){
			m[i] = m1[i];
		}
		else{
			m[i] = INFINITY;
		}
		if(m[i]!=INFINITY){
			count++;
		}
	}
	oo->nni = count;
	//fprintf(stdout,"Forget\t%d\n",oo->nni);
}

opt_uexpr opt_oct_uexpr_of_linexpr(opt_oct_internal_t* pr, double* dst,
			   ap_linexpr0_t* e, int intdim, int dim)
{
#define CLASS_COEFF(idx,coef)						\
  if ((dst[2*idx+2] == -coef) &&				\
      (dst[2*idx+3] == coef)) {				\
    if (u.type==OPT_ZERO) { u.type = OPT_UNARY;  u.i = idx; u.coef_i = coef; }	\
    else              { u.type = OPT_BINARY; u.j = idx; u.coef_j = coef; }	\
    continue;								\
  }
  
#define CLASS_VAR(idx)							\
  if (idx>=intdim) u.is_int = 0;                                        \
  if (u.type==OPT_EMPTY) continue;						\
  if ((dst[2*idx+2] == 0) && (dst[2*idx+3] == 0)) continue;	\
  if (u.type>=OPT_BINARY) { u.type = OPT_OTHER; continue; } 			\
  CLASS_COEFF(idx,1);							\
  CLASS_COEFF(idx,-1);							\
  u.type = OPT_OTHER;
  
#define COEFF(c,i)                                                      \
  ap_coeff_reduce(&c);                                                   \
  if (opt_bounds_of_coeff(pr,dst + i,dst + i + 1,c,false)) u.type = OPT_EMPTY;    \
  if (c.discr!=AP_COEFF_SCALAR || !is_integer(dst[i])) u.is_int = 0;
  opt_uexpr u = { OPT_ZERO, 0, 0, 0, 0, 1 };
  int i;
  
  COEFF(e->cst,0);
  switch (e->discr) {
  case AP_LINEXPR_DENSE:
    /*****
    TODO: handle arg_assert
    arg_assert(e->size<=dim,return u;);
   ****/
    if(e->size > dim)return u;
    for (i=0;i<e->size;i++) {
      
      COEFF(e->p.coeff[i],2*i+2);
      CLASS_VAR(i);
    }
    for (;i<dim;i++) {
      dst[2*i+2] = 0;
      dst[2*i+3] = 0;
    }
    break;
  case AP_LINEXPR_SPARSE:
    for (i=0;i<dim;i++) {
      dst[2*i+2] = 0;
      dst[2*i+3] = 0;
    }
    for (i=0;i<e->size;i++) {
      ap_dim_t d = e->p.linterm[i].dim;
      if (d==AP_DIM_MAX) continue;
      /***********
	TODO: handle arg_assert
	arg_assert(d<dim,return u;);
      ************/
      if(d>=dim)return u;
      COEFF(e->p.linterm[i].coeff,2*d+2);
      CLASS_VAR(d);
    }
    break;
  default: break;/********TODO: handle arg_assert arg_assert(0,return u;);*****/
  }
  return u;
}



bool opt_hmat_add_lincons(opt_oct_internal_t* pr, opt_oct_mat_t* oo, int intdim, int dim,
		      ap_lincons0_array_t* ar, bool* exact,
		      bool* respect_closure)
{
  double *m = oo->mat;
  int i, j, k, ui, uj;
  int var_pending = 0; /* delay incremental closure as long as possible */
  int closure_pending = 0;
  *exact = 1;
   int max_nni = 2*dim*(dim+1);
  bool flag = is_int_flag ? 1 : 0;
  double *temp1, *temp2;
  int *ind1, *ind2;
  //posix_memalign((void **)&temp1, 32, 2*dim*sizeof(double));
  //posix_memalign((void **)&temp2, 32, 2*dim*sizeof(double));
  temp1 = (double *)calloc(2*dim, sizeof(double));
  temp2 = (double *)calloc(2*dim, sizeof(double));
  int (*incr_closure)(double *,...);
  double size = 2*dim*(dim+1);
  double sparsity = 1- ((double)(oo->nni)/size);
  if(sparsity >=sparse_threshold){
	incr_closure = &incremental_closure_opt_sparse;
  }
  else{ 
  	#if defined(VECTOR)
		incr_closure = &incremental_closure_opt_dense;
  //}
  	#else
		incr_closure = &incremental_closure_opt_dense_scalar;
  	#endif
  }
  for (i=0;i<ar->size;i++) {
   ap_constyp_t c = ar->p[i].constyp;
    opt_uexpr u;

    switch (c) {

      /* skipped */
    case AP_CONS_EQMOD:
    case AP_CONS_DISEQ:
      *exact = 0;
      continue;

      /* handled */
    case AP_CONS_EQ:
    case AP_CONS_SUPEQ:
    case AP_CONS_SUP:
      break;

      /* error */
    default:
      assert(0);
    }


    /* now handle ==, >=, > */
    
    u = opt_oct_uexpr_of_linexpr(pr,pr->tmp,ar->p[i].linexpr0,intdim,dim);
    /* transform e+[-a,b] > 0 into >= e+[-(a+1),b-1] >= 0 on integer constraints */
    if (u.is_int && c==AP_CONS_SUP) {
      c = AP_CONS_SUPEQ;
      pr->tmp[0] = pr->tmp[0] + 1;
      pr->tmp[1] = pr->tmp[1] - 1;
    }
    
    int count;
    switch (u.type) {

    case OPT_EMPTY:
      /* OPT_EMPTY constraint: no added information */
      break;

    case OPT_ZERO:
      if ((c==AP_CONS_SUPEQ && (pr->tmp[1]>=0)) ||
	  /* [-a,b] >= 0 <=> b >= 0 */
	  (c==AP_CONS_SUP && (pr->tmp[1]>0)) ||
	  /* [-a,b] > 0 <=> b > 0 */
	  (c==AP_CONS_EQ && (pr->tmp[0]>=0) && (pr->tmp[1]>=0))
	  /* [-a,b] = 0 <=> a >= 0 && b >= 0 */
	  )
	; /* trivial */
      else return true; /* unsatisfiable */
      break;

    case OPT_UNARY:
	
      /* can we delay incremental closure further? */
      if (*respect_closure && closure_pending && var_pending!=u.i) {
	if (incr_closure(oo,temp1,temp2,dim,var_pending, is_int_flag)) return true;
      }
      count = oo->nni;
      closure_pending = 1;
      var_pending = u.i;

      if (u.coef_i==1) ui = 2*u.i; else ui = 2*u.i+1;
      pr->tmp[0] = 2*pr->tmp[0];
      pr->tmp[1] = 2*pr->tmp[1];
      //int ind = (ui^1) + (((ui + 1)*(ui + 1))/2);
      if(m[opt_matpos(ui,ui^1)]==INFINITY){
      	m[opt_matpos(ui,ui^1)] = pr->tmp[1];
      }
      else{
	m[opt_matpos(ui,ui^1)] = min(m[opt_matpos(ui,ui^1)], pr->tmp[1]);
	count++;
      }
      /*  c_i X_i + [-a,b] >= 0 <=> -c_i X_i <= b */
      if (c==AP_CONS_EQ) {
	if(m[opt_matpos(ui^1,ui)]==INFINITY){
		m[opt_matpos(ui^1,ui)] = pr->tmp[0];
	}
        else{
		m[opt_matpos(ui^1,ui)] = min(m[opt_matpos(ui^1,ui)], pr->tmp[0]);
		count++;
	}
      }
      /*  c_i X_i + [-a,b] <= 0 <=>  c_i X_i <= a */
      if (c==AP_CONS_SUP) *exact = 0; /* not exact for strict constraints */
      oo->nni = min(max_nni,count);	
      break;

    case OPT_BINARY:
      
      /* can we delay incremental closure further? */
      if (*respect_closure && closure_pending &&
	  var_pending!=u.i && var_pending!=u.j) {
	//printf("APRON Before applying OPT_BINARY\n");
	if (incr_closure(oo,temp1,temp2,dim,var_pending, is_int_flag)) return true;
      }
      closure_pending = 1;
      var_pending = (var_pending==u.j) ? u.j : u.i;
      count = oo->nni;	
      if ( u.coef_i==1) ui = 2*u.i; else ui = 2*u.i+1;
      if ( u.coef_j==1) uj = 2*u.j; else uj = 2*u.j+1;
      if(m[opt_matpos2(uj,ui^1)]==INFINITY){
		m[opt_matpos2(uj,ui^1)]=pr->tmp[1];
      }
      else{
      	m[opt_matpos2(uj,ui^1)] = min(m[opt_matpos2(uj,ui^1)], pr->tmp[1]);
	count++;
      }
      /*  c_i X_i + c_j X_j + [-a,b] >= 0 <=> -c_i X_i - c_j X_j <= b */
      if (c==AP_CONS_EQ){
	if(m[opt_matpos2(uj^1,ui)]== INFINITY){
		m[opt_matpos2(uj^1,ui)] = pr->tmp[0];
	}
        else{
		m[opt_matpos2(uj^1,ui)] = min(m[opt_matpos2(uj^1,ui)], pr->tmp[0]); 
		count++;
	}
      }
      /*  c_i X_i + c_j X_j + [-a,b] <= 0 <=>  c_i X_i + c_j X_j <= a */
      if (c==AP_CONS_SUP) *exact = 0; /* not exact for strict constraints */
      oo->nni = min(max_nni,count);
      break;

    case OPT_OTHER:
      {
	/* general, approximated case */
	//////fprintf(stdout,"OTHERS\n");
        //////fflush(stdout);
	double tmpa = 0, tmpb = 0, Cb = 0, cb = 0;
	int Cinf = 0;            /* number of infinite upper bounds */
	int Cj1 = 0, Cj2 = 0; /* variable index with infinite bound */
	int cinf = 0;            /* number of infinite lower bounds */
	int cj1 = 0, cj2 = 0; /* variable index with infinite bound */

	*respect_closure = false; /* do not respect closure */

	count = oo->nni;
	/* compute 2 * upper bound, ignoring components leading to +oo */
	cb = 2*pr->tmp[0];
	Cb = 2*pr->tmp[1];
	for (j=0;j<dim;j++) {
	  double tmp[8];
	  double a_inf = m[opt_matpos(2*j,2*j+1)];
	  double a_sup = m[opt_matpos(2*j+1,2*j)];
	  double b_inf = pr->tmp[2*j + 2];
	  double b_sup = pr->tmp[2*j + 3];
	  if((a_sup == 0) || (b_sup == 0)){
		tmp[0] = 0;
		tmp[4] = 0;
	  }
	  else{
		  tmp[0] = a_sup * b_sup;
	  	  tmp[4] = -a_sup; 
		  tmp[4] = tmp[4] * b_sup;
	  }
	
	  if((a_inf == 0) || (b_inf == 0)){
		  tmp[1] = 0;
		  tmp[5] = 0;		
	  }
	  else{
	  	  tmp[1] = a_inf * b_inf;
	  	  tmp[5] = - a_inf;  
		  tmp[5] = tmp[5] * b_inf;
	 }

	 if((a_sup== 0) || (b_inf == 0)){
		  tmp[2] = 0;
		  tmp[6] = 0;
	 }
	 else{
	  	  tmp[6] = a_sup * b_inf;
	  	  tmp[2] = -a_sup;  
		  tmp[2] = tmp[2] * b_inf;
	}
	
	if((a_inf == 0) || (b_sup == 0)){
		  tmp[3] = 0;
		  tmp[7] = 0;
	}
	else{
	  	  tmp[7] = a_inf * b_sup;
	  	  tmp[3] = -a_inf;  
		  tmp[3] = tmp[3] * b_sup;
	}

  	  tmpb = max(tmp[0],tmp[1]);
  	  tmpb = max(tmpb,tmp[2]);
  	  tmpb = max(tmpb,tmp[3]);

  	  tmpa = max(tmp[4],tmp[5]);
  	  tmpa = max(tmpa,tmp[6]);
  	  tmpa = max(tmpa,tmp[7]);
	  
	  if (tmpa == INFINITY) { 
		cinf++; 
		cj2 = cj1; 
		cj1 = j; 
	  }
	  else {
		cb = cb + tmpa;
	  }
	  if (tmpb == INFINITY) { 
		Cinf++; 
		Cj2 = Cj1; 
		Cj1 = j; 
	  }
	  else {
		Cb = Cb + tmpb;
	  }
	  
	}
	
	/* upper bound */
	if (Cb == INFINITY) ;

	else if (!Cinf) {
	  /* no infinite bound: derive quadratic number of bounds */
	  //////fprintf(stdout,"Quadratic\n");
          //////fflush(stdout);
	  
	  for (j=0;j<dim;j++) {
	    if ((pr->tmp[2*j+2] <= -1) &&
		(m[opt_matpos(2*j+1,2*j)] != INFINITY)) {
	      /* -x <= expr-x <= max(expr) - max x */
	      tmpa = Cb - m[opt_matpos(2*j + 1, 2*j)];
	      uj = 2*j+1;
	    }
	    else if ((pr->tmp[2*j+3]<=-1) &&
		     (m[opt_matpos(2*j,2*j+1)] != INFINITY)) {
	      /* x <= expr+x <= max(expr) - max(-x) */
	      tmpa = Cb - m[opt_matpos(2*j, 2*j + 1 )];
	      uj = 2*j;
	    }
	    else continue;
	    
	    for (k=j+1;k<dim;k++) {
	      if ((pr->tmp[2*k+2]<=-1) &&
		  (m[opt_matpos(2*k+1,2*k)] != INFINITY)) {
		/* (+/-)x -y <= max(expr) - max((+/-)x) - max y */
		tmpb = tmpa - m[opt_matpos(2*k + 1, 2*k)];
		tmpb = tmpb/2;
		if(m[opt_matpos(2*k,uj)] ==INFINITY){
			m[opt_matpos(2*k,uj)] = tmpb;
			count++;
		}
		else{
			m[opt_matpos(2*k,uj)] = min(m[opt_matpos(2*k,uj)], tmpb);
		}
	      }
	      else if ((pr->tmp[2*k+3] <=-1) &&
		       (m[opt_matpos(2*k,2*k+1)] !=  INFINITY)) {
		/* (+/-)x +y <= max(expr) - max((+/-)x) - max (-y) */
		tmpb = tmpa - m[opt_matpos(2*k, 2*k + 1)];
		tmpb = tmpb/2;
		if(m[opt_matpos(2*k + 1,uj)]==INFINITY){
			m[opt_matpos(2*k + 1,uj)] = tmpb;
			count++;
		}
		else{
			m[opt_matpos(2*k + 1,uj)] = min(m[opt_matpos(2*k + 1,uj)],tmpb);
		}
	      }
	    }
	  }
	  oo->nni = min(max_nni,count);
	}

	else if (Cinf==1) {
	  /* one infinite bound: derive linear number of bounds */
          //////fprintf(stdout,"Linear\n");
          //////fflush(stdout);
	  if ((pr->tmp[2*Cj1+3] == -1) &&
	      (pr->tmp[2*Cj1+2] == 1)) uj = 2*Cj1;
	  else if ((pr->tmp[2*Cj1+3] == 1) &&
		   (pr->tmp[2*Cj1+2] == -1)) uj = 2*Cj1+1;
	  else goto Cbrk;
	  for (k=0;k<dim;k++) {
	    if (k==Cj1) continue;
	    if ((pr->tmp[2*k+2] <=-1) &&
		(m[opt_matpos(2*k+1,2*k)] != INFINITY)) {
	      /* (+/-)x -y <= max(expr) - max((+/-)x) - max y */
	      tmpb = Cb - m[opt_matpos(2*k + 1, 2*k)];
	      tmpb = tmpb/2;
	      if(m[opt_matpos2(2*k,uj)] == INFINITY){
		 m[opt_matpos2(2*k,uj)] = tmpb;
		 count++;
	      }
	      else{
	      	m[opt_matpos2(2*k,uj)] = min(m[opt_matpos2(2*k,uj)], tmpb);
	      }
	    }
	    else if ((pr->tmp[2*k+3] <=-1) &&
		     (m[opt_matpos(2*k,2*k+1)] != INFINITY)) {
	      /* (+/-)x +y <= max(expr) - max((+/-)x) - max (-y) */
	      tmpb = Cb - m[opt_matpos(2*k, 2*k + 1)];
	      tmpb = tmpb/2;
	      if(m[opt_matpos2(2*k + 1,uj)]==INFINITY){
		m[opt_matpos2(2*k + 1,uj)] = tmpb;
		count++;
	      }
	      else{
	      	m[opt_matpos2(2*k + 1,uj)] = min(m[opt_matpos2(2*k + 1,uj)], tmpb);
	      }
	    }
	  }
          oo->nni = min(max_nni,count);
	}

	else if (Cinf==2) {
	  /* two infinite bounds: derive just one bound */
          //////fprintf(stdout,"One\n");
          //////fflush(stdout);
	  if ((pr->tmp[2*Cj1+3]==-1) &&
	      (pr->tmp[2*Cj1+2]==1)) ui = 2*Cj1;
	  else if ((pr->tmp[2*Cj1+3] == 1) &&
		   (pr->tmp[2*Cj1+2] ==-1)) ui = 2*Cj1+1;
	  else goto Cbrk;
	  if ((pr->tmp[2*Cj2+3] == -1) &&
	      (pr->tmp[2*Cj2+2] == 1)) uj = 2*Cj2;
	  else if ((pr->tmp[2*Cj2+3] == 1) &&
		   (pr->tmp[2*Cj2+2] == -1)) uj = 2*Cj2+1;
	  else goto Cbrk;
	  tmpa = Cb/2;
	  if(m[opt_matpos2(uj^1,ui)]==INFINITY){
		m[opt_matpos2(uj^1,ui)] = tmpa;
		count++;
	  }
	  else{
	  	m[opt_matpos2(uj^1,ui)] = min(m[opt_matpos2(uj^1,ui)],tmpa);
	  }
	  oo->nni = min(max_nni,count);
	}
	
	/* if more than two infinite bounds: do nothing */

      Cbrk:
	//////fprintf(stdout, "OUTPUT\t%g\t%g\t%g\t%g\t%g\t%g\n",tmpa,tmpb,cb,Cb, Cinf, cinf);
      	  //print_opt_hmat(m,dim);
          //////fflush(stdout);
	/* lower bound */
	count = oo->nni;
	if (c==AP_CONS_EQ) {
		//////fprintf(stdout,"Equality\n");
          	//////fflush(stdout);
	if (cb == INFINITY) ;
	else if (!cinf) {
	  for (j=0;j<dim;j++) {
	    if ((pr->tmp[2*j+3] <= -1) &&
		(m[opt_matpos(2*j+1,2*j)] != INFINITY)) {
	      tmpa = cb - m[opt_matpos(2*j + 1, 2*j)];
	      uj = 2*j+1;
	    }
	    else if ((pr->tmp[2*j+2] <=-1) &&
		     (m[opt_matpos(2*j,2*j+1)] != INFINITY)) {
	      tmpa = cb - m[opt_matpos(2*j, 2*j + 1)];
	      uj = 2*j;
	    }
	    else continue;
	    for (k=j+1;k<dim;k++) {
	      if ((pr->tmp[2*k+3] <=-1) &&
		  (m[opt_matpos(2*k+1,2*k)] != INFINITY)) {
		tmpb = tmpa - m[opt_matpos(2*k + 1, 2*k)];
		tmpb = tmpb/2;
		if(m[opt_matpos(2*k, uj)]==INFINITY){
			m[opt_matpos(2*k, uj)] = tmpb;
			count++;
		}
		else{
			m[opt_matpos(2*k, uj)] = min(m[opt_matpos(2*k,uj)], tmpb);
		}
	      }
	      else if ((pr->tmp[2*k+2] <=-1) &&
		       (m[opt_matpos(2*k,2*k+1)] != INFINITY)) {
		tmpb = tmpa - m[opt_matpos(2*k, 2*k + 1)];
		tmpb = tmpb/2;
		if(m[opt_matpos(2*k + 1, uj)]==INFINITY){
			m[opt_matpos(2*k + 1, uj)] = tmpb;
			count++;
		}
		else{
			m[opt_matpos(2*k + 1, uj)] = min(m[opt_matpos(2*k + 1, uj)], tmpb);
		}
	      }
	    }
	  }
	  oo->nni = min(max_nni,count);
	}
	else if (cinf==1) {
	  if ((pr->tmp[2*cj1+2] ==-1) &&
	      (pr->tmp[2*cj1+3] == 1)) uj = 2*cj1;
	  else if ((pr->tmp[2*cj1+2] == 1) &&
		   (pr->tmp[2*cj1+3] ==-1)) uj = 2*cj1+1;
	  else goto cbrk;
	  for (k=0;k<dim;k++) {
	    if (k==cj1) continue;
	    if ((pr->tmp[2*k+3] <= -1) &&
		(m[opt_matpos(2*k+1,2*k)] != INFINITY)) {
	      tmpb = cb - m[opt_matpos(2*k + 1, 2*k)];
	      tmpb = tmpb/2;
	      if(m[opt_matpos2(2*k,uj)]==INFINITY){
		 m[opt_matpos2(2*k,uj)] = tmpb;
		 count++;
	      }
              else{	
	      	m[opt_matpos2(2*k,uj)] = min(m[opt_matpos2(2*k,uj)], tmpb);
	      }
	    }
	    else if ((pr->tmp[2*k+2] <= -1) &&
		     (m[opt_matpos(2*k,2*k+1)] != INFINITY)) {
	      tmpb = cb - m[opt_matpos(2*k, 2*k + 1)];
	      tmpb = tmpb/2;
	      if(m[opt_matpos2(2*k + 1,uj)]==INFINITY){
		 m[opt_matpos2(2*k + 1,uj)] = tmpb;
		 count++;
	      }
	      else{
	      	m[opt_matpos2(2*k + 1,uj)] = min(m[opt_matpos2(2*k + 1,uj)], tmpb);
	      }
	    }
	  }
	   oo->nni = min(max_nni,count);
	}
	else if (cinf==2) {
	  if ((pr->tmp[2*cj1+2]==-1) &&
	      (pr->tmp[2*cj1+3] == 1)) ui = 2*cj1;
	  else if ((pr->tmp[2*cj1+2] == 1) &&
		   (pr->tmp[2*cj1+3] ==-1)) ui = 2*cj1+1;
	  else goto cbrk;
	  if ((pr->tmp[2*cj2+2] == -1) &&
	      (pr->tmp[2*cj2+3] == 1)) uj = 2*cj2;
	  else if ((pr->tmp[2*cj2+2] == 1) &&
		   (pr->tmp[2*cj2+3] == -1)) uj = 2*cj2+1;
	  else goto cbrk;
	  tmpa = cb/2;
          if(m[opt_matpos2(uj^1,ui)]==INFINITY){
		m[opt_matpos2(uj^1,ui)] = tmpa;
		count++;
	  }
	  else{
	  	m[opt_matpos2(uj^1,ui)] = min(m[opt_matpos2(uj^1,ui)], tmpa);
	  }
	}
	 oo->nni = min(max_nni,count);
	}

      cbrk:
	*exact = 0;
      }
      break;

    default: assert(0);
    }
  }

  /* apply pending incremental closure now */
  if (*respect_closure && closure_pending)
    if (incr_closure(oo,temp1,temp2,dim,var_pending,is_int_flag)) return true;
  return false;
}





