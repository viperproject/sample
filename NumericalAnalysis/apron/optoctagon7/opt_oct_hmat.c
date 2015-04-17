#include <stdio.h>
#include <stdlib.h>
#include <immintrin.h>
#include "opt_oct_hmat.h"

opt_oct_mat_t* opt_hmat_alloc(int size){
	double *m = (double *)malloc(size*sizeof(double));
	opt_oct_mat_t *oo= (opt_oct_mat_t *)malloc(sizeof(opt_oct_mat_t));
	oo->mat = m;
	oo->nni = 0;
	oo->acl = create_array_comp_list();
	oo->is_dense = false;
	oo->ti = false;
	return oo;
}

void opt_hmat_free(opt_oct_mat_t *oo){
	
        free(oo->mat);
	if(!oo->is_dense){
		free_array_comp_list(oo->acl);
	}
	free(oo);
		
}

void top_mat(double *m, int dim){
	
	int n = 2*dim;
	int size = 2*dim*(dim+1);
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
	//memset(m,INFINITY,size);
	for(int i = 0; i < 2*dim; i++){
		int ind = i + (((i + 1)*(i + 1))/2);	
		m[ind] = 0.0;
	}
	
}

void convert_to_dense_mat(opt_oct_mat_t * oo, int dim, bool flag){
	
	double *src = oo->mat;
	for(int i = 0; i < dim; i++){
		comp_list_t * li = find(oo->acl,i);
		if(li==NULL){
			ini_self_relation(src,i,dim);
		}
	}
	for(int i = 0; i < dim; i++){
		comp_list_t * li = find(oo->acl,i);
		for(int j = 0; j <i; j++){
			comp_list_t * lj = find(oo->acl,j);
			if((li!=lj) || ((li==NULL) && (lj==NULL))){
				ini_relation(src,i,j,dim);
			}
			
		}
	}
	
	
	
	
}

opt_oct_mat_t * opt_hmat_alloc_top(int dim){
	
	double *m;
	int size = 2*dim*(dim + 1);
	//posix_memalign((void **)&m,32,size*sizeof(double));
	m = (double *)malloc(size*sizeof(double));
	assert(m);
	//double inf = 1.0/0.0;
	//top_mat(m,dim);
	/*int n = 2*dim;
	for(int i = 0; i < n; i++){
		int ind = i + (((i + 1)*(i + 1))/2);	
		m[ind] = 0.0;
	}*/
	opt_oct_mat_t * oo = (opt_oct_mat_t *)malloc(sizeof(opt_oct_mat_t));
	oo->mat = m;
	oo->nni = 2*dim;
	oo->acl = create_array_comp_list();
	oo->is_dense = false;
	oo->ti = false;
	oo->is_top = true;
		
	return oo;
}

opt_oct_mat_t *opt_hmat_copy(opt_oct_mat_t * src_mat, int dim){
	if(!src_mat){
		return NULL;
	}
	double *src = src_mat->mat;
	double *dest;
	int n = 2*dim;
	int size = 2*dim*(dim+1);
	//posix_memalign((void **)&dest,32,size*sizeof(double));
	dest = (double *)calloc(size,sizeof(double));
	
	double sparsity = 1- ((double)(src_mat->nni/size));
	opt_oct_mat_t * dst_mat = (opt_oct_mat_t *)malloc(sizeof(opt_oct_mat_t));
	if(!src_mat->is_dense){
		
		dst_mat->ti = false;
		//top_mat(dest,dim);
		comp_list_t * cl = src_mat->acl->head;
		while(cl!=NULL){
			unsigned short int comp_size = cl->size;
			unsigned short int * ca = to_sorted_array(cl,dim);
		
			for(int i = 0; i < 2*comp_size; i++){
				int i1 = (i%2==0)? 2*ca[i/2] : 2*ca[i/2] + 1;
				for(int j = 0; j < 2*comp_size; j++){
					int j1 = (j%2==0)? 2*ca[j/2]: 2*ca[j/2]+1;
					if(j1 > (i1|1)){
						break;
					}
					int ind = j1 + (((i1 + 1)*(i1 + 1))/2);	
					dest[ind] = src[ind];
				}
			}
			free(ca);
			cl = cl->next;
		}
		
		
	}
	else{
		dst_mat->ti = true;
		
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
	}	
	
	
	//memcpy(dest,src,size*sizeof(double));
	
	dst_mat->mat = dest;
	dst_mat->nni = src_mat->nni;
  	dst_mat->is_dense = src_mat->is_dense;
	dst_mat->is_top = src_mat->is_top;
	if(dst_mat->is_dense){
		dst_mat->acl = NULL;
	}
	else{
		dst_mat->acl = copy_array_comp_list(src_mat->acl);
	}
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

double recalculate_sparsity(opt_oct_mat_t *oo, int dim){
	int size = 2*dim*(dim+1);
	int count = 0;
	double *m = oo->mat;
	for(int i = 0; i < size; i++){
		if(m[i]!=INFINITY){
			count++;
		}
	}
	oo->nni = count;
	return 1- ((double)(oo->nni/(double)size));
}

bool opt_hmat_strong_closure(opt_oct_mat_t *oo, int dim){
	double *temp1, *temp2;
	unsigned short int *ind1, *ind2;
	temp1 = (double *)malloc(2*dim*sizeof(double));
	temp2 = (double *)malloc(2*dim*sizeof(double));
	bool flag = is_int_flag ? true : false;
	bool res;
	double size = 2*dim*(dim+1);
	double sparsity = 1- ((double)(oo->nni/size));
	if(sparsity >= sparse_threshold){
		if(oo->is_dense){
			oo->is_dense = false;
			oo->acl = extract(oo->mat,dim);
		}
		ind1 = (unsigned short int *)malloc(2*(2*dim + 1)*sizeof(unsigned short int));
		ind2 = (unsigned short int *)malloc(2*(2*dim + 1)*sizeof(unsigned short int));
		res = strong_closure_comp_sparse(oo,temp1,temp2,ind1,ind2,dim,flag);
		free(ind1);
		ind1 = NULL;
		free(ind2);
		ind2 = NULL;
	}
	else{
		
		sparsity = recalculate_sparsity(oo,dim);
		
		if(sparsity >= sparse_threshold){
			if(oo->is_dense){
				oo->is_dense = false;
				oo->acl = extract(oo->mat,dim);
			}
			ind1 = (unsigned short int *)calloc(2*(2*dim + 1), sizeof(unsigned short int));
			ind2 = (unsigned short int *)calloc(2*(2*dim + 1), sizeof(unsigned short int));
			res = strong_closure_comp_sparse(oo,temp1,temp2,ind1, ind2,dim, flag);
			free(ind1);
			ind1 = NULL;
			free(ind2);
			ind2 = NULL;
		}
		else{
			if(!oo->is_dense){
				oo->is_dense = true;
				if(!oo->ti){
					oo->ti = true;
					convert_to_dense_mat(oo,dim,false);
					
				}
				
				free_array_comp_list(oo->acl);
			}
			
			#if defined(VECTOR)
				res = strong_closure_dense(oo,temp1,temp2,dim, flag);
			#else
				res = strong_closure_dense_scalar(oo,temp1,temp2,dim, flag);
			#endif
		}
	}
        free(temp1);
	temp1 = NULL;
	free(temp2);
        temp2 = NULL;
	sparsity = 1- ((double)(oo->nni/size));
	return res;

}

bool is_top_avx_half_double(opt_oct_mat_t *oo, int dim){
	
	double *m = oo->mat;
	int size = 2*dim*(dim + 1);
	int n = 2*dim;
	bool flag = true;
	if(!oo->is_dense){
		if(oo->acl->size==0){
			 
			return true;
		}
		/****
			Since we compute only the over approximation of connected components, we may have to check explicity
		*****/
		comp_list_t * cl = oo->acl->head;
		while(cl!=NULL){
			unsigned short int * ca = to_sorted_array(cl,dim);
			unsigned short int comp_size = cl->size;
			for(unsigned short int i = 0; i < 2*comp_size; i++){
				unsigned short int i1 = (i%2==0)? 2*ca[i/2] : 2*ca[i/2]+1;
				for(unsigned short int j = 0; j < 2*comp_size;j++){
					unsigned short int j1 = (j%2==0)? 2*ca[j/2] : 2*ca[j/2]+1;
					if(j1 > (i1|1)){
						break;
					}
					if(i1==j1){
						continue;
					}
					else{
						int ind = j1 + ((i1 + 1)*(i1 + 1))/2;
						if(m[ind]!=INFINITY){
							flag = false;
							break;
						}
					}
				}
			}
			cl = cl->next;
		}
		/****
			remove the connected components
		****/
		if(flag){
			clear_array_comp_list(oo->acl);	
		}
	}
	else{
		
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
		
	}
	return flag;
}


bool is_equal_avx_half_double(opt_oct_mat_t *oo1, opt_oct_mat_t *oo2, int dim){
	double *m1= oo1->mat;
	double *m2 = oo2->mat;
	int size = 2*dim*(dim + 1);
	if(!oo1->is_dense && !oo2->is_dense){
		/***
			If oo1 and oo2 do not contain the same set of connected components then they cannot be equal
		***/
		if(!is_equal_array_comp_list(oo1->acl,oo2->acl,dim)){
			return false;
		}
		/****
			If we come here we know that both oo1 and oo2 contain the same set of connected components
		*****/
		
		comp_list_t *cl = oo1->acl->head;
		
		/***
			To Improve Cache Performance
		****/
		
		while(cl!=NULL){
			unsigned short int comp_size = cl->size;
			unsigned short int * ca = to_sorted_array(cl,dim);
			for(int i = 0; i < 2*comp_size; i++){
				int i1 = (i%2==0)? 2*ca[i/2] : 2*ca[i/2] + 1;
				for(int j = 0; j < 2*comp_size; j++){
					int j1 = (j%2==0)? 2*ca[j/2]: 2*ca[j/2]+1;
					if(j1 > (i1|1)){
						break;
					}
					int ind = j1 + (((i1 + 1)*(i1 + 1))/2);	
					if(m1[ind]!=m2[ind]){
						free(ca);
						return false;
					}
				}
			}
			free(ca);
			cl= cl->next;
		}
		
  		fflush(stdout);
	}
	else{
		/***
			Both Operands will be closed so dense already
		****/
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
		
	}
	
	return true;
}

bool is_lequal_avx_half_double(opt_oct_mat_t *oo1, opt_oct_mat_t *oo2, int dim){
	double *m1 = oo1->mat;
	double *m2 = oo2->mat;
	int size = 2*dim*(dim + 1);
	if(!oo1->is_dense && !oo2->is_dense){
		int il = is_lequal_array_comp_list(oo2->acl,oo1->acl,dim);
		if(!il){
			return false;
		}
		array_comp_list_t *acl = intersection_array_comp_list(oo1->acl, oo2->acl,dim);
		comp_list_t *cl = acl->head;
		while(cl != NULL){ 
			unsigned short int comp_size = cl->size;
			unsigned short int * ca = to_sorted_array(cl,dim);
		
			for(int i = 0; i < 2*comp_size; i++){
				int i1 = (i%2==0)? 2*ca[i/2] : 2*ca[i/2] + 1;
				for(int j = 0; j < 2*comp_size; j++){
					int j1 = (j%2==0)? 2*ca[j/2]: 2*ca[j/2]+1;
					if(j1 > (i1|1)){
						break;
					}
					int ind = j1 + (((i1 + 1)*(i1 + 1))/2);	
					if(m1[ind] > m2[ind]){
						return false;
					}
				}
			}
			free(ca);
			cl = cl->next;
		}
	}
	else{
		/***
			Second Operand is not closed so we have to make it dense if it is sparse
		****/
		if(!oo2->is_dense){
			if(!oo2->ti){
				oo2->ti = true;
				convert_to_dense_mat(oo2,dim,false);
			}
			
		}
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
	}
	return true;
}

void meet_avx_half(opt_oct_mat_t *oo, opt_oct_mat_t *oo1, opt_oct_mat_t *oo2, int dim, bool destructive){
	double *m = oo->mat;
	double *m1 = oo1->mat;
	double *m2 = oo2->mat;
	int size = 2*dim*(dim + 1);
	int n = 2*dim;
	if(!oo1->is_dense && !oo2->is_dense){
		array_comp_list_t * temp = oo1->acl;
		array_comp_list_t * acl = union_array_comp_list(oo1->acl,oo2->acl,dim);		
		oo->is_dense = false;
		if(!destructive){
			oo->ti = false;
			//top_mat(m,dim);
		}
		comp_list_t * cl = acl->head;
		while(cl!=NULL){
			unsigned short int comp_size = cl->size;
			unsigned short int * ca = to_sorted_array(cl,dim);
			
			if(!oo1->ti){
				
				for(int i = 0; i < comp_size; i++){
					int i1 = ca[i];
					for(int j = 0; j <=i; j++){
						int j1 = ca[j];
						handle_binary_relation(m1,oo1->acl,i1,j1,dim);
					}
				}
			}
			if(!oo2->ti){
				for(int i = 0; i < comp_size; i++){
					int i1 = ca[i];
					for(int j = 0; j <=i; j++){
						int j1 = ca[j];
						handle_binary_relation(m2,oo2->acl,i1,j1,dim);
					}
				}
			}
			
			for(int i = 0; i < 2*comp_size; i++){
				int i1 = (i%2==0)? 2*ca[i/2] : 2*ca[i/2] + 1;
				for(int j = 0; j < 2*comp_size; j++){
					int j1 = (j%2==0)? 2*ca[j/2]: 2*ca[j/2]+1;
					if(j1 > (i1|1)){
						break;
					}
					
					int ind = j1 + (((i1 + 1)*(i1 + 1))/2);	
					m[ind] = min(m1[ind],m2[ind]);
				}
			}
			free(ca);
			cl = cl->next;
		}
		oo->acl = acl;
		if(destructive){
			free_array_comp_list(temp);
		}
	}
	else{
		/*****
			None of the operands are closed so we have to change them to dense if they are sparse
		******/
		if(!oo2->is_dense){
			if(!oo2->ti){
				oo2->ti = true;
				convert_to_dense_mat(oo2,dim,false);
			}
			
		}
		if(!oo1->is_dense){
			if(!oo1->ti){
				oo1->ti = true;
				convert_to_dense_mat(oo1,dim,false);
			}
			
		}
		
		if(!destructive){
			oo->ti = true;
			free_array_comp_list(oo->acl);
		}
		oo->is_dense = true;
		
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
				m[i] = min(m1[i],m2[i]);
			}
		#endif
			for(int i = (size/4)*4; i < size; i++){
				m[i] = min(m1[i],m2[i]);
			}
	}
	
	
	if(oo->is_dense){
		oo->nni = 2*dim*(dim+1);
	}
	else{
		comp_list_t *cl = oo->acl->head;
		int count = 2*dim;
		while(cl!=NULL){
			count = count + 2*(cl->size)*(cl->size);
			cl = cl->next;
		}
		oo->nni = count;
	}	
	
}

void forget_array_avx_half(opt_oct_mat_t *oo, ap_dim_t *arr,int dim, int arr_dim, bool project){
	
	double *m = oo->mat;
	array_comp_list_t * acl = oo->acl;
	for(int i = 0; i < arr_dim; i++){
		ap_dim_t d = 2*arr[i];
		/*****
			Handle Connected Components here
			
		******/
		if(!oo->is_dense){
			comp_list_t *cl = find(acl,arr[i]);
			if(cl!=NULL){
				remove_comp(cl,arr[i]);
			}
		}
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
			m[d1 + (d+1)] = 0;
			m[d + d2] = 0;
			/*****
				Handle Connected Components in case of Project
			******/
			if(!oo->is_dense){
				comp_list_t * cj = create_comp_list();
				insert_comp(cj,arr[i]);
				insert_comp_list(acl,cj);
			}
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

void join_avx_half(opt_oct_mat_t *oo, opt_oct_mat_t *oo1, opt_oct_mat_t *oo2, int dim, bool destructive){
	double *m = oo->mat;
	double *m1 = oo1->mat;
	double *m2 = oo2->mat;
	//int count = 0;
	
	int size = 2*dim*(dim + 1);
	int n = 2*dim;
	if(!(oo1->is_dense) && !(oo2->is_dense)){
		array_comp_list_t * temp = oo1->acl;
		oo->acl = intersection_array_comp_list(oo1->acl,oo2->acl,dim);
		oo->is_dense = false;
		
		if(destructive){
			free_array_comp_list(temp);
		}
		if(!destructive){
			oo->ti = false;
			//top_mat(m,dim);
		}
		comp_list_t * cl = oo->acl->head;
		while(cl!=NULL){
			unsigned short int comp_size = cl->size;
			unsigned short int * ca = to_sorted_array(cl,dim);
			for(int i = 0; i < 2*comp_size; i++){
				int i1 = (i%2==0)? 2*ca[i/2] : 2*ca[i/2] + 1;
				for(int j = 0; j < 2*comp_size; j++){
					int j1 = (j%2==0)? 2*ca[j/2]: 2*ca[j/2]+1;
					if(j1 > (i1|1)){
						break;
					}
					int ind = j1 + (((i1 + 1)*(i1 + 1))/2);	
					m[ind] = max(m1[ind],m2[ind]);
				}
			}
			free(ca);
			cl = cl->next;
		}
	}
	else{
		if(!oo2->is_dense){
			if(!oo2->ti){
				oo2->ti = true;
				convert_to_dense_mat(oo2,dim,false);
			}
			
		}
		if(!oo1->is_dense){
			if(!oo1->ti){
				oo1->ti = true;
				convert_to_dense_mat(oo1,dim,false);
			}
			
		}
		/*if(destructive && !(oo1->is_dense)){
			free_array_comp_list(oo1->acl);
		}*/
		if(!destructive){
			oo->ti = true;
			free_array_comp_list(oo->acl);
		}
		oo->is_dense = true;
		
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
				m[i] = max(m1[i],m2[i]);
			}
		#endif
			for(int i = (size/4)*4; i <size; i++){
				m[i] = max(m1[i],m2[i]);
			}
	}
	oo->nni = min(oo1->nni,oo2->nni);
}

void opt_hmat_addrem_dimensions(opt_oct_mat_t * dst_mat, opt_oct_mat_t* src_mat,
			    ap_dim_t* pos, int nb_pos,
			    int mult, int dim, bool add)
{
  int i,j,new_j,org_j;
  new_j = org_j = pos[0]*2;
  double * dst = dst_mat->mat;
  double * src = src_mat->mat;
  unsigned short int * map = (unsigned short int *)calloc(dim+1,sizeof(unsigned short int));
  ap_dim_t * add_pos = (ap_dim_t *)calloc(nb_pos,sizeof(ap_dim_t));
  int new_dim;
  
  if(nb_pos){
	  
	  if(add){
		/****
			Exact number of non infinities for add 
		****/
		new_dim = dim + max(nb_pos,mult);
		int max_nni = 2*new_dim*(new_dim +1);
		dst_mat->nni = min(max_nni,src_mat->nni + 2*nb_pos);
		/******
			Build Map of Connected Components for add
		******/
	
		int l = 0,k= 0,p =0,ac=0;
		
		for(int i = 0; i <=dim; i++){
			//
			while((l < nb_pos) &&(i==pos[l])){
				/***
					For expand mult can be greater than 1 in which case nb_pos will be 1
				****/
				add_pos[ac] = p;
				p = p + mult;
				ac++;
				l++;
			}
		
			map[k] = p;
			k++;
			p++;
		}
	
	  }
	  else{
		/**** 
			Approximation of number of non infinities for remove 
		****/
	
		 new_dim = dim - nb_pos;
		int new_size = 2*new_dim*(new_dim+1);
		dst_mat->nni = min(src_mat->nni - 2*nb_pos,new_size);
		dst_mat->nni = max(2*new_dim,dst_mat->nni);
		/******
			Build Map of Connected Components for remove
		******/
		int l = 0;
		for(int i = 0; i < dim; i++){
			if((l < nb_pos) && (i==pos[l])){
				map[i] = new_dim;
				l++;
			}
			else{
				map[i] = i - l;
			}
		}
	  }
		
		/*****
			Handle Connected Components
		******/
		
		if(!src_mat->is_dense){
			array_comp_list_t * acl1 = src_mat->acl;	
			array_comp_list_t * acl2 = dst_mat->acl;
			comp_list_t *cl1 = acl1->head;
			while(cl1!=NULL){
				comp_list_t *cl2 = create_comp_list();
				comp_t * c1 = cl1->head;
				while(c1!=NULL){
					unsigned short int num = c1->num;
					if(map[num]!=new_dim){
						insert_comp(cl2,map[num]);
					}
					c1 = c1->next;
				}
				if(cl2->size > 0){
					insert_comp_list(acl2,cl2);
				}
				else{
					free_comp_list(cl2);
				}
				cl1 = cl1->next;
			}
		}
		
     }
     dst_mat->is_dense = src_mat->is_dense;
     if(!src_mat->is_dense){
		if(nb_pos){
			
			array_comp_list_t * acl = src_mat->acl;
			comp_list_t *cl = acl->head;
			while(cl!=NULL){
				unsigned short int * ca = to_sorted_array(cl,dim);
				unsigned short int comp_size = cl->size;
				for(unsigned short int i = 0; i < comp_size; i++){
					int i1 = ca[i];
					int ni = map[i1];
					if(ni==new_dim){
						continue;
					}
				
					for(unsigned short int j = 0; j < comp_size; j++){
						int j1 = ca[j];
						if(j1>(i1|1)){
							break;
						}
						int nj = map[j1];
						if(nj==new_dim){
							continue;
						}
						dst[opt_matpos2(2*ni,2*nj)] = src[opt_matpos2(2*i1,2*j1)];
						dst[opt_matpos2(2*ni,2*nj+1)] = src[opt_matpos2(2*i1,2*j1+1)];
						dst[opt_matpos2(2*ni+1,2*nj)] = src[opt_matpos2(2*i1+1,2*j1)];
						dst[opt_matpos2(2*ni+1,2*nj+1)] = src[opt_matpos2(2*i1+1,2*j1+1)];
					}
				}
				cl = cl->next;
			}
			
		}
		dst_mat->ti = false;
     }
     else{
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
	  	//////fflush(stdout);
		/* next line */
		org_c += size_org_line;
		new_c += size_new_line;
	      }
	    }
	  }
	
	if(add){
		forget_array_avx_half(dst_mat,add_pos,new_dim,nb_pos,false);
		
		for(i = 0; i < nb_pos; i++){
			int i1 = add_pos[i];
			int i2 = i1*2;
			int i3 = i2 + 1;
			int ind = i2 + (((i2+1)*(i2+1))/2);
			dst[ind] = 0;
			ind = i3 + (((i3+1)*(i3+1))/2);
			dst[ind] = 0;
		}
		
	}	
	dst_mat->ti = true;
      }
	
	
  	free(map);
        free(add_pos);
  
}



void opt_hmat_permute(opt_oct_mat_t* dest_mat, opt_oct_mat_t* src_mat,
		  int dst_dim, int src_dim,
		  ap_dim_t* permutation)
{
  double *dst = dest_mat->mat;
  double *src = src_mat->mat; 
  if(!src_mat->is_dense){
	  //top_mat(dst,dst_dim);
	  dest_mat->ti = false;
	  array_comp_list_t * acl1 = src_mat->acl;
  	  comp_list_t * cl1 = acl1->head;
  	  
	  while(cl1 != NULL){
		unsigned short int comp_size = cl1->size;
		unsigned short int * ca1 = to_sorted_array(cl1,src_dim);
		for(int i = 0; i < comp_size; i++){
			int i1 = ca1[i];
			int new_ii = 2*permutation[i1];
			for(int j = 0; j <= i; j++){
				int j1 = ca1[j];
				//if(j1 > (i1|1)){
					//break;
				//}
				int new_jj = 2*permutation[j1];
				int d_ind = opt_matpos2(new_ii,new_jj);
				int s_ind = opt_matpos2(2*i1,2*j1);
				dst[d_ind] = src[s_ind];	
				d_ind = opt_matpos2(new_ii,new_jj+1);
				s_ind = opt_matpos2(2*i1,2*j1+1);
				dst[d_ind] = src[s_ind];
				d_ind = opt_matpos2(new_ii+1,new_jj);
				s_ind = opt_matpos2(2*i1+1,2*j1);
				dst[d_ind] = src[s_ind];
				d_ind = opt_matpos2(new_ii+1,new_jj+1);
				s_ind = opt_matpos2(2*i1+1,2*j1+1);
				dst[d_ind] = src[s_ind];
			}
		}
		free(ca1);
		cl1 = cl1->next;
	  }
	/*****
		Handle Connected Components
  	******/
  
 	array_comp_list_t * acl2 = dest_mat->acl;
  	cl1 = acl1->head;
  	
  	while(cl1 != NULL){
		comp_list_t * cl2 = create_comp_list();
		comp_t * c1 = cl1->head;
		while(c1 != NULL){
			unsigned short int num = c1->num;
			insert_comp(cl2,permutation[num]);
			c1 = c1->next;
		}
		
		insert_comp_list(acl2,cl2);
		cl1 = cl1->next;
 	 } 
  }
  else{
	  
	  dest_mat->ti = true;
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
	      dst[opt_matpos2(new_ii,new_jj+1)] = src[1];
	      dst[opt_matpos2(new_ii+1,new_jj)] = src[2*(i+1)];
	      dst[opt_matpos2(new_ii+1,new_jj+1)] = src[2*(i+1)+1];*/
		dst[ind1] = src[0];
		dst[ind2] = src[1];
		dst[ind3] = src[2*(i + 1)];
		dst[ind4] = src[2*(i + 1) + 1];
	    }
	    src+=2*(i+1);
	  }
  }
  dest_mat->nni = src_mat->nni;
  dest_mat->is_dense = src_mat->is_dense;
  
}

void widening_half(opt_oct_mat_t *oo, opt_oct_mat_t *oo1, opt_oct_mat_t *oo2, int dim){
	double *m = oo->mat;
	double *m1 = oo1->mat;
	double *m2 = oo2->mat;
	int count = 0;
	int size = 2*dim*(dim+1);
	if(!oo1->is_dense && !oo2->is_dense){
		oo->is_dense = false;
		oo->ti = false;
		oo->acl = intersection_array_comp_list(oo1->acl,oo2->acl,dim);
		comp_list_t *cl = oo->acl->head;
		while(cl!=NULL){
			unsigned short int comp_size = cl->size;
			unsigned short int * ca = to_sorted_array(cl,dim);
			for(int i = 0; i < 2*comp_size; i++){
				int i1 = (i%2==0)? 2*ca[i/2] : 2*ca[i/2] + 1;
				for(int j = 0; j < 2*comp_size; j++){
					int j1 = (j%2==0)? 2*ca[j/2] : 2*ca[j/2] + 1;
					if(j1 > (i1|1)){
						break;
					}
					int ind = opt_matpos2(i1,j1);
					if(m1[ind] >=m2[ind]){
						m[ind] = m1[ind];
					}
					else{
						m[ind] = INFINITY;
					}
					if(m[ind]!=INFINITY){
						count++;
					}
				}
			}
			free(ca);
			cl = cl->next;
		}
	}
	else{
		if(!oo2->is_dense){
			if(!oo2->ti){
				oo2->ti = true;
				convert_to_dense_mat(oo2,dim,false);
			}
			
		}
		if(!oo1->is_dense){
			if(!oo1->ti){
				oo1->ti = true;
				convert_to_dense_mat(oo1,dim,false);
			}
			
		}
		oo->is_dense = true;
		oo->ti = true;
		free_array_comp_list(oo->acl);
		for(int i = 0; i <size; i++){
			if(m1[i] >= m2[i]){
				m[i] = m1[i];
			}
			else{
				m[i] = INFINITY;
			}
			if(m[i] != INFINITY){
				count++;
			}
		}
	}
	oo->nni = count;
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
  bool (*incr_closure)(opt_oct_mat_t * ,...);
  double size = 2*dim*(dim+1);
  double sparsity = 1- ((double)(oo->nni)/size);
  if(sparsity >=sparse_threshold){
	if(oo->is_dense){
		oo->is_dense = false;
		oo->acl = extract(oo->mat,dim);
	}
	incr_closure = &incremental_closure_comp_sparse;
  }
  else{ 
	if(!oo->is_dense){
		if(!oo->ti){
			oo->ti = true;
			convert_to_dense_mat(oo,dim,false);
		}
		oo->is_dense = true;
		free_array_comp_list(oo->acl);
	}
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
    array_comp_list_t *acl;
    case OPT_UNARY:
	
      /* can we delay incremental closure further? */
      if (*respect_closure && closure_pending && var_pending!=u.i) {
	if (incr_closure(oo,temp1,temp2,dim,var_pending, is_int_flag)) return true;
      }
      count = oo->nni;
      closure_pending = 1;
      var_pending = u.i;
      /******
		Handle Connected Components for Unary Case
      *******/
      if(!oo->is_dense){
	      acl = oo->acl;
	      if(find(acl,u.i)==NULL){
			comp_list_t * cl = create_comp_list();
			insert_comp(cl,u.i);
			insert_comp_list(acl,cl);
			if(!oo->ti){
				ini_relation(m,u.i,u.i,dim);
			}
	      }
      }
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
	
	if (incr_closure(oo,temp1,temp2,dim,var_pending, is_int_flag)) return true;
      }
      closure_pending = 1;
      var_pending = (var_pending==u.j) ? u.j : u.i;
      count = oo->nni;
      /******
	Handle the connected components for Binary Case for sparse matrices
      ****/
      if(!oo->is_dense){
	      acl = oo->acl;
	      comp_list_t * li = find(acl,u.i);
	      comp_list_t * lj = find(acl,u.j);
	      if(li==NULL){
		  if(!oo->ti){
		  	ini_relation(m,u.i,u.i,dim);
		  }
		  if(lj==NULL){
			comp_list_t *cli = create_comp_list();
		  	insert_comp(cli,u.i);
			insert_comp(cli,u.j);
			insert_comp_list(acl,cli);
			if(!oo->ti){
				ini_relation(m,u.j,u.j,dim);
				ini_relation(m,u.i,u.j,dim);
			}
		  }
		  else{
			if(!oo->ti)
			ini_comp_elem_relation(m,lj,u.i,dim);
			insert_comp(lj,u.i);
		
		  }
		  
	      }
	      else{
			if(lj==NULL){
				if(!oo->ti){
					ini_relation(m,u.j,u.j,dim);
					ini_comp_elem_relation(m,li,u.j,dim);
				}
				insert_comp(li,u.j);
			
			}
			else{
				if(li!=lj){
					if(!oo->ti){
						ini_comp_relations(m,li,lj,dim);
					}
					union_comp_list_direct(li,lj,dim);
					remove_comp_list(acl,lj);
				
				}
			}
	      }
     }
      /*****
		Handling of connected components ends here
      ******/
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
	  
	  double b_inf = pr->tmp[2*j + 2];
	  double b_sup = pr->tmp[2*j + 3];
	  if((b_inf==0) && (b_sup==0)){
		continue;
	  }
	  if((!oo->ti) && (!oo->is_dense) && (find(oo->acl,j)==NULL)){
		ini_relation(m,j,j,dim);
	  }
	  double a_inf = m[opt_matpos(2*j,2*j+1)];
	  double a_sup = m[opt_matpos(2*j+1,2*j)];
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
	 /******
		Handle Connected Components in case Quadratic Number of Bounds are Created
	  *****/
	  //acl = oo->acl;
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
	     comp_list_t * cj;
	     if(!oo->is_dense){
	    	 cj = create_comp_list();
	     }
	    for (k=j+1;k<dim;k++) {
	      
	      if ((pr->tmp[2*k+2]<=-1) &&
		  (m[opt_matpos(2*k+1,2*k)] != INFINITY)) {
		/* (+/-)x -y <= max(expr) - max((+/-)x) - max y */
		tmpb = tmpa - m[opt_matpos(2*k + 1, 2*k)];
		tmpb = tmpb/2;
		if(!oo->is_dense && !oo->ti){
			handle_binary_relation_list(m,oo->acl,cj,j,k,dim);
		}
		if(m[opt_matpos(2*k,uj)] ==INFINITY){
			m[opt_matpos(2*k,uj)] = tmpb;
			count++;
		}
		else{
			m[opt_matpos(2*k,uj)] = min(m[opt_matpos(2*k,uj)], tmpb);
		}
		if(!oo->is_dense){
			insert_comp(cj,k);
		}
	      }
	      else if ((pr->tmp[2*k+3] <=-1) &&
		       (m[opt_matpos(2*k,2*k+1)] !=  INFINITY)) {
		/* (+/-)x +y <= max(expr) - max((+/-)x) - max (-y) */
		tmpb = tmpa - m[opt_matpos(2*k, 2*k + 1)];
		tmpb = tmpb/2;
		if(!oo->is_dense && !oo->ti){
			handle_binary_relation_list(m,oo->acl,cj,j,k,dim);
		}
		if(m[opt_matpos(2*k + 1,uj)]==INFINITY){
			m[opt_matpos(2*k + 1,uj)] = tmpb;
			count++;
		}
		else{
			m[opt_matpos(2*k + 1,uj)] = min(m[opt_matpos(2*k + 1,uj)],tmpb);
		}
		if(!oo->is_dense){
			insert_comp(cj,k);
		}

	      }
	       /******
			Add Components to the new list
	       *******/
		
	    }
		/***
			Insert newly created Comp List in Array
		***/
		if(!oo->is_dense){
			if(cj->size > 0){
				insert_comp(cj,j);
				insert_comp_list_with_union(oo->acl,cj,dim);
			}
			else{
				free_comp_list(cj);
			}
		}
	  }
	  oo->nni = min(max_nni,count);
	}

	else if (Cinf==1) {
	  /* one infinite bound: derive linear number of bounds */
	  if ((pr->tmp[2*Cj1+3] == -1) &&
	      (pr->tmp[2*Cj1+2] == 1)) uj = 2*Cj1;
	  else if ((pr->tmp[2*Cj1+3] == 1) &&
		   (pr->tmp[2*Cj1+2] == -1)) uj = 2*Cj1+1;
	  else goto Cbrk;
	  /****
		Handle the Connected Components in case Linear Number of Bounds are Created 
	  *****/
	  //acl = oo->acl;
	  comp_list_t * cl;
	  if(!oo->is_dense){
	  	cl = create_comp_list();
	  }
	  for (k=0;k<dim;k++) {
	    if (k==Cj1) continue;
	    
	    if ((pr->tmp[2*k+2] <=-1) &&
		(m[opt_matpos(2*k+1,2*k)] != INFINITY)) {
	      /* (+/-)x -y <= max(expr) - max((+/-)x) - max y */
	      tmpb = Cb - m[opt_matpos(2*k + 1, 2*k)];
	      tmpb = tmpb/2;
	      if(!oo->is_dense && !oo->ti){
	      	  handle_binary_relation_list(m,oo->acl,cl,Cj1,k,dim);
	      }
	      if(m[opt_matpos2(2*k,uj)] == INFINITY){
		 m[opt_matpos2(2*k,uj)] = tmpb;
		 count++;
	      }
	      else{
	      	m[opt_matpos2(2*k,uj)] = min(m[opt_matpos2(2*k,uj)], tmpb);
	      }
		if(!oo->is_dense){
			insert_comp(cl,k);
		}
	    }
	    else if ((pr->tmp[2*k+3] <=-1) &&
		     (m[opt_matpos(2*k,2*k+1)] != INFINITY)) {
	      /* (+/-)x +y <= max(expr) - max((+/-)x) - max (-y) */
	      tmpb = Cb - m[opt_matpos(2*k, 2*k + 1)];
	      tmpb = tmpb/2;
	      if(!oo->is_dense && !oo->ti){
	      	     handle_binary_relation_list(m,oo->acl,cl,Cj1,k,dim);
	      }
	      if(m[opt_matpos2(2*k + 1,uj)]==INFINITY){
		m[opt_matpos2(2*k + 1,uj)] = tmpb;
		count++;
	      }
	      else{
	      	m[opt_matpos2(2*k + 1,uj)] = min(m[opt_matpos2(2*k + 1,uj)], tmpb);
	      }
	 	if(!oo->is_dense){
			insert_comp(cl,k);
		}
	    }
	    /*****
		Add components to new List
	    *****/
	    
	  }
	   /*****
		Insert into Component Array
	   *****/
	   if(!oo->is_dense){
		   if(cl->size > 0){
			insert_comp(cl,Cj1);
			insert_comp_list_with_union(oo->acl,cl,dim);
		   }
		   else{
			free_comp_list(cl);
		   }
	   }
          oo->nni = min(max_nni,count);
	}

	else if (Cinf==2) {
	  /* two infinite bounds: derive just one bound */
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
	  /*****
		Handle Connected Components in case Only One bound is created
	  ******/
	        if(!oo->is_dense){
			acl = oo->acl;
			comp_list_t *li = find(acl,Cj1);
			comp_list_t *lj = find(acl,Cj2);
			if(li==NULL){
				if(!oo->ti){
					ini_relation(m,Cj1,Cj1,dim);
				}
				if(lj==NULL){
				comp_list_t *cli = create_comp_list();
					if(!oo->ti){
						ini_relation(m,Cj2,Cj2,dim);
						ini_relation(m,Cj1,Cj2,dim);
					}
					insert_comp(cli,Cj1);
					insert_comp(cli,Cj2);
					insert_comp_list(acl,cli);
				}
				else{
					if(!oo->ti){
						ini_comp_elem_relation(m,lj,Cj1,dim);
					}
					insert_comp(lj,Cj1);
				}
			
			}
			else{
				if(lj==NULL){
					if(!oo->ti){
						ini_relation(m,Cj2,Cj2,dim);
						ini_comp_elem_relation(m,li,Cj2,dim);
					}
					insert_comp(li,Cj2);
				
				}
				else{
					if(li!=lj){
						if(!oo->ti){
							ini_comp_relations(m,li,lj,dim);
						}
						union_comp_list_direct(li,lj,dim);
						remove_comp_list(acl,lj);
					}
				}
			}
	        }
	   /*******
		   Handling ends here
	  *******/
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
	/* lower bound */
	count = oo->nni;
	if (c==AP_CONS_EQ) {
		
	if (cb == INFINITY) ;
	else if (!cinf) {
	   /******
		Handle Connected Components in case Quadratic Number of Bounds are Created
	  *****/
	 // acl = oo->acl;
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
            
	    comp_list_t * cj;
	    if(!oo->is_dense){
	    	cj = create_comp_list();
	    }
	    for (k=j+1;k<dim;k++) {
	      
	      if ((pr->tmp[2*k+3] <=-1) &&
		  (m[opt_matpos(2*k+1,2*k)] != INFINITY)) {
		tmpb = tmpa - m[opt_matpos(2*k + 1, 2*k)];
		tmpb = tmpb/2;
		if(!oo->is_dense && !oo->ti){
			handle_binary_relation_list(m,oo->acl,cj,j,k,dim);
		}
		if(m[opt_matpos(2*k, uj)]==INFINITY){
			m[opt_matpos(2*k, uj)] = tmpb;
			count++;
		}
		else{
			m[opt_matpos(2*k, uj)] = min(m[opt_matpos(2*k,uj)], tmpb);
		}
		if(!oo->is_dense){
			insert_comp(cj,k);
		}
	      }
	      else if ((pr->tmp[2*k+2] <=-1) &&
		       (m[opt_matpos(2*k,2*k+1)] != INFINITY)) {
		tmpb = tmpa - m[opt_matpos(2*k, 2*k + 1)];
		tmpb = tmpb/2;
		if(!oo->is_dense && !oo->ti){
			handle_binary_relation_list(m,oo->acl,cj,j,k,dim);
		}
		if(m[opt_matpos(2*k + 1, uj)]==INFINITY){
			m[opt_matpos(2*k + 1, uj)] = tmpb;
			count++;
		}
		else{
			m[opt_matpos(2*k + 1, uj)] = min(m[opt_matpos(2*k + 1, uj)], tmpb);
		}
		if(!oo->is_dense){
			insert_comp(cj,k);
		}
	      }
		/******
			Add Components to the new list
	       *******/
		
	    }
		/******
			Add newly created list into the array
		*******/
		if(!oo->is_dense){
			if(cj->size > 0){
				insert_comp(cj,j);
				insert_comp_list_with_union(oo->acl,cj,dim);
			}
			else{
				free_comp_list(cj);
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
	   /****
		Handle the Connected Components in case Linear Number of Bounds are Created 
	  *****/
	  //acl = oo->acl;	
	   comp_list_t *cl;
	   if(!oo->is_dense){
	   	cl = create_comp_list();
	   }
	   for (k=0;k<dim;k++) {
	     if (k==cj1) continue;
	   
	     if ((pr->tmp[2*k+3] <= -1) &&
		(m[opt_matpos(2*k+1,2*k)] != INFINITY)) {
	      tmpb = cb - m[opt_matpos(2*k + 1, 2*k)];
	      tmpb = tmpb/2;
	      if(!oo->is_dense && !oo->ti){
	      	handle_binary_relation_list(m,oo->acl,cl,cj1,k,dim);
	      }
	      if(m[opt_matpos2(2*k,uj)]==INFINITY){
		 m[opt_matpos2(2*k,uj)] = tmpb;
		 count++;
	      }
              else{	
	      	m[opt_matpos2(2*k,uj)] = min(m[opt_matpos2(2*k,uj)], tmpb);
	      }
		if(!oo->is_dense){
			insert_comp(cl,k);
		}
	    }
	    else if ((pr->tmp[2*k+2] <= -1) &&
		     (m[opt_matpos(2*k,2*k+1)] != INFINITY)) {
	      tmpb = cb - m[opt_matpos(2*k, 2*k + 1)];
	      tmpb = tmpb/2;
	      //Incremental initialization
	      if(!oo->is_dense && !oo->ti){
	      	handle_binary_relation_list(m,oo->acl,cl,cj1,k,dim);
	      }
	      if(m[opt_matpos2(2*k + 1,uj)]==INFINITY){
		 m[opt_matpos2(2*k + 1,uj)] = tmpb;
		 count++;
	      }
	      else{
	      	m[opt_matpos2(2*k + 1,uj)] = min(m[opt_matpos2(2*k + 1,uj)], tmpb);
	      }
		if(!oo->is_dense){
			insert_comp(cl,k);
		}
	    }
	        /*****
		     Add components to new List
	        *****/
		
	  }
	   /*****
		Insert into Component Array
	   *****/
	   if(!oo->is_dense){
		   if(cl->size > 0){
			insert_comp(cl,cj1);
			insert_comp_list_with_union(oo->acl,cl,dim);
		   }
		   else{
			free_comp_list(cl);
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
	  /******
		Handle Connected Components in Case only One bound is created
	  ******/
		if(!oo->is_dense){
			acl = oo->acl;
			comp_list_t *li = find(acl,cj1);
			comp_list_t *lj = find(acl,cj2);
			if(li==NULL){
				if(!oo->ti){
					ini_relation(m,cj1,cj1,dim);
				}
				if(lj==NULL){
					comp_list_t *cli = create_comp_list();
					if(!oo->ti){
						ini_relation(m,cj2,cj2,dim);
						ini_relation(m,cj1,cj2,dim);
					}
					insert_comp(cli,cj1);
					insert_comp(cli,cj2);
					insert_comp_list(acl,cli);
				}
				else{
					if(!oo->ti){
						ini_comp_elem_relation(m,lj,cj1,dim);
					}
					insert_comp(lj,cj1);
				}
			
			}
			else{
				if(lj==NULL){
					if(!oo->ti){
						ini_relation(m,cj2,cj2,dim);
						ini_comp_elem_relation(m,li,cj2,dim);
					}
					insert_comp(li,cj2);
				}
				else{
					if(li!=lj){
						if(!oo->ti){
							ini_comp_relations(m,li,lj,dim);
						}
						union_comp_list_direct(li,lj,dim);
						remove_comp_list(acl,lj);
					
					}
				}
			}
	     }
	  /*******
			Handling ends here
	  *******/
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





