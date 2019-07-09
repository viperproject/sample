#include "opt_oct_closure_comp_sparse.h"


bool strengthning_int_comp_sparse(opt_oct_mat_t * oo,  unsigned short int * ind1, double *temp, int n){
	double *result = oo->mat;
	array_comp_list_t *acl = oo->acl;
	int count = oo->nni;
	int s = 0;
	char *cm = (char *)calloc(n/2, sizeof(char));
	comp_list_t *cl = acl->head;
	unsigned short int num_comp = acl->size;
	char * jm = (char *)calloc(num_comp,sizeof(char));
	/****
		Corresponding to each component, store the index of list containint it. 
	*****/
	for(int l = 0; l < num_comp; l++){
		comp_t *c = cl->head;
		while(c!=NULL){
			unsigned short int num = c->num;
			cm[num] = l;
			c = c->next;
		}
		cl = cl->next;
	}
	int jc = 0;
	for(unsigned i = 0; i < n; i++){
		int ind = i + ((((i^1) + 1)*((i^1) + 1))/2);
		temp[i] = ceil(result[ind]/2);
		if(temp[i] != INFINITY){
			ind1[s+1] = i;
			/***
				get the component and look for the index of component list that contains it and then add that component list to the join list
			***/
			int cn = i/2;
			int cln = cm[cn];
			if(!jm[cln]){
				jm[cln] = 1;
				jc++;
			}
			s++;
		}
	}
	free(cm);
	if(jc > 1){
		cl = acl->head;
		int nr = 0;
		comp_list_t *jl = create_comp_list();
		char *map = (char *)calloc(n/2,sizeof(char)); 
		for(int l = 0; l < num_comp; l++){
			if(jm[l]){
				union_comp_list(jl,cl,map);
				comp_list_t * temp = cl;
				cl = cl->next;
				remove_comp_list(acl,temp);
				nr++;
			}
			else{
				cl = cl->next;
			}
		}
		insert_comp_list(acl,jl);
		free(map);
	}
	free(jm);
	ind1[0] = s;
	for(unsigned i = 0; i < ind1[0]; i++){
		unsigned i1 = ind1[i + 1];
		double t1 = temp[i1];
		for(unsigned j = 0; j < ind1[0];j++){
			unsigned j1 = ind1[j + 1];
			double t2 = temp[j1];
			int ind = j1 + ((((i1^1) + 1)*((i1^1) + 1))/2);
			//result[n*(i1^1) + j1] = min(result[n*(i1^1) + j1], t1 + t2);
			if(result[ind]!=INFINITY){
				result[ind] = min(result[ind], t1 + t2);
			}
			else{
				result[ind] = t1 + t2;
				count++;
			}
		}	
	}
	oo->nni = count;
	for(unsigned i = 0; i < n; i++){
		int ind = i + (((i+1)*(i+1))/2);
		if(result[ind] < 0){
			return true;
		}
		else{
			result[ind] = 0;
		}
	}
	return false;
}

bool strengthning_comp_sparse(opt_oct_mat_t *oo, unsigned short int * ind1, double *temp, int n){
	double *result = oo->mat;
	array_comp_list_t *acl = oo->acl;
	int s = 0;
	int count = oo->nni;
	char *cm = (char *)calloc(n/2,sizeof(char));
	
	comp_list_t * cl = acl->head;
	unsigned short int num_comp = acl->size;
	char * jm = (char *)calloc(num_comp,sizeof(char));
	/****
		Corresponding to each component, store the index of list containing it. 
	*****/
	for(int l = 0; l < num_comp; l++){
		comp_t *c = cl->head;
		while(c!=NULL){
			unsigned short int num = c->num;
			cm[num] = l;
			c = c->next;
		}
		cl = cl->next;
	}
	int jc = 0;
        cl = oo->acl->head;
        while(cl!=NULL){
		int comp_size = cl->size;
		comp_t *c = cl->head;
		for(unsigned i = 0; i < 2*comp_size; i++){
			int i1 = (i%2==0)? 2*c->num : 2*c->num+1;
			int ind = i1 + ((((i1^1) + 1)*((i1^1) + 1))/2);
			//temp[i] = result[n*(i^1) + i];
			temp[i1] = result[ind];
			if(temp[i1] != INFINITY){
				ind1[s+1] = i1;
				/***
					get the component and look for the index of component list that contains it and then add that component list to the join list
				***/
				int cn = i1/2;
				int cln = cm[cn];
				if(!jm[cln]){
					jm[cln] = 1;
					jc++;
				}
				s++;
			}
			if(i%2==1){
				c = c->next;
			}
		}
		cl = cl->next;
	}
	free(cm);
        /*****
		Code for Incremental Initialization
	******/
	if(jc > 1){
		comp_list_t * ci = acl->head;
		for(int i = 0; i < num_comp; i++){
			if(jm[i]){
				comp_list_t * cj = acl->head; 
				for(int j = 0; j < i; j++){
					if(!jm[j]){
						cj = cj->next;
						continue;
					}
					else{
						ini_comp_relations(result,ci,cj,n/2);
						cj = cj->next;
					}
				}
			}
			ci = ci->next;
		}
	}
	if(jc > 1){
		cl = acl->head;
		int nr = 0;
		comp_list_t *jl = create_comp_list();
		char *map = (char *)calloc(n/2,sizeof(char)); 
		for(int l = 0; l < num_comp; l++){
			if(jm[l]){
				union_comp_list(jl,cl,map);
				comp_list_t *temp = cl;
				cl = cl->next;
				remove_comp_list(acl,temp);
				nr++;
			}
			else{
				cl = cl->next;
			}
		}
		insert_comp_list(acl,jl);
		free(map);
	}
	
	free(jm);
	
	ind1[0] = s;
	for(unsigned i = 0; i < ind1[0]; i++){
		unsigned i1 = ind1[i + 1];
		double t1 = temp[i1];
		for(unsigned j = 0; j < ind1[0];j++){
			unsigned j1 = ind1[j + 1];
			if(j1 > (i1|1)){
				continue;
			}
			double t2 = temp[j1];
			int ind = j1 + ((((i1^1) + 1)*((i1^1) + 1))/2);
			//result[n*(i1^1) + j1] = min(result[n*(i1^1) + j1], (t1 + t2)/2);
			if(result[ind]!=INFINITY){
				result[ind] = min(result[ind], (t1 + t2)/2);
			}
			else{
				result[ind] = (t1+t2)/2;
				count++;
			}
		}	
	}
	oo->nni = count;
	cl = oo->acl->head;
	while(cl != NULL){
		comp_t * c = cl->head;
		unsigned short int comp_size = cl->size;
		for(unsigned short int i = 0; i < 2*comp_size; i++){
			int i1 = (i%2==0)? 2*c->num : 2*c->num+1;
			int ind = i1 + (((i1+1)*(i1+1))/2);
			if(result[ind] < 0){
				return true;
			}
			else{
				result[ind] = 0;
			}
			if(i1%2==1){
				c = c->next;
			}
		}
		cl = cl->next;
	}
	/*for(unsigned i = 0; i < n; i++){
		int ind = i + (((i+1)*(i+1))/2);
		if(result[ind] < 0){
			return true;
		}
		else{
			result[ind] = 0;
		}
	}*/
	
	return false;
}


void compute_index_comp_sparse(double *result, unsigned short int *ca, unsigned short int comp_size, unsigned short int *index1, unsigned short int *index2, unsigned short int k, int dim){
    int n = 2*dim;
    int m = 2*dim + 1;
    index1[0] = 0;
    index1[n + 1] = 0;
    index2[0] = 0;
    index2[n + 1] = 0;
    int s1 = 0, s2 = 0;
    int ind;
    //comp_t *ci = cl->head;
    //unsigned short int comp_size = cl->size;
   // unsigned short int * ca = to_sorted_array(cl,dim);
    for(int i = 0; i < 2*comp_size; i++){
	int i1 = (i%2==0) ? 2*ca[i/2] : 2*ca[i/2]+1;
	if(i1>=(2*k+2)){
		ind = (2*k) + (((i1 + 1)*(i1 + 1))/2); 
		//if(result[n*i + 2*k] != INFINITY){
		if(result[ind] != INFINITY){
		    index2[s1 + 1] = i1;
		    s1++;
		}
		ind = ((2*k)^1) + (((i1 + 1)*(i1 + 1))/2);  
		//if(result[n*i + ((2*k)^1)] != INFINITY){
		if(result[ind] != INFINITY){
		    index2[n + s2 + 2] = i1;
		    s2++;
		}
	}
    }
    index2[0] = s1;
    index2[n + 1] = s2;

    s1 = 0, s2 = 0;
    for(int j  = 0; j < 2*comp_size; j++){
	int j1 = (j%2==0) ? 2*ca[j/2] : 2*ca[j/2]+1;
	if(j1 < (2*k)){
		ind = j1 + ((((2*k) + 1)*((2*k) + 1))/2);
		//if(result[n*2*k + j] != INFINITY){
		if(result[ind] != INFINITY){
		    index1[s1 + 1] = j1;
		    s1++;
		}
		ind = j1 + (((((2*k)^1) + 1)*(((2*k)^1) + 1))/2);
		//if(result[n*((2*k)^1) + j] != INFINITY){
		if(result[ind] != INFINITY){
		    index1[n + s2 + 2] = j1;
		    s2++;
		}
	}
    }
    index1[0] = s1;
    index1[n + 1] = s2;
    //free(ca);
}



void print_index(unsigned short int *ind,int dim){
	int n = 2*dim;
	int s = ind[0];
	fprintf(stdout,"Size is %d\n",s);
	for(int i = 0; i < s; i++){
		fprintf(stdout,"%d ",ind[i+1]);
	}
	fprintf(stdout,"\n");
	s = ind[n+1];
	fprintf(stdout,"Size is %d\n",s);
	for(int i = 0; i < s; i++){
		fprintf(stdout,"%d ",ind[n + i +2]);
	}
	fprintf(stdout,"\n");
	fflush(stdout);
}

bool strong_closure_comp_sparse(opt_oct_mat_t *oo, double *temp1, double *temp2, unsigned short int *index1, unsigned short int *index2, int dim, bool is_int){
    
    double *result = oo->mat;
    array_comp_list_t *acl = oo->acl;
    int count = oo->nni;
    int n = 2*dim; 
    int m = 2*dim + 1;
    for(int i = 0; i < n; i++){
        temp1[i] = 0;
        temp2[i] = 0;
    }
    unsigned short int num_comp = acl->size;
    comp_list_t * cl = acl->head;
    
    for(int l = 0; l < num_comp; l++){
	    unsigned short int comp_size = cl->size;
	    unsigned short int * ca = to_sorted_array(cl,dim);
	    //comp_t * ck = cl->head;
	    for(int k = 0; k < comp_size; k++){
		//Compute index at start of iteration
		unsigned short int k1 = ca[k];
		//ck = ck->next;
		compute_index_comp_sparse(result,ca, comp_size, index1, index2, k1, dim);
		
		int s1 = index1[0],s2 = index1[n + 1];
		int s3 = index2[0], s4 = index2[n + 1];
		int pos1 = ((2*k1)^1) + ((((2*k1) + 1)*((2*k1) + 1))/2);
		//int pos2 = matpos2((2*k)^1, 2*k);
		int pos2 = (2*k1) + (((((2*k1)^1) + 1)*(((2*k1)^1) + 1))/2);
		//Compute (2k) and (2k+1)-th row and column, update the index
		if(result[pos1]!= INFINITY){
			  for(int i = 0; i < s3;i++){
				int i2 = index2[i + 1];
				int i1 = i2;
				//int ind2 = n*i2 + ((2*k)^1);
				//int ind1 = n*i1 + (2*k);
				int ind1 = (2*k1) + (((i1 + 1)*(i1 + 1))/2);
				int ind2 = ((2*k1)^1) + (((i2 + 1)*(i2 + 1))/2);

				if(result[ind2]!= INFINITY){
					result[ind2] = min(result[ind2],  result[pos1] + result[ind1]);
				}
				else{
					result[ind2] =  result[pos1] + result[ind1];
					//index1[m*i2 + index1[m*i2] + 1] = ((2*k)^1);
					index2[n + s4 + 2] = i2;
					//index1[m*i2]++;
					s4++;
					count++;
				}
				//temp2[i2] = result[ind2];
			}
			index2[n + 1] = s4;
		}
		
		for(int i = 2*k1+2; i < n; i++){
			int ind = ((2*k1)^1) + (((i+1)*(i+1))/2);
			//temp2[i] = result[n*i + ((2*k)^1)];
			temp2[i] = result[ind];
		}
	

		if(result[pos2] != INFINITY){
		
			for(int i = 0; i < s4; i++){
				int i2 = index2[n + i + 2];
				int i1 = i2;
				//int ind2 = n*i2 + ((2*k)^1);
				//int ind1 = n*i1 + (2*k);
				int ind1 = (2*k1) + (((i1 + 1)*(i1 + 1))/2);
				int ind2 = ((2*k1)^1) + (((i2 + 1)*(i2 + 1))/2);
				if(result[ind1] != INFINITY){
					result[ind1] = min(result[ind1], result[pos2] + result[ind2]);
				}
				else{
					result[ind1] = result[pos2] + result[ind2];
					//index1[m*i1 + index1[m*i1] + 1] = 2*k;
					index2[s3 + 1] = i1;
					//index1[m*i1]++;
					s3++;
					count++;
				}
				//temp1[i1] = result[ind1];
			}

			index2[0] = s3;
		}
		
		for(int i = 2*k1+2; i < n; i++){
			int ind = (2*k1) + (((i+1)*(i+1))/2);
			//temp1[i] = result[n*i + (2*k)];
			temp1[i] = result[ind];
		}

		if(result[pos2] != INFINITY){

			for(int j = 0; j < s1; j++){
				//int ind4 = get_index(n, 2*k,j);
				//int j1 = index1[m*(2*k) + j + 1];
				int j1 = index1[j + 1];
				int ind1 = j1 + ((((2*k1) + 1)*((2*k1) + 1))/2);
				int ind2 = j1 + (((((2*k1)^1) + 1)*(((2*k1)^1) + 1))/2);
				if(result[ind2] != INFINITY ){
					result[ind2] = min(result[ind2], result[pos2] + result[ind1]);
				}
				else{
					result[ind2] =  result[pos2] + result[ind1];
					index1[n + s2 + 2] = j1;
					//index2[m*j1 + index2[m*j1] + 1] = ((2*k)^1);
					s2++;
					count++;
					//index2[m*j1]++;
				} 
			}
			index1[n + 1] = s2;
		}

		if(result[pos1] != INFINITY){

			for(int j = 0; j < s2; j++){
				//int ind4 = get_index(n, 2*k,j);
				//int j1 = index1[m*((2*k)^1) + j + 1];
				int j1 = index1[n + j + 2];
				int ind1 = j1 + ((((2*k1) + 1)*((2*k1) + 1))/2);
				int ind2 = j1 + (((((2*k1)^1) + 1)*(((2*k1)^1) + 1))/2);
				//if(result[ind2] != std::numeric_limits<double>::infinity(
				if(result[ind1] != INFINITY ){
					result[ind1] = min(result[ind1], result[pos1] + result[ind2]);
				}
				else{
					result[ind1] =  result[pos1] + result[ind2];
					index1[s1 + 1] = j1;
					//index2[m*j1 + index2[m*j1] + 1] = (2*k);
					s1++;
					count++;
					//index2[m*j1]++;
				}
			}
			index1[0] = s1;
		}
	        
		//This is the Main Loop Divided into four parts
		//First Part through 2k+1
		int ind1_k = index1[0];
		int ind2_k = index1[n + 1];
		int ind3_k = index2[0];
		int ind4_k = index2[n + 1];
		for(int i = 0; i < ind1_k; i++){
			int i1 = index1[i + 1];
			int i2 = (i1%2==0) ? (i1 + 1): i1;
			int br = i2 < 2*k1 ? i2 : 2*k1 - 1;
			int ind1 = i1 + ((((2*k1) + 1)*((2*k1) + 1))/2);
			//double t1 = result[n*(2*k) + i1];
			double t1 = result[ind1];
			//double t2 = result[n*((2*k)^1) + (i^1)];
			//int j2 = (j/2)*2;
			for(int j = 0;j < ind2_k ; j++){
				//int ind2 = get_index(k,j);
		        	//int j1 = index1[m*((2*k)^1) + j + 1];
				int j1 = index1[n + j + 2];
				if(j1 > br){
					break;
					//continue;
				}
				int ind2 = j1 + (((((2*k1)^1) + 1)*(((2*k1)^1) + 1))/2);
				//double op1 = t1 + result[n*((2*k)^1) + j1];RDTSC(end);
        
				double op1 = t1 + result[ind2];
				//double op2 = t2 + result[n*(2*k) + j];
				//double op3 = min(op1, op2);
				int ind3 = j1 + ((((i1^1) + 1)*((i1^1) + 1))/2);
				//result[n*(i1^1) + j1] = min(result[n*(i1^1) + j1],op1 );
				if(result[ind3]!=INFINITY){
					result[ind3] = min(result[ind3],op1 );
				}
				else{
					result[ind3] = op1;
					count++;
				}
			}
			//for(int j = 0; j < index2[m*2*k]; j++){
			for(int j = 0;j < ind3_k; j++){
				int j1 = index2[j + 1];
				if(j1>i2){
					 break;
					//continue;
			    	}
				double op1 = t1 + temp1[j1];
				int ind3 = (j1^1) + ((((i1^1) + 1)*((i1^1) + 1))/2);
				//result[n*(i1^1) + (j1^1)] = min(result[n*(i1^1) + (j1^1)],op1 );
				if(result[ind3]!=INFINITY){
					result[ind3] = min(result[ind3],op1 );
				}
				else{
					result[ind3] = op1;
					count++;
				}
			}
			//}
		}
		
		//Second Part through 2k
		for(int i = 0; i < ind2_k; i++){
		    int i1 = index1[n + i + 2];
		    int i2 = (i1%2==0) ? (i1 + 1): i1;
		    int br = i2 < 2*k1 ? i2 : 2*k1 - 1;
		    //double t1 = result[n*(2*k) + i1];
		    int ind1 = i1 + (((((2*k1)^1) + 1)*(((2*k1)^1) + 1))/2);
		    //double t2 = result[n*((2*k)^1) + i1];
		    double t2 = result[ind1];
		    //int j2 = (j/2)*2;
		    for(int j = 0; j < ind1_k; j++){
			    int j1 = index1[j + 1];
			    if(j1 > br){
				break;
				//continue;
			    }
			    int ind2 = j1 + ((((2*k1) + 1)*((2*k1) + 1))/2);
		            //double op2 = t2 + result[n*(2*k) + j1];
			    double op2 = t2 + result[ind2];
			    int ind3 = j1 + ((((i1^1) + 1)*((i1^1) + 1))/2);
		            //result[n*(i1^1) + j1] = min(result[n*(i1^1) + j1],op2 );
			    if(result[ind3] !=INFINITY){
			    	result[ind3] = min(result[ind3],op2 );
			    }
			    else{
				result[ind3] = op2;
				count++;
			    }
		        }
		        //for(int j = 0; j < index2[m*((2*k)^1)]; j++){
			  for(int j = 0; j < ind4_k; j++){
			     int j1 = index2[n + j + 2];
			     if(j1>i2){
				break;
				//continue;
			    }
		            double op2 = t2 + temp2[j1];
			    int ind3 = (j1^1) + ((((i1^1) + 1)*((i1^1) + 1))/2);
		            //result[n*(i1^1) + (j1^1)] = min(result[n*(i1^1) + (j1^1)],op2 );
			    if(result[ind3]!=INFINITY){
			    	result[ind3] = min(result[ind3],op2 );
			    }
			    else{
				result[ind3] = op2;
				count++;
			    }
		        }
		    //}
		}
	    
		//Third Part i >= (2*k+1)
		for(int i = 0; i < ind4_k; i++){
		    int i1 = index2[n + i + 2];
		    int i2 = (i1%2==0) ? (i1 + 1): i1;
		    int br = i2 < 2*k1 ? i2 : 2*k1 - 1;
		    int ind1 = ((2*k1)^1) + (((i1 + 1)*(i1 + 1))/2);
		    //double t1 = result[n*i1 + ((2*k)^1)];
		    double t1 = result[ind1];
		   
		    for(int j = 0; j < ind2_k; j++){
		   	    //int j1 = index1[m*((2*k)^1) + j + 1];
			    int j1 = index1[n + j + 2];
			    if(j1 > br){
				break;
				//continue;
			    }
			    int ind2 = j1 + (((((2*k1)^1) + 1)*(((2*k1)^1) + 1))/2);
		            //double op1 = t1 + result[n*((2*k)^1) + j1];
			    double op1 = t1 + result[ind2];
			    int ind3 = j1 + (((i1 + 1)*(i1 + 1))/2);
		            //result[n*i1 + j1] = min(result[n*i1 + j1],op1 );
			    if(result[ind3]!=INFINITY){
			    	result[ind3] = min(result[ind3],op1 );
			    }
			    else{
				result[ind3] = op1;
				count++;
			    }
		        }
		        
		 	for(int j = 0; j < ind3_k ; j++){
		            
			    int j1 = index2[j + 1];
			    if(j1>i2){
				break;
				//continue;
			     }
		            double op1 = t1 + temp1[j1];
		            //double op1 = t1 + result[n*(j1) + 2*k];
		     	    int ind3 = (j1^1) + (((i1 + 1)*(i1 + 1))/2);
			    if(result[ind3]!=INFINITY){
			    	result[ind3] = min(result[ind3],op1 );
			    }
			    else{
				result[ind3] = op1;
				count++;
			    }
		        }
		}
		
		//Fourth Part i >= 2*k
		for(int i = 0; i < ind3_k; i++){
		    //int i1 = index2[m*(2*k) + i + 1];
		    int i1 = index2[i + 1];
		    int i2 = (i1%2==0) ? (i1 + 1): i1;
		    int br = i2 < 2*k1 ? i2 : 2*k1 - 1;
		    //double t2 = result[n*i1 + (2*k)];
		    int ind1 = (2*k1) + (((i1 + 1)*(i1 + 1))/2);
		    double t2 = result[ind1];
		    for(int j = 0; j < ind1_k; j++){
			    int j1 = index1[j + 1];
			    if(j1 > br){
				break;
				//continue;
			    }
			    
		            //double op2 = t2 + result[n*(2*k) + j1];
			    int ind2 = j1 + ((((2*k1) + 1)*((2*k1) + 1))/2);
			    double op2 = t2 + result[ind2];
			    int ind3 = j1 + (((i1 + 1)*(i1 + 1))/2);
		            //result[n*i1 + j1] = min(result[n*i1 + j1],op2 );
			    if(result[ind3]!=INFINITY){
			    	result[ind3] = min(result[ind3],op2 );
			    }
			    else{
				result[ind3] = op2;
				count++;
			    }
		        }
		       
		       // j >= 2*k
		       for(int j = 0; j < ind4_k ; j++){
			    int j1 = index2[n + j + 2];
			    if(j1>i2){
				break;
				//continue;
			    }
			    
		            double op2 = t2 + temp2[j1];
			    int ind3 = (j1^1) + (((i1 + 1)*(i1 + 1))/2);
		            //result[n*i1 + (j1^1)] = min(result[n*i1 + (j1^1)],op2 );
			    if(result[ind3]!=INFINITY){
			    	result[ind3] = min(result[ind3],op2 );
			    }
			    else{
				result[ind3] = op2;
				count++;
			    }
		        }
		}
		
	    }
	free(ca);
	cl = cl->next;
    }
    oo->nni = count;
    
    if(is_int){
		if(strengthning_int_comp_sparse(oo,index1,temp1,n)){
			return 1;
		}
    	}
    	else{
    		if(strengthning_comp_sparse(oo,index1,temp1,n)){
			return 1;
		}
    }
    //return strengthning_dense_scalar(result,temp1,n);
}

