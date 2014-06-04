#include "opt_oct_incr_closure_sparse.h"

double incremental_closure_calc_perf_sparse(double cycles, int dim){
  int n = 2*dim;
  return  (7*n*n)/cycles;
}


int incremental_closure_opt_sparse(double *m, double *temp1, double *temp2, int dim, int v, bool is_int){
	int n = 2*dim;
	int ii = 2*v + 1;
	int j;
	int *index1, *index2;
	//posix_memalign((void **)&index1, 32, 2*(2*dim + 1)*sizeof(int));
	//posix_memalign((void **)&index2, 32, 2*(2*dim + 1)*sizeof(int));
	index1 = (int *)calloc(2*(2*dim + 1),sizeof(int));
	index2 = (int *)calloc(2*(2*dim + 1),sizeof(int));
	for(unsigned k = 0; k < n; k=k + 2){
		////fprintf(stdout, "%d\n",k);
  		//print(m, dim);
		//fflush(stdout);
		int v1 = 2*v;
		int v2 = 2*v + 1;
		int v1v2 = v2 + (((v1 + 1)*(v1 + 1))/2);
		int v2v1 = v1 + (((v2 + 1)*(v2 + 1))/2);
		int kk = (k^1);
		int br1 = k < v1 ? k : v1;
		int br2 = kk < v1 ? kk : v1;
		for(unsigned i = 2*v; i < 2*v + 2; i++){
			//double ik = m[n*i + k];
			int ind_ik, ind_ikk;
			if(k <=i){
				ind_ik = k + (((i + 1)*(i + 1))/2);
			}
			else{
				ind_ik = (i^1) + ((((k^1) + 1)*((k^1) + 1))/2);
			}
			
			if(kk <=i){
				ind_ikk = kk + (((i + 1)*(i + 1))/2);
			}
			else{
				ind_ikk = (i^1) + ((((kk^1) + 1)*((kk^1) + 1))/2);
			}
			double ik = m[ind_ik];
			double ikk = m[ind_ikk];
			//double ki = m[n*k + i];
			int ind_ki, ind_kki;
			if ( k <= i){
				ind_ki = (k^1) + ((((i^1) + 1)*((i^1) + 1))/2);
			}
			else{
				ind_ki = i + (((k + 1)*(k + 1))/2);
			}

			if ( kk <= i){
				ind_kki = (kk^1) + ((((i^1) + 1)*((i^1) + 1))/2);
			}
			else{
				ind_kki = i + (((kk + 1)*(kk + 1))/2);
			}
			
			//int ind_ki = i + (((k + 1)*(k + 1))/2);
			double ki = m[ind_ki];	
			double kki = m[ind_kki];
			if(ik != INFINITY){		
				for(j = 0; j <br1; j++){
					//double kj = m[n*k + j];
					int ind_kj = j + (((k + 1)*(k + 1))/2);
					double kj = m[ind_kj];
					//double jk = m[n*j + k];
					//int ind_jk = k + (((j + 1)*(j + 1))/2);
					//double jk = m[ind_jk];
					//m[n*i + j] = min(m[n*i + j], ik + kj);
					int ind_ij = j + (((i + 1)*(i + 1))/2);
					m[ind_ij] = min(m[ind_ij], ik + kj);
					//m[n*j + i] = min(m[n*j + i], jk + ki);
				}
			}

			if(ik != INFINITY){
				for(; j < v1; j++){
					//double kj = m[n*k + j];
					int ind_kj = (k^1) + ((((j^1) + 1)*((j^1) + 1))/2);
					double kj = m[ind_kj];
					//double jk = m[n*j + k];
					int ind_ij = j + (((i + 1)*(i + 1))/2);
					//m[n*i + j] = min(m[n*i + j], ik + kj);
					m[ind_ij] = min(m[ind_ij], ik + kj);
					//m[n*j + i] = min(m[n*j + i], jk + ki);
				}
			}

			if(ki != INFINITY){
				for(j= 2*v + 2; j < k; j++ ){
					int ind_jk = (j^1) + ((((k^1) + 1)*((k^1) + 1))/2);
					double jk = m[ind_jk];
					int ind_ji = i + (((j + 1)*(j + 1))/2);
					m[ind_ji] = min(m[ind_ji], jk + ki);
				}
			}

			if(ki != INFINITY){
				for(; j < 2*dim; j++){
					int ind_jk = k + (((j + 1)*(j + 1))/2);
					double jk = m[ind_jk];
					int ind_ji = i + (((j + 1)*(j + 1))/2);
					m[ind_ji] = min(m[ind_ji], jk + ki);
				}
			}

			if(ikk != INFINITY){
				for(j = 0; j <br1; j++){
					//double kj = m[n*k + j];
					int ind_kkj = j + (((kk + 1)*(kk + 1))/2);
					double kkj = m[ind_kkj];
					//double jk = m[n*j + k];
					//int ind_jk = k + (((j + 1)*(j + 1))/2);
					//double jk = m[ind_jk];
					//m[n*i + j] = min(m[n*i + j], ik + kj);
					int ind_ij = j + (((i + 1)*(i + 1))/2);
					m[ind_ij] = min(m[ind_ij], ikk + kkj);
					//m[n*j + i] = min(m[n*j + i], jk + ki);
				}
			}
			if(ikk != INFINITY){
				for(; j < v1; j++){
					//double kj = m[n*k + j];
					int ind_kkj = (kk^1) + ((((j^1) + 1)*((j^1) + 1))/2);
					double kkj = m[ind_kkj];
					//double jk = m[n*j + k];
					int ind_ij = j + (((i + 1)*(i + 1))/2);
					//m[n*i + j] = min(m[n*i + j], ik + kj);
					m[ind_ij] = min(m[ind_ij], ikk + kkj);
					//m[n*j + i] = min(m[n*j + i], jk + ki);
				}
			}
	
			if(kki != INFINITY){
				for(j= 2*v + 2; j < k; j++ ){
					int ind_jkk = (j^1) + ((((kk^1) + 1)*((kk^1) + 1))/2);
					double jkk = m[ind_jkk];
					int ind_ji = i + (((j + 1)*(j + 1))/2);
					m[ind_ji] = min(m[ind_ji], jkk + kki);
				}
			}

			if(kki != INFINITY){
				for(; j < 2*dim; j++){
					int ind_jkk = kk + (((j + 1)*(j + 1))/2);
					double jkk = m[ind_jkk];
					int ind_ji = i + (((j + 1)*(j + 1))/2);
					m[ind_ji] = min(m[ind_ji], jkk + kki);
				}
			}
			
		}
		m[v1v2] = min(m[v1v2],m[opt_matpos2(v1,k)] + m[opt_matpos2(k,v2)]);
		m[v1v2] = min(m[v1v2],m[opt_matpos2(v1,kk)] + m[opt_matpos2(kk,v2)]);
		m[v2v1] = min(m[v2v1],m[opt_matpos2(v2,k)] + m[opt_matpos2(k,v1)]);
		m[v2v1] = min(m[v2v1],m[opt_matpos2(v2,kk)] + m[opt_matpos2(kk,v1)]);
		
	}

	int v1 = (2*v);
	int v2 = (2*v)^1;
	int vi = (((v1 + 1)*(v1 + 1))/2);
	int vvi = (((v2 + 1)*(v2 + 1))/2);
	int pos1 = v2 + vi;
	//int pos2 = opt_matpos2((2*k)^1, 2*k);
	int pos2 = v1 + vvi;
	//variable v in pivot position
	for(int i = v1 + 2; i < n;i++){
		int ind1 = v2 + (((i+1)*(i+1))/2);
		int ind2 = v1 + (((i+1)*(i+1))/2);
		if(m[pos1]!= INFINITY)
			m[ind1] = min(m[ind1], m[ind2] + m[pos1] );
		temp2[i] = m[ind1];
	}
	for(int i = v1 + 2; i < n; i++){
		int ind1 = v2 + (((i+1)*(i+1))/2);
		int ind2 = v1 + (((i+1)*(i+1))/2);
		if(m[pos2]!=INFINITY)
			m[ind2] = min(m[ind2], m[ind1] + m[pos2] );
		temp1[i] = m[ind2];
	}

	if(m[pos2]!=INFINITY){
		for(int j = 0; j < v1; j++){
			//int ind3 = opt_matpos2((2*k)^1,j);
			int ind3 = j + vvi;
			//int ind4 = opt_matpos2( 2*k,j);
			int ind4 = j + vi;
			//result[n*((2*k)^1) + j] = min(result[n*((2*k)^1) + j], result[n*((2*k)^1) + 2*k] + result[n*(2*k) + j]);
			m[ind3] = min(m[ind3], m[pos2] + m[ind4]);
		}
	}

	if(m[pos1] != INFINITY){
		for(int j = 0; j < v1; j++){
			//int ind3 = opt_matpos2((2*k)^1,j);
			int ind3 = j + vvi;
			//int ind4 = opt_matpos2(2*k,j);
			int ind4 = j + vi;
			//result[n*2*k + j] = min(result[n*2*k + j], result[n*2*k + ((2*k)^1)] + result[n*((2*k)^1) + j]);
			m[ind4] = min(m[ind4], m[pos1] + m[ind3]);
		}
	}

	compute_index_sparse(m,index1,index2,v,dim);
	//for(unsigned k = 2*v; k < 2*v + 2; k++){
	int ind1_k = index1[0];
	int ind2_k = index1[n + 1];
	int ind3_k = index2[0];
	int ind4_k = index2[n + 1];

	//First part	
	for(int i = 0; i < ind1_k; i++){
		int i1 = index1[i + 1];
        	int i2 = (i1%2==0) ? (i1 + 1): i1;
		int br = i2 < 2*v ? i2 : 2*v - 1;
		int ind1 = i1 + ((((2*v) + 1)*((2*v) + 1))/2);
		//double t1 = result[n*(2*k) + i1];
		double t1 = m[ind1];
		//double t2 = result[n*((2*k)^1) + (i^1)];
		//int j2 = (j/2)*2;
		for(int j = 0;j < ind2_k ; j++){
			//int ind2 = get_index(k,j);
                	//int j1 = index1[m*((2*k)^1) + j + 1];
			int j1 = index1[n + j + 2];
			if(j1 > br){
				break;
			}
			int ind2 = j1 + (((((2*v)^1) + 1)*(((2*v)^1) + 1))/2);
			//double op1 = t1 + result[n*((2*k)^1) + j1];
			double op1 = t1 + m[ind2];
			//double op2 = t2 + result[n*(2*k) + j];
			//double op3 = min(op1, op2);
			int ind3 = j1 + ((((i1^1) + 1)*((i1^1) + 1))/2);
			//result[n*(i1^1) + j1] = min(result[n*(i1^1) + j1],op1 );
			m[ind3] = min(m[ind3],op1 );
		}
		//for(int j = 0; j < index2[m*2*k]; j++){
		for(int j = 0;j < ind3_k; j++){
			int j1 = index2[j + 1];
			if(j1>i2){
				 break;
		    	}
			double op1 = t1 + temp1[j1];
			//double op2 = t2 + temp2[j];
			//double op3 = min(op1, op2);
			int ind3 = (j1^1) + ((((i1^1) + 1)*((i1^1) + 1))/2);
			//result[n*(i1^1) + (j1^1)] = min(result[n*(i1^1) + (j1^1)],op1 );
			m[ind3] = min(m[ind3],op1 );
		}
		//}
	}
        
	//Second Part
        for(int i = 0; i < ind2_k; i++){
	    int i1 = index1[n + i + 2];
            int i2 = (i1%2==0) ? (i1 + 1): i1;
	    int br = i2 < 2*v ? i2 : 2*v - 1;
            //double t1 = result[n*(2*k) + i1];
	    int ind1 = i1 + (((((2*v)^1) + 1)*(((2*v)^1) + 1))/2);
            //double t2 = result[n*((2*k)^1) + i1];
	    double t2 = m[ind1];
            //int j2 = (j/2)*2;
            for(int j = 0; j < ind1_k; j++){
		    int j1 = index1[j + 1];
		    if(j1 > br){
			break;
		    }
		    int ind2 = j1 + ((((2*v) + 1)*((2*v) + 1))/2);
                    //double op2 = t2 + result[n*(2*k) + j1];
		    double op2 = t2 + m[ind2];
		    int ind3 = j1 + ((((i1^1) + 1)*((i1^1) + 1))/2);
                    //result[n*(i1^1) + j1] = min(result[n*(i1^1) + j1],op2 );
		    m[ind3] = min(m[ind3],op2 );
                }
                //for(int j = 0; j < index2[m*((2*k)^1)]; j++){
		  for(int j = 0; j < ind4_k; j++){
		     int j1 = index2[n + j + 2];
		     if(j1>i2){
			break;
		    }
                    double op2 = t2 + temp2[j1];
		    int ind3 = (j1^1) + ((((i1^1) + 1)*((i1^1) + 1))/2);
                    //result[n*(i1^1) + (j1^1)] = min(result[n*(i1^1) + (j1^1)],op2 );
		    m[ind3] = min(m[ind3],op2 );
                }
            //}
        }
    
	//Third Part
        for(int i = 0; i < ind4_k; i++){
	    int i1 = index2[n + i + 2];
            int i2 = (i1%2==0) ? (i1 + 1): i1;
            int br = i2 < 2*v ? i2 : 2*v - 1;
	    int ind1 = ((2*v)^1) + (((i1 + 1)*(i1 + 1))/2);
            //double t1 = result[n*i1 + ((2*k)^1)];
	    double t1 = m[ind1];
            //double t2 = result[n*((2*k)^1) + (i^1)];
            //int j2 = (j/2)*2;
           
            for(int j = 0; j < ind2_k; j++){
           	    //int j1 = index1[m*((2*k)^1) + j + 1];
		    int j1 = index1[n + j + 2];
		    if(j1 > br){
			break;
		    }
		    int ind2 = j1 + (((((2*v)^1) + 1)*(((2*v)^1) + 1))/2);
                    //double op1 = t1 + result[n*((2*k)^1) + j1];
		    double op1 = t1 + m[ind2];
		    int ind3 = j1 + (((i1 + 1)*(i1 + 1))/2);
                    //result[n*i1 + j1] = min(result[n*i1 + j1],op1 );
		    m[ind3] = min(m[ind3],op1 );
                }
                
         	for(int j = 0; j < ind3_k ; j++){
                    
		    int j1 = index2[j + 1];
		    if(j1>i2){
			break;
		     }
		    //cout<<result[n*(j1^1) + 2*k]<<"\t"<<result[n*j1 + 2*k]<<"\n";
                    double op1 = t1 + temp1[j1];
                    //double op1 = t1 + result[n*(j1) + 2*k];
             	    int ind3 = (j1^1) + (((i1 + 1)*(i1 + 1))/2);
                    //result[n*i1 + (j1^1)] = min(result[n*i1 + (j1^1)],op1 );
		    m[ind3] = min(m[ind3],op1 );
                }
            //}
        }
        
        //Fourth Part
        for(int i = 0; i < ind3_k; i++){
            //int i1 = index2[m*(2*k) + i + 1];
	    int i1 = index2[i + 1];
            int i2 = (i1%2==0) ? (i1 + 1): i1;
	     int br = i2 < 2*v ? i2 : 2*v - 1;
            //double t1 = result[n*(2*k) + i1];
            //double t2 = result[n*i1 + (2*k)];
	    int ind1 = (2*v) + (((i1 + 1)*(i1 + 1))/2);
	    double t2 = m[ind1];
            for(int j = 0; j < ind1_k; j++){
		    int j1 = index1[j + 1];
		    if(j1 > br){
			break;
		    }
                    //double op2 = t2 + result[n*(2*k) + j1];
		    int ind2 = j1 + ((((2*v) + 1)*((2*v) + 1))/2);
		    double op2 = t2 + m[ind2];
                    //double op3 = min(op1, op2);
		    int ind3 = j1 + (((i1 + 1)*(i1 + 1))/2);
                    //result[n*i1 + j1] = min(result[n*i1 + j1],op2 );
		    m[ind3] = min(m[ind3],op2 );
                }
               
                
               for(int j = 0; j < ind4_k ; j++){
		    int j1 = index2[n + j + 2];
		    if(j1>i2){
			break;
		    }
                    double op2 = t2 + temp2[j1];
                    //double op3 = min(op1, op2);
		    int ind3 = (j1^1) + (((i1 + 1)*(i1 + 1))/2);
                    //result[n*i1 + (j1^1)] = min(result[n*i1 + (j1^1)],op2 );
		    m[ind3] = min(m[ind3],op2 );
                }
           // }
        }

  	//incr_closure_time += cycles;
	bool res;
  	//fprintf(stdout,"Incr_Closure Time is\t%g\n",incr_closure_time);	
	if(is_int){
		
		res = strengthning_int_sparse(m, index1,temp1, n);
		
	}
	else{
        	res = strengthning_sparse(m, index1, temp1, n);
	}
	free(index2);
	free(index1);
	return res;
}
