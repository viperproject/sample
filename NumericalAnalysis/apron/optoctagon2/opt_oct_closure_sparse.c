
#include "opt_oct_closure_sparse.h"

//#include <boost/test/included/unit_test.hpp>
#include <assert.h>

#define MIN(a,b) a < b ? a : b
#define U_i 32
#define U_j 32

double strong_closure_calc_perf_sparse(double cycles, int dim){
  double n = 2*dim;
  return  (n*n*n)/cycles;
}

void compute_index_sparse(double *result, int *index1, int *index2, int k, int dim){
    int n = 2*dim;
    int m = 2*dim + 1;
    index1[0] = 0;
    index1[n + 1] = 0;
    index2[0] = 0;
    index2[n + 1] = 0;
    int s1 = 0, s2 = 0;
    int ind;
    for(int i = 2*k + 2; i < n; i++){
	ind = (2*k) + (((i + 1)*(i + 1))/2); 
        //if(result[n*i + 2*k] != INFINITY){
	if(result[ind] != INFINITY){
            index2[s1 + 1] = i;
            s1++;
        }
        ind = ((2*k)^1) + (((i + 1)*(i + 1))/2);  
        //if(result[n*i + ((2*k)^1)] != INFINITY){
	if(result[ind] != INFINITY){
            index2[n + s2 + 2] = i;
            s2++;
        }
        
    }
    index2[0] = s1;
    index2[n + 1] = s2;

    s1 = 0, s2 = 0;
    for(int j = 0; j < 2*k; j++){
	ind = j + ((((2*k) + 1)*((2*k) + 1))/2);
        //if(result[n*2*k + j] != INFINITY){
	if(result[ind] != INFINITY){
            index1[s1 + 1] = j;
            s1++;
        }
	ind = j + (((((2*k)^1) + 1)*(((2*k)^1) + 1))/2);
        //if(result[n*((2*k)^1) + j] != INFINITY){
	if(result[ind] != INFINITY){
            index1[n + s2 + 2] = j;
            s2++;
        }
    }
    index1[0] = s1;
    index1[n + 1] = s2;
}

int get_index_sparse(int n, int i, int j){
	if(j<=i){
		return (n*i + j);
	}
	else{
		return (n*(j^1) + (i^1));
	}
}

bool strengthning_int_sparse(double * result, int * ind1, double *temp, int n){
	int s = 0;
	for(unsigned i = 0; i < n; i++){
		int ind = i + ((((i^1) + 1)*((i^1) + 1))/2);
		temp[i] = ceil(result[ind]/2);
		if(temp[i] != INFINITY){
			ind1[s+1] = i;
			s++;
		}
	}
	ind1[0] = s;
	//cout<<"s is \t"<<s<<"\n";
	for(unsigned i = 0; i < ind1[0]; i++){
		unsigned i1 = ind1[i + 1];
		double t1 = temp[i1];
		for(unsigned j = 0; j < ind1[0];j++){
			unsigned j1 = ind1[j + 1];
			double t2 = temp[j1];
			int ind = j1 + ((((i1^1) + 1)*((i1^1) + 1))/2);
			//result[n*(i1^1) + j1] = min(result[n*(i1^1) + j1], t1 + t2);
			result[ind] = min(result[ind], t1 + t2);
		}	
	}
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

bool strengthning_sparse(double * result, int * ind1, double *temp, int n){
	int s = 0;
	for(unsigned i = 0; i < n; i++){
	        int ind = i + ((((i^1) + 1)*((i^1) + 1))/2);
		//temp[i] = result[n*(i^1) + i];
		temp[i] = result[ind];
		if(temp[i] != INFINITY){
			ind1[s+1] = i;
			s++;
		}
	}
	ind1[0] = s;
	for(unsigned i = 0; i < ind1[0]; i++){
		unsigned i1 = ind1[i + 1];
		double t1 = temp[i1];
		for(unsigned j = 0; j < ind1[0];j++){
			unsigned j1 = ind1[j + 1];
			if(j1 > (i1|1)){
				break;
			}
			double t2 = temp[j1];
			int ind = j1 + ((((i1^1) + 1)*((i1^1) + 1))/2);
			//result[n*(i1^1) + j1] = min(result[n*(i1^1) + j1], (t1 + t2)/2);
			result[ind] = min(result[ind], (t1 + t2)/2);
		}	
	}
	for(unsigned i = 0; i < n; i++){
		int ind = i + (((i+1)*(i+1))/2);
		if(result[ind] < 0){
			return true;
		}
		else{
			result[ind] = 0;
		}
	}
	 //cout<<"\nAfter Strengthening Version_37\n";
    	 //print_v39(result,n/2);
	return false;
}



void sort_sparse(int * m, int n){
    if(n==0)
        return;
	for (int i = 1 ; i <= n - 1; i++) {
        //cout<<"Coming here "<<i<<"\n";
		int temp = m[i];
 		int j;
    		for (j = i;j > 0 && temp < m[j-1];j--) {
      			m[j]   = m[j-1];
  		  }
		m[j] = temp;
	}
}

void print_index_sparse(int *m, int n){

	for(int i = 0; i < n; i++){
		printf("%d\t",m[i]);
	}
	printf("\n");
}

bool strong_closure_sparse(double *result, double *temp1, double *temp2, int *index1, int *index2, int dim, bool is_int){
    
    int size = 4 * dim * dim;
    int n = 2*dim; 
    int m = 2*dim + 1;
    for(int i = 0; i < n; i++){
        temp1[i] = 0;
        temp2[i] = 0;
    }

    for(int k = 0; k < dim; k++){
	//Compute index at start of iteration
        compute_index_sparse(result, index1, index2, k, dim);
	
	int s1 = index1[0],s2 = index1[n + 1];
	int s3 = index2[0], s4 = index2[n + 1];
	int pos1 = ((2*k)^1) + ((((2*k) + 1)*((2*k) + 1))/2);
	//int pos2 = matpos2((2*k)^1, 2*k);
	int pos2 = (2*k) + (((((2*k)^1) + 1)*(((2*k)^1) + 1))/2);
	//Compute (2k) and (2k+1)-th row and column, update the index
	if(result[pos1]!= INFINITY){
		  for(int i = 0; i < s3;i++){
			int i2 = index2[i + 1];
			int i1 = i2;
			//int ind2 = n*i2 + ((2*k)^1);
			//int ind1 = n*i1 + (2*k);
			int ind1 = (2*k) + (((i1 + 1)*(i1 + 1))/2);
			int ind2 = ((2*k)^1) + (((i2 + 1)*(i2 + 1))/2);

			if(result[ind2]!= INFINITY){
				result[ind2] = min(result[ind2],  result[pos1] + result[ind1]);
			}
			else{
				result[ind2] =  result[pos1] + result[ind1];
				//index1[m*i2 + index1[m*i2] + 1] = ((2*k)^1);
				index2[n + s4 + 2] = i2;
				//index1[m*i2]++;
				s4++;
			}
			//temp2[i2] = result[ind2];
		}
		index2[n + 1] = s4;
	}
	for(int i = 2*k + 2; i < n; i++){
		int ind = ((2*k)^1) + (((i+1)*(i+1))/2);
		//temp2[i] = result[n*i + ((2*k)^1)];
		temp2[i] = result[ind];
	}
	
	

	if(result[pos2] != INFINITY){
		
		for(int i = 0; i < s4; i++){
			int i2 = index2[n + i + 2];
			int i1 = i2;
			//int ind2 = n*i2 + ((2*k)^1);
			//int ind1 = n*i1 + (2*k);
			int ind1 = (2*k) + (((i1 + 1)*(i1 + 1))/2);
			int ind2 = ((2*k)^1) + (((i2 + 1)*(i2 + 1))/2);
			if(result[ind1] != INFINITY){
				result[ind1] = min(result[ind1], result[pos2] + result[ind2]);
			}
			else{
				result[ind1] = result[pos2] + result[ind2];
				//index1[m*i1 + index1[m*i1] + 1] = 2*k;
				index2[s3 + 1] = i1;
				//index1[m*i1]++;
				s3++;
			}
			temp1[i1] = result[ind1];
		}

		index2[0] = s3;
	}
	for(int i = 2*k + 2; i < n; i++){
		int ind = (2*k) + (((i+1)*(i+1))/2);
		//temp1[i] = result[n*i + (2*k)];
		temp1[i] = result[ind];
	}

	if(result[pos2] != INFINITY){

		for(int j = 0; j < s1; j++){
			//int ind4 = get_index(n, 2*k,j);
			//int j1 = index1[m*(2*k) + j + 1];
			int j1 = index1[j + 1];
			int ind1 = j1 + ((((2*k) + 1)*((2*k) + 1))/2);
			int ind2 = j1 + (((((2*k)^1) + 1)*(((2*k)^1) + 1))/2);
			if(result[ind2] != INFINITY ){
				result[ind2] = min(result[ind2], result[pos2] + result[ind1]);
			}
			else{
				result[ind2] =  result[pos2] + result[ind1];
				index1[n + s2 + 2] = j1;
				//index2[m*j1 + index2[m*j1] + 1] = ((2*k)^1);
				s2++;
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
			int ind1 = j1 + ((((2*k) + 1)*((2*k) + 1))/2);
			int ind2 = j1 + (((((2*k)^1) + 1)*(((2*k)^1) + 1))/2);
			//if(result[ind2] != std::numeric_limits<double>::infinity(
			if(result[ind1] != INFINITY ){
				result[ind1] = min(result[ind1], result[pos1] + result[ind2]);
			}
			else{
				result[ind1] =  result[pos1] + result[ind2];
				index1[s1 + 1] = j1;
				//index2[m*j1 + index2[m*j1] + 1] = (2*k);
				s1++;
				//index2[m*j1]++;
			}
		}
		index1[0] = s1;
	}
       
	//This is the Main Loop Divided into four parts
	//First Part
	int ind1_k = index1[0];
	int ind2_k = index1[n + 1];
	int ind3_k = index2[0];
	int ind4_k = index2[n + 1];
	for(int i = 0; i < ind1_k; i++){
		int i1 = index1[i + 1];
        	int i2 = (i1%2==0) ? (i1 + 1): i1;
		int br = i2 < 2*k ? i2 : 2*k - 1;
		int ind1 = i1 + ((((2*k) + 1)*((2*k) + 1))/2);
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
			}
			int ind2 = j1 + (((((2*k)^1) + 1)*(((2*k)^1) + 1))/2);
			//double op1 = t1 + result[n*((2*k)^1) + j1];
			double op1 = t1 + result[ind2];
			//double op2 = t2 + result[n*(2*k) + j];
			//double op3 = min(op1, op2);
			int ind3 = j1 + ((((i1^1) + 1)*((i1^1) + 1))/2);
			//result[n*(i1^1) + j1] = min(result[n*(i1^1) + j1],op1 );
			result[ind3] = min(result[ind3],op1 );
		}
		//for(int j = 0; j < index2[m*2*k]; j++){
		for(int j = 0;j < ind3_k; j++){
			int j1 = index2[j + 1];
			if(j1>i2){
				 break;
		    	}
			double op1 = t1 + temp1[j1];
			int ind3 = (j1^1) + ((((i1^1) + 1)*((i1^1) + 1))/2);
			//result[n*(i1^1) + (j1^1)] = min(result[n*(i1^1) + (j1^1)],op1 );
			result[ind3] = min(result[ind3],op1 );
		}
		//}
	}
        
	//Second Part
        for(int i = 0; i < ind2_k; i++){
	    int i1 = index1[n + i + 2];
            int i2 = (i1%2==0) ? (i1 + 1): i1;
	    int br = i2 < 2*k ? i2 : 2*k - 1;
            //double t1 = result[n*(2*k) + i1];
	    int ind1 = i1 + (((((2*k)^1) + 1)*(((2*k)^1) + 1))/2);
            //double t2 = result[n*((2*k)^1) + i1];
	    double t2 = result[ind1];
            //int j2 = (j/2)*2;
            for(int j = 0; j < ind1_k; j++){
		    int j1 = index1[j + 1];
		    if(j1 > br){
			break;
		    }
		    int ind2 = j1 + ((((2*k) + 1)*((2*k) + 1))/2);
                    //double op2 = t2 + result[n*(2*k) + j1];
		    double op2 = t2 + result[ind2];
		    int ind3 = j1 + ((((i1^1) + 1)*((i1^1) + 1))/2);
                    //result[n*(i1^1) + j1] = min(result[n*(i1^1) + j1],op2 );
		    result[ind3] = min(result[ind3],op2 );
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
		    result[ind3] = min(result[ind3],op2 );
                }
            //}
        }
    
	//Third Part
        for(int i = 0; i < ind4_k; i++){
	    int i1 = index2[n + i + 2];
            int i2 = (i1%2==0) ? (i1 + 1): i1;
            int br = i2 < 2*k ? i2 : 2*k - 1;
	    int ind1 = ((2*k)^1) + (((i1 + 1)*(i1 + 1))/2);
            //double t1 = result[n*i1 + ((2*k)^1)];
	    double t1 = result[ind1];
           
            for(int j = 0; j < ind2_k; j++){
           	    //int j1 = index1[m*((2*k)^1) + j + 1];
		    int j1 = index1[n + j + 2];
		    if(j1 > br){
			break;
		    }
		    int ind2 = j1 + (((((2*k)^1) + 1)*(((2*k)^1) + 1))/2);
                    //double op1 = t1 + result[n*((2*k)^1) + j1];
		    double op1 = t1 + result[ind2];
		    int ind3 = j1 + (((i1 + 1)*(i1 + 1))/2);
                    //result[n*i1 + j1] = min(result[n*i1 + j1],op1 );
		    result[ind3] = min(result[ind3],op1 );
                }
                
         	for(int j = 0; j < ind3_k ; j++){
                    
		    int j1 = index2[j + 1];
		    if(j1>i2){
			break;
		     }
                    double op1 = t1 + temp1[j1];
                    //double op1 = t1 + result[n*(j1) + 2*k];
             	    int ind3 = (j1^1) + (((i1 + 1)*(i1 + 1))/2);
		    result[ind3] = min(result[ind3],op1 );
                }
        }
        
        //Fourth Part
        for(int i = 0; i < ind3_k; i++){
            //int i1 = index2[m*(2*k) + i + 1];
	    int i1 = index2[i + 1];
            int i2 = (i1%2==0) ? (i1 + 1): i1;
	     int br = i2 < 2*k ? i2 : 2*k - 1;
            //double t2 = result[n*i1 + (2*k)];
	    int ind1 = (2*k) + (((i1 + 1)*(i1 + 1))/2);
	    double t2 = result[ind1];
            for(int j = 0; j < ind1_k; j++){
		    int j1 = index1[j + 1];
		    if(j1 > br){
			break;
		    }
                    //double op2 = t2 + result[n*(2*k) + j1];
		    int ind2 = j1 + ((((2*k) + 1)*((2*k) + 1))/2);
		    double op2 = t2 + result[ind2];
		    int ind3 = j1 + (((i1 + 1)*(i1 + 1))/2);
                    //result[n*i1 + j1] = min(result[n*i1 + j1],op2 );
		    result[ind3] = min(result[ind3],op2 );
                }
               
                
               for(int j = 0; j < ind4_k ; j++){
		    int j1 = index2[n + j + 2];
		    if(j1>i2){
			break;
		    }
                    double op2 = t2 + temp2[j1];
		    int ind3 = (j1^1) + (((i1 + 1)*(i1 + 1))/2);
                    //result[n*i1 + (j1^1)] = min(result[n*i1 + (j1^1)],op2 );
		    result[ind3] = min(result[ind3],op2 );
                }
        }

    }
   
    if(is_int){
	return strengthning_int_sparse(result,index1,temp1,n);
    }
    else{
    	return strengthning_sparse(result,index1,temp1,n);
    }
}


void print_sparse(double *m, int dim){
   int size = 2*dim*(dim + 1);
    for (int i = 0; i < size; ++i){
            printf("%.15f \t", m[i]);
    }
    printf("\n\n");
}


