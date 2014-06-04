#include "opt_oct_closure_dense.h"
#include <assert.h>

#define U_i 32
#define U_j 32



double strong_closure_calc_perf_dense(double cycles, int dim){
  double n = 2*dim;
  return  (n*n*n)/cycles;
}

bool strengthning_int_dense(double * result, double *temp, int n){
	for(int i = 0; i < n; i++){
		//int ind1 = matpos2(i^1, i);
		int ind1 = i + ((((i^1) + 1)*((i^1) + 1))/2);
		temp[i] = ceil(result[ind1]/2);
	}
	
	for(int i = 0; i < n; i++){
		int i2 = (i|1);
		__m256d t1 = _mm256_set1_pd(temp[i^1]);
		double *p = result + (((i+1)*(i+1))/2);
		for(int j = 0; j < (i2/4); j++){
			//int ind = j*8 + (((i+1)*(i+1))/2);
			__m256d t2 = _mm256_loadu_pd(temp + j*4);
			__m256d op1 = _mm256_add_pd(t1,t2);
			__m256d op2 = _mm256_loadu_pd(p + j*4);
			__m256d res = _mm256_min_pd(op1, op2);
			//result[ind] = min(result[ind], temp[i^1] + temp[j]);
			_mm256_storeu_pd(result + (((i+1)*(i+1))/2) + j*4, res);
		}
		for(int j = (i2/4)*4; j<= i2; j++){
			int ind = j + (((i+1)*(i+1))/2);
			result[ind] = min(result[ind], temp[i^1] + temp[j]);
		}
			
	}

	for(int i = 0; i < n; i++){
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

bool strengthning_dense(double * result, double *temp, int n){
	for(int i = 0; i < n; i++){
		//int ind1 = matpos2(i^1,i);
		int ind1 = i + ((((i^1) + 1)*((i^1) + 1))/2);
		temp[i] = result[ind1]/2;
	}
	
	for(int i = 0; i < n; i++){
		int i2 = (i|1);
		__m256d t1 = _mm256_set1_pd(temp[i^1]);
		double *p = result + (((i+1)*(i+1))/2);
		for(int j = 0; j < (i2/4); j++){
			//int ind = j*8 + (((i+1)*(i+1))/2);
			__m256d t2 = _mm256_loadu_pd(temp + j*4);
			__m256d op1 = _mm256_add_pd(t1,t2);
			__m256d op2 = _mm256_loadu_pd(p + j*4);
			__m256d res = _mm256_min_pd(op1, op2);
			//result[ind] = min(result[ind], temp[i^1] + temp[j]);
			_mm256_storeu_pd(result + (((i+1)*(i+1))/2) + j*4, res);
		}
		for(int j = (i2/4)*4; j<= i2; j++){
			int ind = j + (((i+1)*(i+1))/2);
			result[ind] = min(result[ind], temp[i^1] + temp[j]);
		}
			
	}

	for(int i = 0; i < n; i++){
		int ind = i + (((i+1)*(i+1))/2);
		if(result[ind] < 0){
			return true;
		}
		else{
			result[ind] = 0;
		}
	}
	 //cout<<"\nAfter Strengthening Version_38\n";
    	 //print_dense(result,n/2);
	return false;
}

bool strong_closure_dense(double *result, double *temp1, double *temp2, int dim, bool is_int){
    
    int size = 4 * dim * dim;
    int n = 2*dim; 
    double count = 0;
    for(int k = 0; k < dim; k++){
	//int pos1 = matpos2(2*k, (2*k)^1);
	int ki = ((((2*k) + 1)*((2*k) + 1))/2);
	int kki = (((((2*k)^1) + 1)*(((2*k)^1) + 1))/2);
	int pos1 = ((2*k)^1) + ki;
	//int pos2 = matpos2((2*k)^1, 2*k);
	int pos2 = (2*k) + kki;

	for(int i = 2*k + 2; i < n;i++){
		//int ind1 = matpos2(i,((2*k)^1));
		int ind1 = ((2*k)^1) + (((i+1)*(i+1))/2);
		//int ind2 = matpos2(i,2*k);
		int ind2 = (2*k) + (((i+1)*(i+1))/2);
		//int ind2 = n*i + ((2*k)^1);
		//int ind1 = n*i + (2*k);
		//result[ind2] = min(result[ind2], result[ind1] + result[n*(2*k) + ((2*k)^1)] );
		result[ind1] = min(result[ind1], result[ind2] + result[pos1] );
                count = count + 2;
		temp2[i^1] = result[ind1];
	}


	for(int i = 2*k + 2; i < n; i++){
		//int ind1 = matpos2(i,((2*k)^1));
		int ind1 = ((2*k)^1) + (((i+1)*(i+1))/2);
		//int ind2 = matpos2(i,2*k);
		int ind2 = (2*k) + (((i+1)*(i+1))/2);
		//int ind2 = n*i + ((2*k)^1);
		//int ind1 = n*i + (2*k);
		//result[ind1] = min(result[ind1], result[ind2] + result[n*((2*k)^1) + (2*k)] );
		result[ind2] = min(result[ind2], result[ind1] + result[pos2] );
		temp1[i^1] = result[ind2];
		count = count + 2;
	}

	for(int j = 0; j < (2*k); j++){
		//int ind3 = matpos2((2*k)^1,j);
		int ind3 = j + kki;
		//int ind4 = matpos2( 2*k,j);
		int ind4 = j + ki;
		//result[n*((2*k)^1) + j] = min(result[n*((2*k)^1) + j], result[n*((2*k)^1) + 2*k] + result[n*(2*k) + j]);
		result[ind3] = min(result[ind3], result[pos2] + result[ind4]);
		count = count + 2;
	}
	for(int j = 0; j < (2*k); j++){
		//int ind3 = matpos2((2*k)^1,j);
		int ind3 = j + kki;
		//int ind4 = matpos2(2*k,j);
		int ind4 = j + ki;
		//result[n*2*k + j] = min(result[n*2*k + j], result[n*2*k + ((2*k)^1)] + result[n*((2*k)^1) + j]);
		result[ind4] = min(result[ind4], result[pos1] + result[ind3]);
		count = count + 2;
	}
	double *p1 = result + kki;
	double *p2 = result + ki;
	int l = (2*k + 2);
	int mod = l%4;
	if(mod){
		l = l + (4 - mod);
	}
	for(int i = 0; i < 2*k; i++){
		int i2 = (i%2==0) ? (i + 1): i;
		int br = i2 < 2*k ? i2 : 2*k - 1;
		//int ind1 = matpos2(i,2*k);
		int ind1 = (i^1) + kki;
		//int ind2 = matpos2(i, ((2*k)^1));
		int ind2 = (i^1) + ki;
		//double t1 = result[n*(2*k) + (i^1)];
		//double t2 = result[n*((2*k)^1) + (i^1)];
		double ft1 = result[ind2];
		double ft2 = result[ind1];
		__m256d t1 = _mm256_set1_pd(result[ind2]);
		__m256d t2 = _mm256_set1_pd(result[ind1]);
		
		int b = min(l,i2);
		double *p = result + (((i+1)*(i+1))/2);
		for(int j = 0; j <br/4; j++){
				
			__m256d t3 = _mm256_loadu_pd(p1 + j*4);
			__m256d op1 = _mm256_add_pd(t1,t3);
			//double op2 = t2 + result[ind4];
			__m256d t4 = _mm256_loadu_pd(p2 + j*4);
			__m256d op2 = _mm256_add_pd(t2,t4);
			//double op3 = min(op1, op2);
			__m256d op3 = _mm256_min_pd(op1,op2);
			__m256d op4 = _mm256_loadu_pd(p + j*4);
			__m256d res = _mm256_min_pd(op3, op4);
			_mm256_storeu_pd(p + j*4, res);
			//result[ind5] = min(result[ind5],op3 );
			count = count + 4;
		}
			
		for(int j = (br/4)*4; j<=br;j++){
			int ind3 = j + kki;
			int ind4 = j + ki;
			int ind5 = j + (((i+1)*(i+1))/2);
			double op1 = ft1 + result[ind3];
			double op2 = ft2 + result[ind4];
			double op3 = min(op1, op2);
			result[ind5] = min(result[ind5],op3 );
		}
			
		for(int j = 2*k + 2; j <= b; j++){
			int ind5 = j + (((i+1)*(i+1))/2);
			double op1 = ft1 + temp1[j];
			double op2 = ft2 + temp2[j];
			double op3 = min(op1, op2);
			result[ind5] = min(result[ind5],op3 );
		}

		if(b < i2){
			for(int j = b/4; j <i2/4; j++){
				__m256d t3 = _mm256_loadu_pd(temp1 + j*4);
				__m256d op1 = _mm256_add_pd(t1,t3);
				//double op2 = t2 + temp2[j^1];
				__m256d t4 = _mm256_loadu_pd(temp2 + j*4);
				__m256d op2 = _mm256_add_pd(t2,t4);
				//double op3 = min(op1, op2);
				__m256d op3 = _mm256_min_pd(op1, op2);
				__m256d op4 = _mm256_loadu_pd(p + j*4);
				//result[ind5] = min(result[ind5],op3 );
				__m256d res = _mm256_min_pd(op3, op4);
				_mm256_storeu_pd(p + j*4, res);
				count = count + 4;
			}
			for(int j = (i2/4)*4; j<=i2; j++){
				int ind5 = j + (((i+1)*(i+1))/2);
				double op1 = ft1 + temp1[j];
				double op2 = ft2 + temp2[j];
				double op3 = min(op1, op2);
				result[ind5] = min(result[ind5],op3 );
			}
		}
		//}
	}

	for(int i = 2*k + 2; i < n; i++){
		int i2 = (i%2==0) ? (i + 1): i;
		int br = i2 < 2*k ? i2 : 2*k - 1;
		//int ind1 = matpos2(i,(2*k)^1);
		int ind1 = ((2*k)^1) + (((i+1)*(i+1))/2);
		//int ind2 = matpos2(i,2*k);
		int ind2 = (2*k) + (((i+1)*(i+1))/2);
		//double t1 = result[n*i + ((2*k)^1)];
		//double t2 = result[n*i + 2*k];
		double ft1 = result[ind1];
		double ft2 = result[ind2];
		
		__m256d t1 = _mm256_set1_pd(result[ind1]);
		__m256d t2 = _mm256_set1_pd(result[ind2]);
		int b = min(l,i2);
		
		double *p = result + (((i+1)*(i+1))/2);
	
		for(int j = 0; j < br/4; j++){
			//int ind3 = j + kki;
			//int ind4 = matpos2( 2*k,j);
			//int ind4 = j + ki;
			//int ind5 = matpos2(i,j);
			//int ind5 = j + (((i+1)*(i+1))/2);
			//int ind2 = matpos2(k,j);
			//double op1 = t1 + result[n*((2*k)^1) + j];
			//double op2 = t2 + result[n*(2*k) + j];
			//double op1 = t1 + result[ind3];
			__m256d t3 = _mm256_loadu_pd(p1 + j*4);
			__m256d op1 = _mm256_add_pd(t1,t3);
			//double op2 = t2 + result[ind4];
			__m256d t4 = _mm256_loadu_pd(p2 + j*4);
			__m256d op2 = _mm256_add_pd(t2,t4);
			//double op3 = min(op1, op2);
			__m256d op3 = _mm256_min_pd(op1,op2);
			__m256d op4 = _mm256_loadu_pd(p + j*4);
			//result[ind5] = min(result[ind5],op3 );
			__m256d res = _mm256_min_pd(op3,op4);
			_mm256_storeu_pd(p + j*4,res);
			count = count + 4;
		}
		for(int j = (br/4)*4; j<=br; j++){
			int ind3 = j + kki;
			int ind4 = j + ki;
			int ind5 = j + (((i+1)*(i+1))/2);
			double op1 = ft1 + result[ind3];
			double op2 = ft2 + result[ind4];
			double op3 = min(op1, op2);
			result[ind5] = min(result[ind5],op3 );
			count = count + 4;
		}
		for(int j = 2*k + 2; j <= b; j++){
			//cout<<"coming here*********\n";
			int ind5 = j + (((i+1)*(i+1))/2);
			double op1 = ft1 + temp1[j];
			double op2 = ft2 + temp2[j];
			double op3 = min(op1, op2);
			result[ind5] = min(result[ind5],op3 );
		}
		if(b < i2){
			
			for(int j = b/4; j < i2/4; j++){
				//int ind3 = matpos2(n,(2*k)^1,j);
				//int ind4 = matpos2(n, 2*k,j);
				//int ind2 = matpos2(k,j);
				//double op1 = t1 + result[n*(j^1) + 2*k];
				//double op2 = t2 + result[n*(j^1) + ((2*k)^1)];
				//int ind5 = matpos2(i,j);
				//int ind5 = j + (((i+1)*(i+1))/2);
				//double op1 = t1 + temp1[j]		
				__m256d t3 = _mm256_loadu_pd(temp1 + j*4);
				__m256d op1 = _mm256_add_pd(t1,t3);
				//double op2 = t2 + temp2[j];
				__m256d t4 = _mm256_loadu_pd(temp2 + j*4);
				__m256d op2 = _mm256_add_pd(t2,t4);
				//double op3 = min(op1, op2);
				__m256d op3 = _mm256_min_pd(op1,op2);
				__m256d op4 = _mm256_loadu_pd(p + j*4);
				//result[ind5] = min(result[ind5],op3 );
				__m256d res = _mm256_min_pd(op3,op4);
				_mm256_storeu_pd(p + j*4, res);
				count = count + 4;
			}
			for(int j = (i2/4)*4; j <=i2; j++){
				int ind5 = j + (((i+1)*(i+1))/2);
				double op1 = ft1 + temp1[j];
				double op2 = ft2 + temp2[j];
				double op3 = min(op1, op2);
				result[ind5] = min(result[ind5],op3 );
			}
		}
	}
	
    }
    if(is_int){
	return strengthning_int_dense(result,temp1,n);
    }
    else{
    	return strengthning_dense(result,temp1,n);
    }
}


void print_dense(double *m, int dim){
   int n = 2*dim;
    for (int i = 0; i < 2*dim; ++i){
	int ni = n*i;
        for (int j = 0; j < 2*dim; ++j){
            printf("%.15f \t", m[ni + j]);
        }
        printf("\n");
    }
    printf("\n\n");
}


