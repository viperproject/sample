#include "opt_oct_closure_dense_scalar.h"


bool strengthning_int_dense_scalar(opt_oct_mat_t * oo, double *temp, int n){

	double *result = oo->mat;
	for(int i = 0; i < n; i++){
		int ind1 = opt_matpos2(i^1, i);
		temp[i] = ceil(result[ind1]/2);
	}
	
	for(int i = 0; i < n; i++){
		for(int j = 0; j <= (i|1); j++){
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

bool strengthning_dense_scalar(opt_oct_mat_t * oo, double *temp, int n){
	double *result = oo->mat;
	for(int i = 0; i < n; i++){
		int ind1 = opt_matpos2(i^1,i);
		temp[i] = result[ind1];
	}
	
	for(int i = 0; i < n; i++){
		for(int j = 0; j <= (i|1); j++){
			int ind = j + (((i+1)*(i+1))/2);
			result[ind] = min(result[ind], (temp[i^1] + temp[j])/2);
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



bool strong_closure_dense_scalar(opt_oct_mat_t *oo, double *temp1, double *temp2, int dim, bool is_int){
    double *result = oo->mat;
    int size = 4 * dim * dim;
    int n = 2*dim; 
    double count = 0;
    for(int k = 0; k < dim; k++){
	//int k2 = k==0 ? k + 1 : k;
	//int pos1 = matpos2(2*k, (2*k)^1);
	int pos1 = ((2*k)^1) + ((((2*k) + 1)*((2*k) + 1))/2);
	//int pos2 = matpos2((2*k)^1, 2*k);
	int pos2 = (2*k) + (((((2*k)^1) + 1)*(((2*k)^1) + 1))/2);
	

	for(int i = 2*k + 2; i < n;i++){
		//int ind1 = matpos2(i,((2*k)^1));
		int ind1 = ((2*k)^1) + (((i+1)*(i+1))/2);
		//int ind2 = matpos2(i,2*k);
		int ind2 = (2*k) + (((i+1)*(i+1))/2);
		//int ind2 = n*i + ((2*k)^1);
		//int ind1 = n*i + (2*k);
		//result[ind2] = std::min(result[ind2], result[ind1] + result[n*(2*k) + ((2*k)^1)] );
		result[ind1] = min(result[ind1], result[ind2] + result[pos1] );
		temp2[i^1] = result[ind1];
	}


	
	for(int i = 2*k + 2; i < n; i++){
		//int ind1 = matpos2(i,((2*k)^1));
		int ind1 = ((2*k)^1) + (((i+1)*(i+1))/2);
		//int ind2 = matpos2(i,2*k);
		int ind2 = (2*k) + (((i+1)*(i+1))/2);
		//int ind2 = n*i + ((2*k)^1);
		//int ind1 = n*i + (2*k);
		//result[ind1] = std::min(result[ind1], result[ind2] + result[n*((2*k)^1) + (2*k)] );
		result[ind2] = min(result[ind2], result[ind1] + result[pos2] );
		temp1[i^1] = result[ind2];
	}

	for(int j = 0; j < (2*k); j++){
		//int ind3 = matpos2((2*k)^1,j);
		int ind3 = j + (((((2*k)^1)+1)*(((2*k)^1)+1))/2);
		//int ind4 = matpos2( 2*k,j);
		int ind4 = j + ((((2*k)+1)*((2*k)+1))/2);
		//result[n*((2*k)^1) + j] = std::min(result[n*((2*k)^1) + j], result[n*((2*k)^1) + 2*k] + result[n*(2*k) + j]);
		result[ind3] = min(result[ind3], result[pos2] + result[ind4]);
	}
	for(int j = 0; j < (2*k); j++){
		//int ind3 = matpos2((2*k)^1,j);
		int ind3 = j + (((((2*k)^1)+1)*(((2*k)^1)+1))/2);
		//int ind4 = matpos2(2*k,j);
		int ind4 = j + ((((2*k)+1)*((2*k)+1))/2);
		//result[n*2*k + j] = std::min(result[n*2*k + j], result[n*2*k + ((2*k)^1)] + result[n*((2*k)^1) + j]);
		result[ind4] = min(result[ind4], result[pos1] + result[ind3]);
	}

	for(int i = 0; i < 2*k; i++){
		int i2 = (i%2==0) ? (i + 1): i;
		int br = i2 < 2*k ? i2 : 2*k - 1;
		//int ind1 = matpos2(i,2*k);
		int ind1 = (i^1) + (((((2*k)^1) + 1)*(((2*k)^1) + 1))/2);
		//int ind2 = matpos2(i, ((2*k)^1));
		int ind2 = (i^1) + ((((2*k) + 1)*((2*k) + 1))/2);
		//double t1 = result[n*(2*k) + (i^1)];
		//double t2 = result[n*((2*k)^1) + (i^1)];
		double t1 = result[ind2];
		double t2 = result[ind1];
		//int j2 = (j/2)*2;
			for(int j = 0; j <=br; j++){
				//int ind3 = matpos2((2*k)^1,j);
				int ind3 = j + (((((2*k)^1)+1)*(((2*k)^1)+1))/2);
				//int ind4 = matpos2(2*k,j);
				int ind4 = j + ((((2*k)+1)*((2*k)+1))/2);
				//int ind5 = matpos2(i,j);
				int ind5 = j + (((i+1)*(i+1))/2);
				//int ind2 = matpos2(k,j);
				//double op1 = t1 + result[n*((2*k)^1) + j];
				//double op2 = t2 + result[n*(2*k) + j];
				double op1 = t1 + result[ind3];
				double op2 = t2 + result[ind4];
				double op3 = min(op1, op2);
				result[ind5] = min(result[ind5],op3);
				count = count + 4;
			}
			for(int j = (2*k) + 2; j <=i2; j++){
				//int ind3 = matpos2(n,(2*k)^1,j);
				//int ind4 = matpos2(n, 2*k,j);
				//int ind2 = matpos2(k,j);
				//double op1 = t1 + result[n*(j^1) + 2*k];
				//double op2 = t2 + result[n*(j^1) + ((2*k)^1)];
				//int ind5 = matpos2(i,j);
				int ind5 = j + (((i+1)*(i+1))/2);
				double op1 = t1 + temp1[j];
				double op2 = t2 + temp2[j];
				double op3 = min(op1, op2);
				result[ind5] = min(result[ind5],op3 );
				count = count + 4;
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
		double t1 = result[ind1];
		double t2 = result[ind2];
		//int j2 = (j/2)*2;
			for(int j = 0; j <= br; j++){
				//int ind3 = matpos2((2*k)^1,j);
				int ind3 = j + (((((2*k)^1)+1)*(((2*k)^1)+1))/2);
				//int ind4 = matpos2( 2*k,j);
				int ind4 = j + ((((2*k)+1)*((2*k)+1))/2);
				//int ind5 = matpos2(i,j);
				int ind5 = j + (((i+1)*(i+1))/2);
				//int ind2 = matpos2(k,j);
				//double op1 = t1 + result[n*((2*k)^1) + j];
				//double op2 = t2 + result[n*(2*k) + j];
				double op1 = t1 + result[ind3];
				double op2 = t2 + result[ind4];
				double op3 = min(op1, op2);
				result[ind5] = min(result[ind5],op3 );
				count = count + 4;
			}
			for(int j = (2*k) + 2; j <= i2; j++){
				//int ind3 = matpos2(n,(2*k)^1,j);
				//int ind4 = matpos2(n, 2*k,j);
				//int ind2 = matpos2(k,j);
				//double op1 = t1 + result[n*(j^1) + 2*k];
				//double op2 = t2 + result[n*(j^1) + ((2*k)^1)];
				//int ind5 = matpos2(i,j);
				int ind5 = j + (((i+1)*(i+1))/2);
				double op1 = t1 + temp1[j];
				double op2 = t2 + temp2[j];
				double op3 = min(op1, op2);
				result[ind5] = min(result[ind5],op3 );
				count = count + 4;
			}
		//}
	}
	
    }
    oo->nni = 2*dim*(dim+1);
    if(is_int){
	return strengthning_int_dense_scalar(oo,temp1,n);
    }
    else{
    	return strengthning_dense_scalar(oo,temp1,n);
    }
}

