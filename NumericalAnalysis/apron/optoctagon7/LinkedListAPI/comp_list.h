#ifndef __COMP_LIST_H
#define __COMP_LIST_H

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef struct comp_t{
	unsigned short int num;
	struct comp_t * next;
}comp_t;

typedef struct comp_list_t{
	comp_t *head;
	//comp_t *tail;
	struct comp_list_t * next;
	unsigned short int size;
}comp_list_t;

typedef struct array_comp_list_t{
	comp_list_t *head;
	//comp_list_t *tail;
	unsigned short int size;	
}array_comp_list_t; 

/****
Basic Linked List Insert, Delete and Find functions for Component List
*****/
comp_list_t * create_comp_list();
comp_list_t * copy_comp_list(comp_list_t *src);
void free_comp_list(comp_list_t *cl);
unsigned short int comp_list_size(comp_list_t *cl);
void insert_comp(comp_list_t *cl, unsigned short int num);
int contains_comp(comp_list_t *cl, unsigned short int num);
void remove_comp(comp_list_t *cl, unsigned short int num);
void print_comp_list(comp_list_t *cl);
unsigned short int * to_sorted_array(comp_list_t * cl,unsigned short int n);


/*****
Set Operations on two Component Lists
*****/

comp_list_t * intersection_comp_list(comp_list_t *c1, comp_list_t *c2, unsigned short int n);
int is_disjoint(comp_list_t * cl1, comp_list_t *cl2, unsigned short int n);
int is_included(comp_list_t *cl1, comp_list_t *cl2, unsigned short int n);
void union_comp_list(comp_list_t * cl1,comp_list_t * cl2, char *map);
void union_comp_list_direct(comp_list_t *cl1, comp_list_t *cl2, unsigned short int n);
void unite_comp_lists(comp_list_t *cl1,comp_list_t *cl2,char *map,int i, int j,unsigned short int n);
char * create_map(comp_list_t *cl, unsigned short int n);


/****
Basic Linked List Insert, Delete and Find functions for List of Component List
*****/

array_comp_list_t * create_array_comp_list();
array_comp_list_t * copy_array_comp_list(array_comp_list_t *src);
void free_array_comp_list(array_comp_list_t * acl);
void insert_comp_list(array_comp_list_t *acl, comp_list_t * cl);
void insert_comp_list_with_union(array_comp_list_t * acl, comp_list_t *cl,unsigned short int n);
comp_list_t * find(array_comp_list_t *acl,unsigned short int num);
void remove_comp_list(array_comp_list_t *acl, comp_list_t *cl);
void print_array_comp_list(array_comp_list_t *acl);
void clear_array_comp_list(array_comp_list_t *acl);

/*****
Intersection and Union of two Component Lists
*****/

array_comp_list_t * intersection_array_comp_list(array_comp_list_t *acl1, array_comp_list_t *acl2,unsigned short int n);
comp_list_t * comp_array_union_direct(array_comp_list_t * acl1, array_comp_list_t *acl2, unsigned short int *om1, unsigned short int *om2,  unsigned short int n);
array_comp_list_t * union_array_comp_list(array_comp_list_t *acl1, array_comp_list_t *acl2, unsigned short int n);
int is_equal_array_comp_list(array_comp_list_t *acl1, array_comp_list_t *acl2, unsigned short int n);
int is_lequal_array_comp_list(array_comp_list_t * acl1, array_comp_list_t * acl2, unsigned short int n);
int is_connected(array_comp_list_t *acl, unsigned short int i, unsigned short int j);

/***
Extracting Components
**/
array_comp_list_t * extract(double *m, unsigned short int n);
array_comp_list_t * extract_comps(char *m, unsigned short int n);

/*static int matpos(int i, int j){
	return j + ((i+1)*(i+1))/2;
}

static int matpos2(int i , int j){
	 if (j>i) return matpos(j^1,i^1);
  	 else return matpos(i,j);
}*/

#endif
