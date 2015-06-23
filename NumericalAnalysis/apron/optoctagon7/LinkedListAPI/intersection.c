#include "comp_list.h"

comp_list_t * intersection_comp_list(comp_list_t *c1, comp_list_t *c2, unsigned short int n){
	unsigned short int *map = (unsigned short int *)calloc(n,sizeof(unsigned short int));
	comp_list_t *res = create_comp_list();
	int l = 0;
	comp_t *c = c1->head;
	while(c!=NULL){
		unsigned short int num = c->num;
		map[num] = 1;
		c = c->next;
	}
	c = c2->head;
	while(c!=NULL){
		unsigned short int num = c->num;
		if(map[num]){
			insert_comp(res,num);
		}
		c = c->next;
	}
	free(map);
	return res;
}

array_comp_list_t * intersection_array_comp_list(array_comp_list_t *acl1, array_comp_list_t *acl2, unsigned short int n){
	int s1 = acl1->size;
	int s2 = acl2->size;
	array_comp_list_t * res = create_array_comp_list();
	//int l = 0;
	if(!s1 || !s2){
		return res;
	}
	comp_list_t * cl1 = acl1->head;
	while(cl1!=NULL){
		comp_list_t * cl2 = acl2->head;
		while(cl2!=NULL){
			comp_list_t * src = intersection_comp_list(cl1,cl2,n);
			if(src->size>0){
				insert_comp_list(res,src);
			}
			else{
				free_comp_list(src);
			}
			cl2 = cl2->next;
		}
		cl1 = cl1->next;
	}
	return res;
}
