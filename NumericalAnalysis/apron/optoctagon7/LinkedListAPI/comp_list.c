#include "comp_list.h"

void print_comp_list(comp_list_t *cl){
	if(!cl || !cl->size){
		return;
	}
	comp_t * c = cl->head;
	while(c!=NULL){
		fprintf(stdout,"%d ",c->num);
		c = c->next;
	}
	fprintf(stdout,"\n");
	fflush(stdout);
}



comp_list_t * create_comp_list(){
	comp_list_t * cl = (comp_list_t *)malloc(sizeof(comp_list_t));
	cl->head = NULL;
	//cl->tail = NULL;
	cl->next = NULL;
	cl->size = 0;
	return cl;
}

comp_list_t * copy_comp_list(comp_list_t *src){
	if(!src){
		return NULL;
	}
	comp_list_t * dst = create_comp_list();
	comp_t *s = src->head;
	while(s!=NULL){
		unsigned short int num = s->num;
		insert_comp(dst,num);
		s = s->next;
	}
	return dst;
}

void free_comp_list(comp_list_t *cl){
	if(cl==NULL){
		return;
	}
	comp_t * c = cl->head;
	while(c!=NULL){
		comp_t *temp = c;
		c = c->next;
		free(temp);
		temp = NULL;
	}
	//free(cl->head);
	//free(cl->tail);
	free(cl);
	//cl->head = NULL;
	cl= NULL;
	
}

void insert_comp(comp_list_t *cl, unsigned short int num){
	comp_t *h = cl->head;
	comp_t * c = (comp_t *)malloc(sizeof(comp_t));
	c->num = num;
	c->next = NULL;
	if(!h){
		cl->head = c;
		//cl->tail = c;
		cl->size++;
		return;
	}
	//cl->tail->next = c;
	//cl->tail = cl->tail->next;
	c->next = cl->head;
	cl->head = c;
	cl->size++;
	return;
}


int contains_comp(comp_list_t *cl, unsigned short int num){
	comp_t * c = cl->head;
	while(c!=NULL){
		if(c->num==num){
			return 1;
		}
		c = c->next;
	}
	return 0;
}

unsigned short int comp_list_size(comp_list_t *cl){
	//comp_t *h = cl->head;
	//comp_t *t = cl->tail;
	return cl->size;
}

void remove_comp(comp_list_t *cl, unsigned short int num){
	if(!cl){
		return;
	}
	comp_t * c = cl->head;
	if(!c){
		return;
	}
	if(cl->head->num==num){
		cl->head = cl->head->next;
		cl->size--;
		free(c);
		return;
	}
	if(cl->size==1){
		return;
	}
	while(c->next!=NULL){
		if(c->next->num==num){
			comp_t *temp = c->next;
			/*if(c->next==cl->tail){
				cl->tail = c;
				free(temp);
				c->next = NULL;
			}
			else{*/
				c->next = c->next->next;
				free(temp);
			//}
			cl->size--;
			return;
		}
		c = c->next;
	}
	return;
}

unsigned short int * to_sorted_array(comp_list_t * cl,unsigned short int n){
	unsigned short int comp_size = cl->size;
	char * map = (char *)calloc(n,sizeof(char));
	unsigned short int * res = (unsigned short int *)malloc(comp_size*sizeof(unsigned short int));
	comp_t *c = cl->head;
	while(c!=NULL){
		unsigned short int num = c->num;
		map[num] = 1;
		c = c->next;
	}
	int l = 0;
	for(int i = 0; i < n; i++){
		if(map[i]){
			res[l] = i;
			l++;
		}
	}
	free(map);
	return res;
}


