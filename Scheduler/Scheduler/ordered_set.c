#include "sched_mat.h"

/* Initialize the ordered set
// ==========================
// s is an ordered_set pointer
// id_size is the length of the node's ID array
//
// Example:
// struct ordered_set* s;
// init_set(s, nsats);
*/
void init_set(struct ordered_set* s, int id_size) {
	s->size = 0;
	s->max = NULL;
	s->id_size = id_size;
}
	

/* Obtain the number of elements in the ordered set
// ================================================
// s is an ordered_set pointer
//
// Example:
// int* id = ...;
// float F = ...;
// struct ordered_set* s;
// init_set(s, nsats);
// add(s, id, F);
// printf("%d", size(s)); --> It will print "1"
*/
int size(struct ordered_set* s) {
	return s->size;
}

/* Returns whether the set is empty or not
// =======================================
// s is an ordered_set pointer
//
// Example:
// int* id = ...;
// float F = ...;
// struct ordered_set* s;
// init_set(s, nsats);
// add(s, id, F);
// if (!is_empty(s)) {
// 	printf("The list is not empty");
// }
*/
int is_empty(struct ordered_set* s) {
	return (s->size == 0);
}

int are_equal(struct node* a, struct node* b, int id_size) {
	if (a->F == b->F) {
		int i = 0;
		while (i < id_size && (a->id)[i] == (b->id)[i]) { i++;	}
		if (i == id_size) return 1;
	}
	return 0;
}

/* Adds a new element to the set
// =======================================
// s is an ordered_set pointer
// id is an integer array of id_size elements
//
// If the element is already in the set, the
// resulting set does not change. Else it 
// includes the new element and renew the 
// values of the max and min elements, if 
// necessary.
//
// Example:
// int* id = ...;
// float F = ...;
// struct ordered_set* s;
// init_set(s, nsats);
// add(s, id, F);
*/
void add(struct ordered_set* s, int* id, float F) {
	struct node* new = malloc(sizeof(struct node));
	int i;
	new->F = F;
	new->id = malloc(s->id_size*sizeof(int));
	for(i = 0; i < s->id_size; i++) {
		new->id[i] = id[i];
	}
	struct set_node* elem = malloc(sizeof(struct set_node));
	elem->value = new;
	if (s->size == 0) {
		s->max = elem;
		elem->next = NULL;
#ifdef DEBUG
    	printf("\033[22;31m\n>>>>DEBUG: This element is the first on the "
           "set\n\033[0m");
#endif
	}
	else if (((s->max)->value)->F < new->F) {
		elem->next = s->max;
		s->max = elem;
	}
	else if (are_equal((s->max)->value,new,s->id_size)) {
#ifdef DEBUG
  	  	printf("\033[22;31mThis element is already on the set!!\033[0m");
#endif
		return;
	}
	else {
		struct set_node* n = s->max;
		while(n->next != NULL && ((n->next)->value)->F > new->F) {
			n = n->next;
		}
		/* while(n->next != NULL && ((n->next)->value)->F == new->F) {
			if (are_equal((n->next)->value,new,s->id_size)) {
#ifdef DEBUG
  	  		printf("\033[22;31mThis element is already on the set!!\033[0m");
#endif
			return;
			}
			n = n->next;
		}*/
		elem->next = n->next;
		n->next = elem;
	}
	s->size++;
#ifdef DEBUG
  	printf("\033[22;31mThe set's size is %d\n\033[0m", s->size);
#endif	
}

void copy_node(struct set_node* original, struct node* copy, int id_size) {
	copy->F = (original->value)->F;
	int i;
	copy->id = malloc(id_size*sizeof(int));
	for(i = 0; i < id_size; i++) {
		(copy->id)[i] = ((original->value)->id)[i];
	}
}

/* Returns the max element of the set
// =======================================
// s is an ordered_set pointer
//
// Returns a pointer to the node struct
// having the larger F. If the set is
// empty, a NULL pointer will be returned.
// The set does not change after this op.
//
// Example:
// int* id = ...;
// float F = ...;
// struct ordered_set* s;
// init_set(s, nsats);
// add(s, id, F);
// struct node* max = get_max(s);
// printf("Merit figure: %f\t ID pointer: %d,", max->F, max->id);
*/
struct node* get_max(struct ordered_set* s) {
	if (s->size == 0) return NULL;
	/*struct node* copy = malloc(sizeof(struct node));
	copy_node(s->max, copy, s->id_size);
	return copy;*/
	return (s->max)->value;
}

/* Returns and delete the max element of the set
// =============================================
// s is an ordered_set pointer
//
// Returns a pointer to the node struct
// having the larger F. If the set is
// empty, a NULL pointer will be returned.
// The new set contains the same elements as before
// except for the previous maximum.
//
// Example:
// int* id = ...;
// float F = ...;
// struct ordered_set* s;
// init_set(s, nsats);
// add(s, id, F);
// int size = size(s);
// struct node* max = get_and_delete_max(s);
// int new_size = size(s);
// printf("Previous size: %d\t New size: %d,", size, new_size);
// [new_size should be 0 and size should be 1]
*/
struct node* get_and_delete_max(struct ordered_set* s) {
	if (s->size == 0) {
#ifdef DEBUG
        printf("\033[22;31m\n>>>>DEBUG: There is no node\n\033[0m");
#endif	
		return NULL;
	}
	struct node* result = (s->max)->value;
	if (s->size == 1) {
		s->max = NULL;
	}
	else {
		s->max = (s->max)->next;
	}
	s->size--;
	return result;
}
