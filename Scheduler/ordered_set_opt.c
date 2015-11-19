#include "ordered_set_opt.h"
#include <stdlib.h>

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
	s->min = NULL;
	s->max = NULL;
	s->med = NULL;
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
		s->min = elem;
		s->med = elem;
		elem->next = NULL;
		elem->prev = NULL;
	}
	else if (((s->max)->value)->F < new->F) {
		elem->next = s->max;
		elem->prev = NULL;
		(s->max)->prev = elem;
		s->max = elem;
		if (s->size%2) s->med = s->med->prev;
	}
	else if (are_equal((s->max)->value,new,s->id_size)) return;
	else if (((s->min)->value)->F > new->F) {
		elem->prev = s->min;
		elem->next = NULL;
		(s->min)->next = elem;
		s->min = elem;
		if ((s->size + 1)%2) s->med = s->med->next;
	}
	else if (are_equal((s->min)->value,new,s->id_size)) return;
	else if (((s->med)->value)->F > new->F) {
		struct set_node* n = s->min;
		while(n->prev != s->med && ((n->prev)->value)->F < new->F) {
			n = n->prev;
		}
		if (are_equal((n->prev)->value,new,s->id_size)) return;
		elem->prev = n->prev;
		elem->next = n;
		(n->prev)->next = elem;
		n->prev = elem;
		if ((s->size + 1)%2) s->med = s->med->next;
	}
	// else if (are_equal((s->med)->value,new,s->id_size)) return;
	// NO HACE FALTA YA QUE ESTÁ INCLUIDO ESTE CASO EN EL ELSE IF ANTERIOR
	else {
		struct set_node* n = s->max;
		while(n->next != s->med && ((n->next)->value)->F > new->F) {
			n = n->next;
		}
		if (are_equal((n->next)->value,new,s->id_size)) return;
		elem->next = n->next;
		elem->prev = n;
		(n->next)->prev = elem;
		n->next = elem;
		if (s->size%2) s->med = s->med->prev;
	}
	s->size++;
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
	struct node* copy = malloc(sizeof(struct node));
	copy_node(s->max, copy, s->id_size);
	return copy;
}

/* Returns the median element of the set
// =======================================
// s is an ordered_set pointer
//
// Returns a pointer to the node struct
// having the median value of F. If the set is
// empty, a NULL pointer will be returned.
// The set does not change after this op.
//
// Example:
// int* id = ...;
// float F = ...;
// struct ordered_set* s;
// init_set(s, nsats);
// add(s, id, F);
// struct node* med = get_med(s);
// printf("Merit figure: %f\t ID pointer: %d", med->F, med->id);
*/
struct node* get_med(struct ordered_set* s) {
	if (s->size == 0) return NULL;
	struct node* copy = malloc(sizeof(struct node));
	copy_node(s->med, copy, s->id_size);
	return copy;
}

/* Returns the min element of the set
// =======================================
// s is an ordered_set pointer
//
// Returns a pointer to the node struct
// having the smaller F. If the set is
// empty, a NULL pointer will be returned.
// The set does not change after this op.
//
// Example:
// int* id = ...;
// float F = ...;
// struct ordered_set* s;
// init_set(s, nsats);
// add(s, id, F);
// struct node* min = get_min(s);
// printf("Merit figure: %f\t ID pointer: %d,", min->F, min->id);
*/
struct node* get_min(struct ordered_set* s) {
	if (s->size == 0) return NULL;
	struct node* copy = malloc(sizeof(struct node));
	copy_node(s->min, copy, s->id_size);
	return copy;
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
	if (s->size == 0) return NULL;
	struct node* result = (s->max)->value;
	if (s->size == 1) {
		s->min = NULL;
		s->max = NULL;
		s->med = NULL;
	}
	else {
		s->max = (s->max)->next;
		(s->max)->prev = NULL;
	}
	s->size--;
	return result;
}

/* Returns and delete the min element of the set
// =============================================
// s is an ordered_set pointer
//
// Returns a pointer to the node struct
// having the smaller F. If the set is
// empty, a NULL pointer will be returned.
// The new set contains the same elements as before
// except for the previous minimum.
//
// Example:
// int* id = ...;
// float F = ...;
// struct ordered_set* s;
// init_set(s, nsats);
// add(s, id, F);
// int size = size(s);
// struct node* min = get_and_delete_min(s);
// int new_size = size(s);
// printf("Previous size: %d\t New size: %d,", size, new_size);
// [new_size should be 0 and size should be 1]
*/
struct node* get_and_delete_min(struct ordered_set* s) {
	if (s->size == 0) return NULL;
	struct node* result = (s->min)->value;
	if (s->size == 1) {
		s->min = NULL;
		s->max = NULL;
		s->med = NULL;
	}
	else {
		s->min = (s->min)->prev;
		(s->min)->next = NULL;
	}
	s->size--;
	return result;
}
