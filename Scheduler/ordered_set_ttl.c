#include "ordered_set_ttl.h"
#include <stdlib.h>
#ifdef DEBUG
	#include <stdio.h>
#endif

int is_even(int n) {
	return !(n%2);
}

int is_odd(int n) {
	return is_even(n+1);
}

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
#ifdef DEBUG
	printf("\033[22;31m\n>>>>DEBUG: Ordered_set initialized correctly with id_size %d\n\033[0m", s->id_size);
#endif	
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
// element's TTL is reduced in 1 unit. Else it 
// includes the new element and renew the 
// values of the max and min elements, if 
// necessary.
//
// The TTL value is defined as the number of
// combinatorial predecessors of the node.
// This is the same as defining TTL as the
// number of its ID positions different from 1
// E.g.: The ID 1,1,...,1 has a TTL of 0, and
// the ID 1,2,2,1,1,...1 has a TTL of 2.
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
	int predecessors = 0;
	new->F = F;
	new->id = malloc(s->id_size*sizeof(int));
	for(i = 0; i < s->id_size; i++) {
		new->id[i] = id[i];
		if (id[i] != 1) predecessors++;
	}
#ifdef DEBUG
	printf("\033[22;31m\n>>>>DEBUG: This element has %d predecessor(s)\n\033[0m", predecessors);
#endif
	struct set_node* elem = malloc(sizeof(struct set_node));
	elem->value = new;
	elem->ttl = predecessors;
	if (s->size == 0) {
		s->max = elem;
		s->min = elem;
		s->med = elem;
		elem->next = NULL;
		elem->prev = NULL;
#ifdef DEBUG
		printf("\033[22;31m\n>>>>DEBUG: This element is the first on the set\n\033[0m");
#endif
	} else if (s->size == 1) {
		if (((s->max)->value)->F < new->F) {
			elem->next = s->max;
			elem->prev = NULL;
			(s->max)->prev = elem;
			s->max = elem;
		} else if (are_equal((s->max)->value,new,s->id_size)) {
			((s->max)->ttl)--;
#ifdef DEBUG
			printf("\033[22;31m\n>>>>DEBUG: This element is identical to the maximum\n\033[0m");
#endif
			return;
		} else {
			elem->prev = s->max;
			elem->next = NULL;
			(s->max)->next = elem;
			s->min = elem;
			s->med = elem;
		}
	} else if (s->size == 2) {
		if (((s->max)->value)->F < new->F) {
			elem->next = s->max;
			elem->prev = NULL;
			(s->max)->prev = elem;
			s->max = elem;
			s->med = elem->next;
		} else if (are_equal((s->max)->value,new,s->id_size)) {
			((s->max)->ttl)--;
#ifdef DEBUG
			printf("\033[22;31m\n>>>>DEBUG: This element is identical to the maximum\n\033[0m");
#endif
			return;
		} else if (((s->min)->value)->F < new->F) {
			elem->next = s->min;
			elem->prev = s->max;
			(s->max)->next = elem;
			(s->min)->prev = elem;
			s->med = elem;
		} else if (are_equal((s->min)->value,new,s->id_size)) {
			((s->min)->ttl)--;
#ifdef DEBUG
			printf("\033[22;31m\n>>>>DEBUG: This element is identical to the minimum\n\033[0m");
#endif
			return;
		} else {
			elem->next = NULL;
			elem->prev = s->min;
			(s->min)->next = elem;
			s->min = elem;
		}
	} else if (((s->med)->value)->F < new->F) { 
#ifdef DEBUG
		printf("\033[22;31m\n>>>>DEBUG: This element is greater than the median\n\033[0m");
#endif
		struct set_node* n = s->max;
		while ((n->value)->F >= new->F) {
			if (are_equal(n->value, new, s->id_size)) {
				(n->ttl)--;
				return;
			}
			n = n->next;
		}
		if (n == s->max) {
			s->max = elem;
			elem->prev = NULL;
			elem->next = n;
			n->prev = elem;
		} else {		
			elem->prev = n->prev;
			elem->next = n;
			(n->prev)->next = elem;
			n->prev = elem;
		}
		if (is_even(s->size)) s->med = s->med->prev;
	} else {
		struct set_node* n;
		if (((s->med)->value)->F > new->F) n = s->med;
		else n = s->max;
		while (n->next != NULL && (n->value)->F >= new->F) {
			if (are_equal(n->value, new, s->id_size)) {
				(n->ttl)--;
				return;
			}
			n = n->next;
		}
		if (n == s->min && n->value->F >= new->F) {
			s->min = elem;
			elem->next = NULL;
			elem->prev = n;
			n->next = elem;
		} else {		
			elem->prev = n->prev;
			elem->next = n;
			(n->prev)->next = elem;
			n->prev = elem;
		}
		if (is_odd(s->size)) s->med = s->med->next;
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

/* Returns the max (TTL = 0) element of the set
// ============================================
// s is an ordered_set pointer
//
// Returns a pointer to the node struct
// having the larger F and a TTL = 0. If the set is
// empty or there is no node with TTL = 0
// a NULL pointer will be returned.
// The set does not change after this op.
//
// Example:
// int* id = ...;
// float F = ...;
// struct ordered_set* s;
// init_set(s, nsats);
// add(s, id, F);
// struct node* max = get_max(s);
// printf("Merit figure: %f\t ID pointer: %d", max->F, max->id);
*/
struct node* get_max(struct ordered_set* s) {
	if (s->size == 0) return NULL;
	struct node* copy = malloc(sizeof(struct node));
	struct set_node* n = s->max;
	while(n != NULL && n->ttl != 0) {
		n = n->next;
	}
	if (n == NULL) {
#ifdef DEBUG
		printf("\033[22;31m\n>>>>DEBUG: There is no node with TTL = 0\n\033[0m");
#endif
		return NULL; // There is no node with TTL = 0
	}
	copy_node(n, copy, s->id_size);
	return copy;
}

/* Returns the max element of the set
// =======================================
// s is an ordered_set pointer
//
// Returns a pointer to the node struct
// having the larger F. If the set is
// empty, a NULL pointer will be returned.
// The set does not change after this op.
// The TTL value is not taken into account.
//
// Example:
// int* id = ...;
// float F = ...;
// struct ordered_set* s;
// init_set(s, nsats);
// add(s, id, F);
// struct node* max = get_max(s);
// printf("Merit figure: %f\t ID pointer: %d", max->F, max->id);
*/
struct node* get_max_no_TTL(struct ordered_set* s) {
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
// The TTL value is not taken into account.
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
// The TTL value is not taken into account.
//
// Example:
// int* id = ...;
// float F = ...;
// struct ordered_set* s;
// init_set(s, nsats);
// add(s, id, F);
// struct node* min = get_min(s);
// printf("Merit figure: %f\t ID pointer: %d", min->F, min->id);
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
// having the larger F and a TTL = 0. If the set is
// empty or there is no node with TTL = 0,
// a NULL pointer will be returned.
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
	struct set_node* n = s->max;
	int medseen = 0;
	while(n != NULL && n->ttl != 0) {
		if (n == s->med) medseen = 1;
		n = n->next;
	}
	if (n == NULL) {
#ifdef DEBUG
		printf("\033[22;31m\n>>>>DEBUG: There is no node with TTL = 0\n\033[0m");
#endif
		return NULL; // There is no node with TTL = 0
	}
	if (s->size == 1) {
		s->min = NULL;
		s->max = NULL;
		s->med = NULL;
#ifdef DEBUG
		printf("\033[22;31m\n>>>>DEBUG: The set is now empty\n\033[0m");
#endif
	} else if (s->size == 2) {
		if (n == s->max) {
			(s->min)->prev = NULL;
			s->max = s->min;
		} else {
			(s->max)->next = NULL;
			s->med = s->max;
			s->min = s->max;
		}
	} else {
		if (is_even(s->size) && (medseen || n == s->med)) s->med = s->med->prev;
		else if (is_odd(s->size) && !medseen) s->med = s->med->next;
		if (n == s->max) {
			((s->max)->next)->prev = NULL;
			s->max = (s->max)->next;
#ifdef DEBUG
			printf("\033[22;31m\n>>>>DEBUG: The maximum has TTL = 0\n\033[0m");
#endif
		} else if (n == s->min) {
			((s->min)->prev)->next = NULL;
			s->min = (s->min)->prev;
		} else {
			(n->prev)->next = n->next;
			(n->next)->prev = n->prev;
#ifdef DEBUG
			printf("\033[22;31m\n>>>>DEBUG: The element out is not very special\n\033[0m");
#endif
		}
	}
	s->size--;
#ifdef DEBUG
			printf("\033[22;31m\n>>>>DEBUG: The element out's TTL is %d\n\033[0m", n->ttl);
#endif
	return n->value;
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
// The TTL value is not taken into account.
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
struct node* get_and_delete_max_no_TTL(struct ordered_set* s) {
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
		if (is_odd(s->size)) s->med = s->med->next;
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
// The TTL value is not taken into account.
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
		if (is_even(s->size)) s->med = s->med->prev;
	}
	s->size--;
	return result;
}
