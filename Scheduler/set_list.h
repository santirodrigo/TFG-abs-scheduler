struct list_node {
	struct ordered_set* value;
	struct list_node* next;
}

struct set_list {
	int size;
	struct list_node* first;
}

/* Initialize the set list
// ==========================
// l is a set_list pointer
//
// Example:
// struct set_list* sl;
// init_set(sl);
*/
void init_set_list(struct set_list* l);

/* Adds a new ordered set to the set list
// =======================================
// s is an ordered_set pointer
// sl is an initialized set_list
//
// The resulting set after this operation 
// includes the new set.
//
// Example:
// struct ordered_set* s;
// init_set(s, nsats);
// struct set_list* sl;
// init_set_list(sl);
// add(sl, s);
*/
void add(struct set_list* sl, struct ordered_set* s);

/* Returns and delete the max element on all the sets in the list
// ==============================================================
// s is an ordered_set pointer
//
// Returns a pointer to the node struct
// having the larger F in all the sets
// contained in the list. If the set list is
// empty, a NULL pointer will be returned.
// The new set list contains the same sets
// but modified (the previous maximum elements
// has been deleted).
//
// Example:
// int* id = ...;
// float F = ...;
// struct ordered_set* s;
// init_set(s, nsats);
// add(s, id, F);
// struct set_list* sl;
// init_set_list(sl);
// add(sl, s);
// struct node* max = get_and_delete_max(sl);
*/
struct node* get_and_delete_max(struct set_list* sl);
