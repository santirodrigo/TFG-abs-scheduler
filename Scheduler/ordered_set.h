struct node {
	float F;
	int* id;
};

struct set_node {
	struct node* value;
	struct set_node* next;
};

struct ordered_set {
	struct set_node* max;
	struct set_node* min;
	int size;
	int id_size;
};

/* Initialize the ordered set
// ==========================
// s is an ordered_set pointer
// id_size is the length of the node's ID array
//
// Example:
// struct ordered_set* s;
// init_set(s, nsats);
*/
void init_set(struct ordered_set* s, int id_size);

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
int size(struct ordered_set* s);

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
int is_empty(struct ordered_set* s);

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
void add(struct ordered_set* s, int* id, float F);

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
struct node* get_max(struct ordered_set* s);

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
struct node* get_min(struct ordered_set* s);

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
struct node* get_and_delete_max(struct ordered_set* s);

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
struct node* get_and_delete_min(struct ordered_set* s);

/* EXAMPLE OF SUM(F) DESCENDING 
=====================================================================
ordered_set s;
init_set(&s, nsats);
int combination[nsats], i, n;
struct node* max;
for (i = 0; i < nsats; i++) {
	combination[i] = 1;
}
float F = total_reward(sats, combination);
add(&s, combination, F);
while(!is_empty(&s)){ // Delta^s iteraciones
	for (n = nsats -1; n >= 0; n--) { // s iteraciones
		if (combination[n] != 0) {
			combination[n] = (combination[n]+1)%(golden_index[n]+1);
			float F = total_reward(sats, combination); // s iteraciones
			add(&s, &combination[0], F); // Caso mejor: O(1); Caso peor: O((Delta^s)/2); Caso medio: O((Delta^s)/4)
			if (combination[n] == 0) combination[n] = golden_index[n];
			else combination[n]--;
		}
	}
	max = get_and_delete_max(s);
	combination = max->id;
	printf("%f:", max->F);
}*/

// TOTAL : O(Delta^(2s))