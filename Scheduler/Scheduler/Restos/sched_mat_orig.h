#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <libgen.h>
#include <sys/time.h>
#include <math.h>

extern int ntasks;
extern int nsats;
extern int golden_index_max;

typedef struct local_solution {
  int id;
  float F;
  int *tasks;
} LocalSolution;

typedef struct satellite {
  int id;
  int golden_index;
  LocalSolution *local_solutions;
} Satellite;

struct node {
  int *id;
  float F;
};

struct set_node {
  struct node *value;
  struct set_node *next;
  struct set_node *prev;
  int ttl;
};

struct ordered_set {
  struct set_node *max;
  struct set_node *min;
  struct set_node *med;
  int size;
  int id_size;
};

/*It generates an empty array of a certain length*/
int generate_array(int length, int **p_array);

/*It generates an empty array of floats of a certain length*/
int generate_array_float(int length, float **p_array);

/*It generates an empty array of structs of a certain length*/
int generate_array_struct(int length, LocalSolution **p_array);

int generate_array_long(long long int length, int **p_array);

/*It generates an empty array of local solutions of a certain length*/
int generate_array_satellites(int length, Satellite **p_array);

void print_F_matrix(Satellite *sats);

void print_t_matrix(Satellite *sats);

/*It prints an array with title the text in label */
void print_array(char *label, int *array, int length);

/*It prints an array of floats with title the text in label */
void print_array_float(char *label, float *array, int length);

// LOCAL
void generate_solutions(Satellite *sat);

//===============================================================================
//===============================================================================

/* Initialize the ordered set
 // ==========================
 // s is an ordered_set pointer
 // id_size is the l<ength of the node's ID array
 //
 // Example:
 // struct ordered_set* s;
 // init_set(s, nsats);
 */
void init_set(struct ordered_set *s, int id_size);

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
int size(struct ordered_set *s);

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
int is_empty(struct ordered_set *s);

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
void add(struct ordered_set *s, int *id, float F);

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
struct node *get_max(struct ordered_set *s);

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
struct node *get_max_no_TTL(struct ordered_set *s);

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
struct node *get_med(struct ordered_set *s);

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
struct node *get_min(struct ordered_set *s);

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
struct node *get_and_delete_max(struct ordered_set *s);

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
struct node *get_and_delete_max_no_TTL(struct ordered_set *s);

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
struct node *get_and_delete_min(struct ordered_set *s);
