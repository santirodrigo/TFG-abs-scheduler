#include "sched_mat.h"

int generate_array(int length, int **p_array) {
  int *array;

  *p_array = malloc(length * sizeof(int));
  array = *p_array;
  if (array == NULL) {
    fprintf(stderr, "out of memory\n");
    return -1;
  } else
    return 0;
}

int generate_array_float(int length, float **p_array) {
  float *array;

  *p_array = malloc(length * sizeof(float));
  array = *p_array;
  if (array == NULL) {
    fprintf(stderr, "out of memory\n");
    return -1;
  } else
    return 0;
}
int generate_array_long(long long int length, int **p_array) {
  int *array;

  *p_array = malloc(length * sizeof(int));
  array = *p_array;
  if (array == NULL) {
    fprintf(stderr, "out of memory\n");
    return -1;
  } else
    return 0;
}

int generate_array_struct(int length, LocalSolution **p_array) {
  LocalSolution *array;

  *p_array = malloc(length * sizeof(LocalSolution));
  array = *p_array;
  if (array == NULL) {
    fprintf(stderr, "out of memory\n");
    return -1;
  } else
    return 0;
}
int generate_array_satellites(int length, Satellite **p_array) {
  Satellite *array;

  *p_array = malloc(length * sizeof(Satellite));
  array = *p_array;
  if (array == NULL) {
    fprintf(stderr, "out of memory\n");
    return -1;
  } else
    return 0;
}

void print_array(char *label, int *array, int length) {
  int i;
  printf("%s: ", label);
  for (i = 0; i < length; i++) {
    printf("%d ", array[i]);
  }
  printf("\t");
}

void print_array_float(char *label, float *array, int length) {
  int i;
  printf("%s: ", label);
  for (i = 0; i < length; i++) {
    printf("%.2f ", array[i]);
  }
  printf("\n");
}

void print_F_matrix(Satellite *sats) {
  int j, k;

  for (k = 0; k < nsats; k++) {
    for (j = 0; j < golden_index_max; j++) {
      if (j >= sats[k].golden_index) {
        printf("0.00 ");
        continue;
      }
      printf("%.2f ", sats[k].local_solutions[j].F);
    }
    printf("\n");
  }
  printf("\n");
}

void print_t_matrix(Satellite *sats) {
  int i, j, k;

  for (k = 0; k < nsats; k++) {
    for (j = 0; j < sats[k].golden_index; j++) {
      for (i = 0; i < ntasks; i++) {
        printf("%d ", sats[k].local_solutions[j].tasks[i]);
      }
      printf("\n");
    }
    printf("\n");
  }
  printf("\n");
}
