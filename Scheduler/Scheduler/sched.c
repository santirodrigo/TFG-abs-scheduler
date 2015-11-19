#include "sched_mat.h"
// GLOBAL VARIABLES
int golden_index_max = 0;
int nsats;
int ntasks;
static float tic; // proportion of the number of different tasks in combination
static float max = 0; // value of the objective function
static float max_vec = 0;
static float max_new = 0;
static float max_brute = 0;

int comb_to_number(int *combination, int length, Satellite *sats) {
  int number = 0;
  int i;
  int n = length - 1;
  for (i = 0; i < length; i++) {
    if (combination[i] != 0)
      number += combination[i] * pow((double)sats[i].golden_index + 1, (double)n);
    n--;
  }
  return number;
}

int total_occurrences(Satellite *sats, int *combination) {
  int i, k;
  int *sum; // number of repetitions of each task
  int error;
  int n = 0;
  int nzeros = 0; // tasks not present in combination

  error = generate_array(ntasks, &sum);
  if (error == -1)
    return error;

  for (i = 0; i < ntasks; i++) {
    sum[i] = 0;
  }
  for (k = 0; k < nsats; k++) {
    if (combination[k] == 0)
      continue;
    for (i = 0; i < ntasks; i++) {
      if (sats[k].local_solutions[combination[k] - 1].tasks[i] == 1)
        sum[i]++;
    }
  }
  for (i = 0; i < ntasks; i++) {
    if (sum[i] == 0) {
      nzeros++;
      continue;
    }
    if (sum[i] == 1)
      continue;
    n += sum[i];
  }
  tic = (ntasks - nzeros) * 10 / ntasks;
  // printf("tic: %.2f\n", tic);
  free(sum);
  /*if (n == 0)
    return 1;
  else*/
    return n;
}
/*It computes the sum of the reward function for each satellite and solution*/
float total_reward(Satellite *sats, int *combination) {
  float sum_reward = 0;
  int k;
  for (k = 0; k < nsats; k++) {
    if (combination[k] == 0)
      continue;
    sum_reward += sats[k].local_solutions[combination[k] - 1].F;
  }
  // sum_reward *= tic;
  return sum_reward;
}
/*Checks if the current solution contains any deleted solution*/
int check_solution(Satellite *sats, int *combination) {
  int k;
  for (k = 0; k < nsats; k++) {
    if (combination[k] == 0)
      continue;
    if (sats[k].local_solutions[combination[k] - 1].F == 0.) {
      //  duplicates++;
      return 0;
    }
  }
  return 1;
}
/*It computes the next combination given the current combination.
 It starts by adding 1 to the last position of the vector and if the result
 is greater than the satellite's golden index (number of solutions), this 1
 is carried to the previous position. */
void next_combination(Satellite *sats, int *combination) {
  int n; // satellite index. It starts from the last satellite.

  n = nsats - 1;

  while (n >= 0) {
    combination[n]++;
    if (combination[n] > sats[n].golden_index) {
      combination[n] = 0;
      n--;
    } else
      return;
  }
}
/*It computes the number of combinations, which is in fact the product
 of all golden index plus one (the combination of not using the satellite)*/
long long int number_of_combinations(Satellite *sats) {
  long long int combs = sats[0].golden_index + 1;
  int k;

  for (k = 1; k < nsats; k++) {
    combs *= (sats[k].golden_index + 1);
  }
  return combs;
}

/*It updates the final solution*/
void copy_solution(int *combination, int *solution) {
  int k;

  for (k = 0; k < nsats; k++) {
    solution[k] = combination[k];
  }
}

/* Returns the maximum golden index among all satellites*/
void get_golden_index_max(Satellite *sats) {
  int k;

  for (k = 0; k < nsats; k++) {
    if (sats[k].golden_index > golden_index_max) {
      golden_index_max = sats[k].golden_index;
    }
  }
}

/*It compares if two arrays are equal. If they are, it returns 1, otherwise
 * 0.*/
int compare_solutions(int *solution1, int *solution2) {
  int i;

  for (i = 0; i < ntasks; i++) {
    if (solution1[i] != solution2[i]) {
      return 0;
    }
  }
  return 1;
}

/*If there are two equal solutions with different figure of merit in the same
 satellite, only the one with the highest figure of merit is kept. The function
 returns the number of solutions deleted in the satellite.*/
int delete_duplicates(Satellite *sats, int sol, int sat) {
  //int j;
  //int equals; // 1 if two solutions are equal, 0 otherwise
  int dup = 0;

  /*for (j = sol + 1; j < sats[sat].golden_index; j++) {
    if (sats[sat].local_solutions[j].F == 0.)
      continue;
    equals = compare_solutions(sats[sat].local_solutions[sol].tasks,
                               sats[sat].local_solutions[j].tasks);
    if (equals) {
      sats[sat].local_solutions[j].F = 0; // assuming that solutions are sorted
      dup += 1;
    }
  }*/
  return dup;
}

void sort_satellite(LocalSolution *sols, int golden_index) {
  int c, d;
  float temp;

  for (c = 0; c < (golden_index - 1); c++) {
    for (d = 0; d < golden_index - c - 1; d++) {
      if (sols[d].F < sols[d + 1].F) {
        temp = sols[d].F;
        sols[d].F = sols[d + 1].F;
        sols[d + 1].F = temp;
      }
    }
  }
}

/*It solves the problem. If we are deleteting duplicates, it checks that the
 solution is valid.*/
int solve(Satellite *sats, int *combination, int *solution) {
  int k;
  // long long int combs;
  int n;
  // int valid;
  float r, num;
  int *dups;
  //int *comb_used; //HOLA
  // int ndup = 0; // number of deleted local solutions (duplicated)
  int error;
  //int index; //HOLA

  error = generate_array(nsats, &dups);
  if (error == -1)
    return error;
  //  printf("Combinations: %ld\n", combs);
  // print_F_matrix(sats);
  get_golden_index_max(sats);
  for (k = 0; k < nsats; k++) {
    combination[k] = 1; // We start from the combination 1 1 .. 1
    /*dups[k] = 0;
    for (j = 0; j < sats[k].golden_index - 1; j++) {
      dups[k] += delete_duplicates(sats, j, k);
    }
    ndup += dups[k];*/
  }
  // print_F_matrix(sats);
  /*if (ndup > 0) {
    for (k = 0; k < nsats; k++) {
      sort_satellite(sats[k].local_solutions, sats[k].golden_index);
      sats[k].golden_index -= dups[k];
    }
  }*/
  // combs = number_of_combinations(sats);
  /*error = generate_array_long(combs, &comb_used); //HOLA
  if (error == -1) //HOLA
    return error; //HOLA
  for (k = 0; k < combs; k++) { //HOLA
    comb_used[k] = 0; //HOLA
  } //HOLA*/

  struct ordered_set s;
  init_set(&s, nsats);
  struct node *max_node;
  //index = comb_to_number(combination, nsats, sats); //HOLA
  //if (comb_used[index] != 0) printf ("ERROR!!n"); //HOLA
  r = total_reward(sats, combination);
  add(&s, combination, r);
  //comb_used[index] = 1; //HOLA
  // }
  int first = 1;
  int second = 0;
  int finished, count = 0;
  while (!is_empty(&s) || second) {
    if (second) second = 0;
    if (!first) {
    // AQUÍ ESTOY!!
    	finished = 0;
		for (n = 0; n < nsats && !finished; n++) {
		  if (combination[n] != 1) finished = 1;
		  if (combination[n] != 0) {
		    if (combination[n] == sats[n].golden_index) combination[n] = 0;
		    else combination[n]++;
		  } else
		    continue;
		  // print_array("C", combination, nsats);
		  // index = comb_to_number(combination, nsats, sats); //HOLA
		  // printf("Index: %d\n", index);

		  //  if (comb_used[index] == 0) {
		  // print_array("C", combination, nsats);
    	  // printf("F: %f\n", max_node->F);
    	  // if (comb_used[index] != 0) printf ("ERROR!!n"); //HOLA
		  // comb_used[index] = 1; //HOLA
		  r = total_reward(sats, combination);
		  //rep = total_occurrences(sats, combination);
		  // num = r * (1. - (float)rep / (float)(nsats*ntasks));
		  if (r > max) add(&s, &combination[0], r);
		  //  comb_used[index] = 1;
		  // }

		  if (combination[n] == 0)
		    combination[n] = sats[n].golden_index;
		  else
		    combination[n]--;
		}
	}
	else {
		first = 0;
		second = 1;
	}
    max_node = get_and_delete_max(&s);
    if (max_node == NULL) {
    	return 0;
    }
    combination = max_node->id;
    // print_array("Comb", combination, nsats);
    r = max_node->F;
    if (r <= max)
      break;

    n = total_occurrences(sats, combination);
    // printf ("total ocurrences: %d\n", n);
    num = r * (1. - (float)n / (float)(nsats*ntasks));
    if (num > max) {
      max = num;
      //printf("New max = %f; F = %f; n = %d\n", max, r, n);
      copy_solution(combination, solution);
    }
    count++;
	//if (!(count & 0xFFF)) printf("Count = %d; F = %f; set_size = %d\n", count, r, size(&s));
    // print_array("Comb", combination, nsats);
    // printf("%.2f ", r);
    // printf("%d ", n);
    // printf("%.2f\n", num);
  }
  // printf("Count = %d\n", count);
  free(dups);
  //  free(comb_used);
  return 0;
}

int add_successors(int* combination, Satellite* sats, struct ordered_set* s, int max_rep, int max_rec, int d) {
	int n, finished = 0, rep, rep_succ;
	float r;
	for (n = 0; n < nsats && !finished; n++) {
	  if (combination[n] != 1) finished = 1;
	  if (combination[n] != 0) {
	    if (combination[n] == sats[n].golden_index) combination[n] = 0;
	    else combination[n]++;
	  } else
	    continue;
	  r = total_reward(sats, combination);
	  int m = 0;
	  int opt_comb[nsats];
	  int fin = 0;
	  while (m < nsats) {
	  	if (fin) opt_comb[m] = combination[m]; 
	  	else opt_comb[m] = 0;
	  	if (!fin && combination[m] != 1) fin = 1;
	  	m++;
	  }
	  // int temp;
	  // for (temp = 0; temp < nsats; temp++) printf("opt_comb[%d] = %d\n",temp,opt_comb[temp]);
	  rep_succ = total_occurrences(sats, &opt_comb[0]);
	  rep = total_occurrences(sats, combination);
	  // num = r * (1. - (float)rep / (float)(nsats*ntasks));
	  if (rep_succ <= max_rep && r > max_rec) {
	    if (rep <= max_rep || d > nsats) add(s, &combination[0], r);
	  	else add_successors(&combination[0], sats, s, max_rep, max_rec, d+1);
	  }
	  // if (d > 50) printf("Rec level %d\n", d);
	  if (combination[n] == 0)
	    combination[n] = sats[n].golden_index;
	  else
	    combination[n]--;
	}
	return 0;
}

int solve_rec(Satellite *sats, int *combination, int *solution) {
  int k;
  // long long int combs;
  int n, max_rep;
  float r, num;
  int *dups;
  // int ndup = 0; // number of deleted local solutions (duplicated)
  int error;
  float max_rec = 0;

  error = generate_array(nsats, &dups);
  if (error == -1)
    return error;
  get_golden_index_max(sats);
  for (k = 0; k < nsats; k++) {
    combination[k] = 1; // We start from the combination 1 1 .. 1
  }
  // combs = number_of_combinations(sats);

  struct ordered_set s;
  init_set(&s, nsats);
  struct node *max_node;
  r = total_reward(sats, combination);
  add(&s, combination, r);
  int first = 1;
  int second = 0;
  int count = 0;
  while (!is_empty(&s) || second) {
    if (second) second = 0;
    if (!first) {
    	add_successors(&combination[0], sats, &s, max_rep, max_rec, 0);
	}
	else {
		first = 0;
		second = 1;
	}
    max_node = get_and_delete_max(&s);
    combination = max_node->id;
    // print_array("Comb", combination, nsats);
    r = max_node->F;
    if (r <= max_rec)
      break;

    n = total_occurrences(sats, combination);
    // printf ("total ocurrences: %d\n", n);
    num = r * (1. - (float)n / (float)(nsats*ntasks));
    if (num > max_rec) {
      max_rec = num;
      max_rep = n;
      printf("New max = %f; F = %f; n = %d\n", max_rec, r, n);
      copy_solution(combination, solution);
    }
    count++;
	if (!(count & 0xFFF)) printf("Count = %d; F = %f; set_size = %d\n", count, r, size(&s));
  }
  printf("Count = %d\n", count);
  free(dups);
  return 0;
}

int solve_new(Satellite *sats, int *combination, int *solution) {
  int k;
  // long long int combs;
  int n, max_rep;
  float r, num;
  int *dups;
  // int ndup = 0; // number of deleted local solutions (duplicated)
  int error;

  error = generate_array(nsats, &dups);
  if (error == -1)
    return error;
  get_golden_index_max(sats);
  for (k = 0; k < nsats; k++) {
    combination[k] = 1; // We start from the combination 1 1 .. 1
  }
  // combs = number_of_combinations(sats);

  struct ordered_set s;
  init_set(&s, nsats);
  struct node *max_node;
  r = total_reward(sats, combination);
  add(&s, combination, r);
  int first = 1;
  int second = 0;
  int finished, rep, count = 0;
  while (!is_empty(&s) || second) {
    if (second) second = 0;
    if (!first) {
    	finished = 0;
		for (n = 0; n < nsats && !finished; n++) {
		  if (combination[n] != 1) finished = 1;
		  if (combination[n] != 0) {
		    if (combination[n] == sats[n].golden_index) combination[n] = 0;
		    else combination[n]++;
		  } else
		    continue;
		  r = total_reward(sats, combination);
		  int m = 0;
		  int opt_comb[nsats];
		  int fin = 0;
		  while (m < nsats) {
		  	if (fin) opt_comb[m] = combination[m]; 
		  	else opt_comb[m] = 0;
		  	if (!fin && combination[m] != 1) fin = 1;
		  	m++;
		  }
		  // int temp;
		  // for (temp = 0; temp < nsats; temp++) printf("opt_comb[%d] = %d\n",temp,opt_comb[temp]);
		  rep = total_occurrences(sats, &opt_comb[0]);
		  // rep = total_occurrences(sats, combination);
		  // num = r * (1. - (float)rep / (float)(nsats*ntasks));
		  if (rep <= max_rep && r > max_new) add(&s, &combination[0], r);

		  if (combination[n] == 0)
		    combination[n] = sats[n].golden_index;
		  else
		    combination[n]--;
		}
	}
	else {
		first = 0;
		second = 1;
	}
    max_node = get_and_delete_max(&s);
    combination = max_node->id;
    // print_array("Comb", combination, nsats);
    r = max_node->F;
    if (r <= max_new)
      break;

    n = total_occurrences(sats, combination);
    // printf ("total ocurrences: %d\n", n);
    num = r * (1. - (float)n / (float)(nsats*ntasks));
    if (num > max_new) {
      max_new = num;
      max_rep = n;
#ifdef DEBUGBASIC
      printf("New max = %f; F = %f; n = %d\n", max_new, r, n);
#endif
      copy_solution(combination, solution);
    }
    count++;
#ifdef DEBUGBASIC
	if (!(count & 0xFFF)) printf("Count = %d; F = %f; set_size = %d\n", count, r, size(&s));
#endif
  }
#ifdef DEBUGBASIC
  printf("Count = %d\n", count);
#endif
  free(dups);
  return 0;
}

int solve_vec(Satellite *sats, int *combination, int *solution) {
  int k;
  int n, max_rep = nsats*ntasks; // Es el número máximo de repeticiones posible.
  float r, num;
  int *dups;
  int error, vec;
#ifdef DEBUGMN
  	  	printf("\033[22;31mGenerating array\n\033[0m");
#endif
  error = generate_array(nsats, &dups);
  if (error == -1)
    return error;
  get_golden_index_max(sats);
  for (k = 0; k < nsats; k++) {
    combination[k] = 1; // We start from the combination 1 1 .. 1
  }
#ifdef DEBUGMN
  	  	printf("\033[22;31mFirst combination set!\n\033[0m");
#endif
  int vector_size = nsats*golden_index_max;
  struct ordered_set* sets[vector_size];
  for (vec = 0; vec < vector_size; vec++) {
	  sets[vec] = malloc(sizeof(struct ordered_set));
	  if (sets[vec] != NULL) init_set(sets[vec], nsats);
	  else {
	      fprintf(stderr, "Out of mem\n");
	      return -1;
	  }
  }
#ifdef DEBUGMN
  	  	printf("\033[22;31mSets vector initialized!\n\033[0m");
#endif
  struct node *max_node;
  r = total_reward(sats, combination);
  add(sets[0], combination, r);
  int first = 1;
  int second = 0;
  int set = 1;
  int finished, length = 1, rep, count = 0;
  while ((length > 0) || second) {
    if (second) second = 0;
    if (!first) {
#ifdef DEBUGMN
				printf("\033[22;31mNot first iteration!\n\033[0m");
#endif
    	finished = 0;
		for (n = 0; n < nsats && !finished; n++) {
		  if (combination[n] != 1) finished = 1;
		  if (combination[n] != 0) {
			if (combination[n] == sats[n].golden_index) combination[n] = 0;
			else combination[n]++;
			r = total_reward(sats, combination);
			int m = 0;
			int opt_comb[nsats];
			int fin = 0;
			while (m < nsats) {
				if (fin) opt_comb[m] = combination[m]; 
				else opt_comb[m] = 0;
				if (!fin && combination[m] != 1) fin = 1;
				m++;
			}
			rep = total_occurrences(sats, &opt_comb[0]);
#ifdef DEBUGMN
				printf("\033[22;31mNew son!! rep: %d, r: %f\n\033[0m", rep, r);
#endif
			if (rep <= max_rep && r >= max_vec) { // r >= max_vec mejor que r > max_vec ??
				add(sets[set], &combination[0], r);
#ifdef DEBUGMN
				printf("\033[22;31mNew element added!!\n\033[0m");
#endif
				set++;
				if (set == vector_size) set = 0;
			}
			if (combination[n] == 0)
				combination[n] = sats[n].golden_index;
			else
				combination[n]--;
		  }
		}
	}
	else {
		first = 0;
		second = 1;
#ifdef DEBUGMN
				printf("\033[22;31mFirst iteration!\n\033[0m");
#endif
	}
	int nmax = -1;
	float temp_max = 0;
	for (vec = 0; vec < vector_size; vec++) {
		if (size(sets[vec]) > 0) {
			float itmax = get_max(sets[vec])->F;
			if(itmax >= temp_max) {
				temp_max = itmax;
				nmax = vec;
			}
		}
	}
    if (nmax == -1)  {
#ifdef DEBUGMN
				printf("\033[22;31mVacío!\n\033[0m");
#endif
    	break;
    }
    max_node = get_and_delete_max(sets[nmax]);
    combination = max_node->id;
    r = max_node->F;
    if (r <= max_vec)
    	break;
    n = total_occurrences(sats, combination);
    num = r * (1. - (float)n / (float)(nsats*ntasks));
#ifdef DEBUGMN
				printf("\033[22;31mn = %d, num = %f, max_vec = %f\n\033[0m", n, num, max_vec);
#endif
    if (num > max_vec) {
      max_vec = num;
      max_rep = n;
#ifdef DEBUGBASIC
  	  	printf("\033[22;31mNew max = %f; F = %f; n = %d\n\033[0m", max_vec, r, n);
#endif
      copy_solution(combination, solution);
    }
    free(max_node);
    count++;
	if (!(count & 0xFFF)) {
#ifdef DEBUGBASIC
  	  	printf("Count = %d; F = %f\n", count, r);
#endif
#ifdef DEBUGMN
  	  	for (vec = 0; vec < vector_size; vec++) 
			printf("set_size[%d] = %d\n", vec, size(sets[vec]));
		printf("\n");
#endif
	}
	length = size(sets[0]);
	for (vec = 1; vec < vector_size; vec++) {
		length += size(sets[vec]);
	}
  }
#ifdef DEBUGBASIC
  printf("Count = %d\n", count);
#endif
  for (vec = 0; vec < vector_size; vec++) {
	free(sets[vec]);
  }
  free(dups);
  return 0;
}

int solve_brute(Satellite *sats, int *combination, int *solution) {
  int k;
  long long int combs, i;
  int n;
  float r, num;
  int *dups;
  // int ndup = 0; // number of deleted local solutions (duplicated)
  int error;

  error = generate_array(nsats, &dups);
  if (error == -1)
    return error;
  get_golden_index_max(sats);
  for (k = 0; k < nsats; k++) {
    combination[k] = 1; // We start from the combination 1 1 .. 1
  }
  combs = number_of_combinations(sats);
  int count = 0;
  for (i = 0; i < combs; i++) {
  	n = total_occurrences(sats, combination);
  	r = total_reward(sats, combination);
	num = r * (1. - (float)n / (float)(nsats*ntasks));
    if (num > max_brute) {
      max_brute = num;
#ifdef DEBUGBASIC
      printf("New max = %f; F = %f; n = %d\n", max_brute, r, n);
#endif
      copy_solution(combination, solution);
    }
    count++;
#ifdef DEBUGBASIC
	if (!(count & 0xFFFFFF)) printf("Count = %d; F = %f\n", count, r);
#endif
	int finished = 0;
	for (n = 0; n < nsats && !finished; n++) {
		combination[n]++;
		if (combination[n] > sats[n].golden_index) {
			combination[n] = 0;
		} else finished = 1;
	}
  }
  free(dups);
  return 0;
}


int allocate_satellites(Satellite *sats) {
  int j, k;
  int error;
  for (k = 0; k < nsats; k++) {
    sats[k].id = k + 1;
    sats[k].golden_index = golden_index_max;
    error =
        generate_array_struct(sats[k].golden_index, &(sats[k].local_solutions));
    if (error == -1)
      return error;
    for (j = 0; j < sats[k].golden_index; j++) {
      error = generate_array(ntasks, &(sats[k].local_solutions[j].tasks));
      if (error == -1) {
        return error;
      }
    }
  }
  return 0;
}

int init_satellites(Satellite **sats) {
  int k;
  int error;

  error = generate_array_satellites(nsats, sats);
  if (error == -1)
    return error;
  allocate_satellites(*sats);
  for (k = 0; k < nsats; k++) {
    generate_solutions(&(*sats)[k]);
  }
  return 0;
}

void free_satellites(Satellite *sats) {
  int j, k;
  for (k = 0; k < nsats; k++) {
    for (j = 0; j < sats[k].golden_index; j++) {
      free(sats[k].local_solutions[j].tasks);
    }
    free(sats[k].local_solutions);
  }
  free(sats);
}

int main(int argc, char *argcv[]) {

  float /*combs, deleted_combs,*/ bf_time /*,time_difference*/;
  int error;
  int *combination_b, *combination_v, *combination_n, *solution_b, *solution_v, *solution_n;
  Satellite *sats;
  struct timeval time;
  struct timezone tz;
  suseconds_t start_time_u, final_time_u;
  time_t start_time_s, final_time_s;

  if (argc != 4) {
    fprintf(stderr, "Usage: %s tasks solutions satellites\n",
            basename(argcv[0]));
    return 1;
  } else {
    ntasks = atoi(argcv[1]);
    golden_index_max = atoi(argcv[2]);
    nsats = atoi(argcv[3]);
  }
  printf("%d %d %d ", ntasks, golden_index_max, nsats);
  if (gettimeofday(&time, &tz))
    return 1;
  start_time_u = time.tv_usec;
  srand(start_time_u); // The seed of the random number

  error = generate_array(nsats, &combination_b);
  if (error == -1)
    return error;
  error = generate_array(nsats, &solution_b);
  if (error == -1)
    return error;
    error = generate_array(nsats, &combination_v);
  if (error == -1)
    return error;
  error = generate_array(nsats, &solution_v);
  if (error == -1)
    return error;
    error = generate_array(nsats, &combination_n);
  if (error == -1)
    return error;
  error = generate_array(nsats, &solution_n);
  if (error == -1)
    return error;
  error = init_satellites(&sats);
  if (error == -1)
    return error;
  if (gettimeofday(&time, &tz))
    return 1;
    
  start_time_u = time.tv_usec;
  start_time_s = time.tv_sec;
  solve_brute(sats, combination_b, solution_b);
  if (gettimeofday(&time, &tz))
    return 1;
  final_time_u = time.tv_usec;
  final_time_s = time.tv_sec;
  bf_time =
      (final_time_s - start_time_s) * 1000000 + final_time_u - start_time_u;
  printf(" | %.0f\t| %f\t", bf_time, max_brute);
  print_array("Solution (brute)", solution_b, nsats);
  free(solution_b);
  free(combination_b);
  
  /*start_time_u = time.tv_usec;
  start_time_s = time.tv_sec;
  solve_vec(sats, combination_v, solution_v);
  if (gettimeofday(&time, &tz))
    return 1;
  final_time_u = time.tv_usec;
  final_time_s = time.tv_sec;
  bf_time =
      (final_time_s - start_time_s) * 1000000 + final_time_u - start_time_u;
  printf(" | %.0f\t| %f\t", bf_time, max_vec);
  print_array("Solution (vec)", solution_v, nsats);
  free(solution_v);
  free(combination_v);*/
  
  /*start_time_u = time.tv_usec;
  start_time_s = time.tv_sec;
  solve_new(sats, combination_n, solution_n);
  if (gettimeofday(&time, &tz))
    return 1;
  final_time_u = time.tv_usec;
  final_time_s = time.tv_sec;
  bf_time =
      (final_time_s - start_time_s) * 1000000 + final_time_u - start_time_u;
  printf(" | %.0f\t| %f\t", bf_time, max_new);
  print_array("Solution (new)", solution_n, nsats);
  // printf("Max: %.2f\n", max);
  // printf("Duplicates: %d\n", duplicates);*/
  //printf("\n");
  start_time_u = time.tv_usec;
  start_time_s = time.tv_sec;
  solve(sats, combination_n, solution_n);
  if (gettimeofday(&time, &tz))
    return 1;
  final_time_u = time.tv_usec;
  final_time_s = time.tv_sec;
  bf_time =
      (final_time_s - start_time_s) * 1000000 + final_time_u - start_time_u;
  printf(" | %.0f\t| %f\t", bf_time, max);
  print_array("Solution (old)", solution_n, nsats);
  printf("\n");
  free_satellites(sats);
  free(solution_n);
  free(combination_n);
  return 0;
}
