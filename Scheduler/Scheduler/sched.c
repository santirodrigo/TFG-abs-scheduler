#include "sched_mat.h"

int golden_index_max = 0;
int nsats;
int ntasks;
static float max = 0;
static float max_brute = 0;

// Returns the number of repetitions of a combination of selected local
// schedules as the sum[t in {task_1,...,task_n}] {ocurrences(t) - 1}
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
			continue; // Substitute by if (combination[k] != 0) { for...}
		for (i = 0; i < ntasks; i++) {
			if (sats[k].local_solutions[combination[k] - 1].tasks[i] == 1)
			sum[i]++;
		}
	}
	
	for (i = 0; i < ntasks; i++) {
		if (sum[i] == 0) {
			nzeros++;
			continue; // Substitute by an else clause
			}
		if (sum[i] == 1)
			continue; // Substitute by if (sum[i] > 1) n += sum[i];
		n += sum[i]; // Mmmmm... Estoy sumando la vez que la hago bien y las repeticiones...
	}
	free(sum);
	return n;
}

int task_count(Satellite *sats, int *combination) {
	int i, k;
	int *present; // set if a task is done in this combination
	int error;
	int n = 0;

	error = generate_array(ntasks, &present);
	if (error == -1)
		return error;

	for (i = 0; i < ntasks; i++) {
		present[i] = 0;
	}
	
	for (k = 0; k < nsats; k++) {
		if (combination[k] != 0) {
			for (i = 0; i < ntasks; i++) {
				if (sats[k].local_solutions[combination[k] - 1].tasks[i] == 1 && present[i] == 0)
					present[i] = 1;
			}
		}
	}
	
	for (i = 0; i < ntasks; i++) {
		if (present[i] == 1) n += present[i];
	}
	free(present);
	return n;
}

// Computes the sum of the reward function for each satellite and solution
float total_reward(Satellite *sats, int *combination) {
	float sum_reward = 0;
	int k;
	for (k = 0; k < nsats; k++) {
		if (combination[k] == 0)
			continue;
		sum_reward += sats[k].local_solutions[combination[k] - 1].F;
	}
	return sum_reward;
}

// It computes the number of combinations, which is in fact the product
// of all golden index plus one (the combination of not using the satellite)
long long int number_of_combinations(Satellite *sats) {
	long long int combs = sats[0].golden_index + 1;
	int k;

	for (k = 1; k < nsats; k++) {
		combs *= (sats[k].golden_index + 1);
	}
	
	return combs;
}

// It updates the final solution
void copy_solution(int *combination, int *solution) {
	int k;

	for (k = 0; k < nsats; k++) {
		solution[k] = combination[k];
	}
}

// Returns the maximum golden index among all satellites
void get_golden_index_max(Satellite *sats) {
	int k;

	for (k = 0; k < nsats; k++) {
		if (sats[k].golden_index > golden_index_max) {
			golden_index_max = sats[k].golden_index;
		}
	}
}

//It compares if two arrays are equal. If they are, it returns 1, otherwise 0
int compare_solutions(int *solution1, int *solution2) {
	int i;

	for (i = 0; i < ntasks; i++) {
		if (solution1[i] != solution2[i]) {
			return 0;
		}
	}
	return 1;
}

// SRM: DEPRECATED UNLESS IT IS FOUND USEFUL ///////////////////////////////////
// If there are two equal solutions with different figure of merit in the same
// satellite, only the one with the highest figure of merit is kept. It
// returns the number of solutions deleted in the satellite.
/*int delete_duplicates(Satellite *sats, int sol, int sat) {*/
/*  int j;*/
/*  int equals; // 1 if two solutions are equal, 0 otherwise*/
/*  int dup = 0;*/

/*  for (j = sol + 1; j < sats[sat].golden_index; j++) {*/
/*    if (sats[sat].local_solutions[j].F == 0.)*/
/*      continue;*/
/*    equals = compare_solutions(sats[sat].local_solutions[sol].tasks,*/
/*                               sats[sat].local_solutions[j].tasks);*/
/*    if (equals) {*/
/*      sats[sat].local_solutions[j].F = 0; // assuming solutions are sorted*/
/*      dup += 1;*/
/*    }*/
/*  }*/
/*  return dup;*/
/*}*/
////////////////////////////////////////////////////////////////////////////////

int solve(Satellite *sats, int *combination, int *solution) {
	int k, n, max_rep;
	float r, num;
	int *dups;
	int error;

	error = generate_array(nsats, &dups);
	if (error == -1)
		return error;
	
	golden_index_max = 0; //reiniciamos el valor por si no es el de entrada
	get_golden_index_max(sats);
	for (k = 0; k < nsats; k++) {
		if (sats[k].golden_index >= 1) combination[k] = 1;
		else combination[k] = 0;// We start from the combination 1 1 .. 1
	}

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
				// Para la combinación actual introducimos en el conjunto s
				// aquellas combinaciones que son sucesoras directas de la com-
				// binación actual. Sin embargo, como una combinación cualquiera
				// puede tener más de un predecesor, sólo estudiamos un subcon-
				// junto de las sucesoras de cada combinación, de manera que no
				// estudiemos una misma combinación más de una vez. El subcon-
				// junto de las sucesoras que sí estudiamos de una combinación 
				// cualquiera viene dado por todas las sucesoras directas de c
				// tales que si c es de la forma (1,...,1,i,j,...,n) sólo modi-
				// ficamos aquellas posiciones de c que son igual a 1 ADEMÁS de
				// la que es igual a i (i.e., la inmediatamente posterior a la
				// última posición de la ráfaga de 1s que encontramos al co-
				// mienzo de c). Observación: Si c no empieza por uno, sólo
				// modificamos la primera posición.
				if (combination[n] != 1 && !(sats[n].golden_index == 0)) finished = 1;
				if (combination[n] != 0) {
					if (combination[n] >= sats[n].golden_index)
						combination[n] = 0;
					else combination[n]++;
				} else continue; // Si combination[n] es 0, no hay sucesor.
				r = total_reward(sats, combination);
				int m = 0;
				int opt_comb[nsats];
				// La combinación óptima (opt_comb) es aquella sucesora de la 
				// combinación actual (combination) con mayor número de 0s ->
				// representa con total seguridad el menor número de repeticio-
				// nes que puede alcanzar el subconjunto de las sucesoras de la
				// combinación actual, incluida ella misma.
				int fin = 0;
				while (m < nsats) {
					if (fin) opt_comb[m] = combination[m]; 
					else opt_comb[m] = 0;
					if (!fin && combination[m] != 1) fin = 1;
					m++;
				}
				rep = total_occurrences(sats, &opt_comb[0]);
				if (rep <= max_rep && r > max) add(&s, &combination[0], r);

				if (combination[n] == 0) combination[n] = sats[n].golden_index;
				else combination[n]--;
			}
		}
		else {
			first = 0;
			second = 1;
		}
		max_node = get_and_delete_max(&s);
		if (max_node != NULL) { // Es posible que s estuviera vacío: saltamos.
			combination = max_node->id;
			r = max_node->F;
			if (r <= max)
				break;

			n = total_occurrences(sats, combination);
			num = r * (1. - (float)n / (float)(nsats*ntasks));
			// Posible: Incluir una penalización por el número de tareas realizadas en absoluto
			if (num > max) {
				max = num;
				max_rep = n;
				#ifdef DEBUGBASIC
				printf("New max = %f; F = %f; n = %d\n", max, r, n);
				#endif
				copy_solution(combination, solution);
			}
			count++;
			#ifdef DEBUGBASIC
			if (!(count & 0xFFF))
				printf("Count = %d; F = %f; set_size = %d\n", count, r, size(&s));
			#endif
		}
	}
	#ifdef DEBUGBASIC
	printf("Count = %d\n", count);
	#endif
	free(dups);
	return 0;
}

int solve_brute(Satellite *sats, int *combination, int *solution) {
	int k, n;
	long long int combs, i;
	float r, num;
	int *dups;
	int error;

	error = generate_array(nsats, &dups);
	if (error == -1)
		return error;
	golden_index_max = 0; //reiniciamos el valor por si no es el de entrada
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
	float bf_time;
	int error;
/*	int *combination_b, *solution_b;*/
	int *combination, *solution;
	Satellite *sats;
	struct timeval time;
	struct timezone tz;
	suseconds_t start_time_u, final_time_u;
	time_t start_time_s, final_time_s;

	if (argc != 5) {
		fprintf(stderr, "Usage: %s tasks solutions satellites filename\n",
		basename(argcv[0]));
		return 1;
	} else {
		ntasks = atoi(argcv[1]);
		golden_index_max = atoi(argcv[2]);
		nsats = atoi(argcv[3]);
	}
	printf("%d %d %d \n", ntasks, golden_index_max, nsats);
	if (gettimeofday(&time, &tz))
		return 1;
	start_time_u = time.tv_usec;
	srand(start_time_u); // The seed of the random number

// SRM: Uncomment only if using brute solve ////////////////////////////////////
/*  error = generate_array(nsats, &combination_b);*/
/*  if (error == -1)*/
/*    return error;*/
/*  error = generate_array(nsats, &solution_b);*/
/*  if (error == -1)*/
/*    return error;*/
////////////////////////////////////////////////////////////////////////////////

	error = generate_array(nsats, &combination);
	if (error == -1)
		return error;
	error = generate_array(nsats, &solution);
	if (error == -1)
		return error;
	error = init_satellites(&sats);
	if (error == -1)
		return error;
	if (gettimeofday(&time, &tz))
		return 1;

// SRM: Solve in the brute way... //////////////////////////////////////////////
/*  start_time_u = time.tv_usec;*/
/*  start_time_s = time.tv_sec;*/
/*  solve_brute(sats, combination_b, solution_b);*/
/*  if (gettimeofday(&time, &tz))*/
/*    return 1;*/
/*  final_time_u = time.tv_usec;*/
/*  final_time_s = time.tv_sec;*/
/*  bf_time =*/
/*      (final_time_s - start_time_s) * 1000000 + final_time_u - start_time_u;*/
/*  printf("\n Time (brute): %.0f\t| Max (brute): %f\t| ", bf_time, max_brute);*/
/*  print_array("Solution (brute)", solution_b, nsats);*/
/*  free(solution_b);*/
/*  free(combination_b);*/
////////////////////////////////////////////////////////////////////////////////

// SRM: Solve efficiently... //////////////////////////////////////////////////
	start_time_u = time.tv_usec;
	start_time_s = time.tv_sec;
	solve(sats, combination, solution);
	if (gettimeofday(&time, &tz))
		return 1;
	final_time_u = time.tv_usec;
	final_time_s = time.tv_sec;
	bf_time =
	(final_time_s - start_time_s) * 1000000 + final_time_u - start_time_u;
	int tc = task_count(sats, combination);
	FILE *file = fopen(argcv[4], "a" );
	fprintf(file, "%d", tc);
    fclose(file);
	printf(" | Time: %.0f\t| Max: %f\t| ", bf_time, max);
	print_array("Solution", solution, nsats);
	printf("\n");
////////////////////////////////////////////////////////////////////////////////

	free_satellites(sats);
	free(solution);
	free(combination);
	return 0;
}
