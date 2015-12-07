#include "sched_mat.h"
#include <stdio.h>

void generate_solutions(Satellite *sat) {
	int i, j, t;
	int c, d;
	LocalSolution *temp = malloc(sizeof(LocalSolution));

	char *tasks = malloc(ntasks * sizeof(char));

	char *input_path = malloc(256 * sizeof(char));
	sprintf(input_path,"../../Task_Planner/out/%d/satellite.out",sat->id);

	FILE *input_file = fopen(input_path,"r");

	char *input_tmp = malloc(256 * sizeof(char));
	
	LocalSolution *actsol;
	
	for (j = 0; j < sat->golden_index; j++) {
		actsol = (LocalSolution *)&(sat->local_solutions[j]);
		actsol->id = j+1;
		for(i = 0; i < ntasks; i++) {
			actsol->tasks[i] = 0; // Initialize to not_done (= 0)
		}
		if (fgets(input_tmp, 256, input_file) == NULL) {
			sat->golden_index = j;
			actsol->F = 0;
		} else {
			sscanf(input_tmp,"F:%f:%s",&(actsol->F),tasks);
			d = 0;
			while(sscanf((char *)(tasks+d),"%d:",&t) != EOF) {
				actsol->tasks[t-1] = 1;
				d+=2;
			}
		}
	}
	for (c = 0; c < (sat->golden_index - 1); c++) {
		for (d = 0; d < sat->golden_index - c - 1; d++) {
			if (((sat->local_solutions[d]).F) < ((sat->local_solutions[d+1]).F)) {
				temp->id = sat->local_solutions[d].id;
				temp->F = sat->local_solutions[d].F;
				temp->tasks = sat->local_solutions[d].tasks;

				sat->local_solutions[d].id = sat->local_solutions[d + 1].id;
				sat->local_solutions[d].F = sat->local_solutions[d + 1].F;
				sat->local_solutions[d].tasks = sat->local_solutions[d + 1].tasks;

				sat->local_solutions[d + 1].id = temp->id;
				sat->local_solutions[d + 1].F = temp->F;
				sat->local_solutions[d + 1].tasks = temp->tasks;
			}
		}
	}
	#ifdef DEBUG_DIST_LOC
	printf("\n");
	for (i = 0; i < sat->golden_index; i++) {
		printf("\nF : %f",(sat->local_solutions[i]).F);
		for (j = 0; j < ntasks; j++) {
			printf(" %d ",(sat->local_solutions[i]).tasks[j]);
		}
	}
	printf("\n");
	#endif
}
