#ifdef OPT
	#include "ordered_set_opt.h"
#elif TTL
	#include "ordered_set_ttl.h"
#else
	#include "ordered_set.h"
#endif
#include <stdio.h>
#include <stdlib.h>

void main() {
	struct ordered_set s;
	struct node* temp;
	int id_length;
#ifdef OPT
	printf("WELCOME TO THE OPT_ORDERED_SET TESTS!!!\n");
#elif TTL
	printf("WELCOME TO THE TTL_ORDERED_SET TESTS!!!\n");
#else	
	printf("WELCOME TO THE ORDERED_SET TESTS!!!\n");
#endif	
	printf("First of all, set the length of the id:\n\n");
	scanf("%d", &id_length);
	init_set(&s, id_length);
	int opt = -1;
	int id[id_length];
	struct node* work;
	while (opt != 0) {
		switch (opt) {
			case 1:
				printf("\n\t\tEnter the F value for this new element:\n");
				int i, length;
				float F;
				scanf("%f", &F);
				printf("\t\tNow the %d id elements, one per line:\n", id_length);
				for(i = 0; i < id_length; i++) {
					scanf("%d", &id[i]);
				}
				length = size(&s);
				printf("\n\t\t\tTrying with element number %d...\n", length+1);
				add(&s, id, F);
				if (size(&s) == length) printf("\n\t\tNo new element has been introduced\n");
				else {
					printf("\n\t\tA new element (%f) has been introduced with id: ", F);
					for(i = 0; i < id_length; i++) {
						printf(" %d", id[i]);
					}
					printf("\n");
				}
				break;
			case 2:
				if (is_empty(&s)) printf("\nThe set is empty\n");
				else {
					temp = get_max(&s);
					int j;
					if (temp != NULL) {
						printf("\nThe maximum element is %f : ",temp->F);
						for(j = 0; j < id_length; j++) {
							printf("\t%d", temp->id[j]);
						}
						printf("\n");
					} else printf("\nNo element with TTL = 0");
				}
				break;
			case 3:
				if (is_empty(&s)) printf("\nThe set is empty\n");
				else {
					temp = get_min(&s);
					int j;
					printf("\nThe minimum element is %f : ",temp->F);
					for(j = 0; j < id_length; j++) {
						printf("\t%d", temp->id[j]);
					}
					printf("\n");
				}
				break;
			case 4:
				if (is_empty(&s)) printf("\nThe set is empty\n");
				else {
					printf("\nThe set contains:\n");
					int j;
					struct ordered_set t;
					init_set(&t, id_length);
					while (size(&s) > 0) {
#ifdef TTL					
						temp = get_and_delete_max_no_TTL(&s);
#else				
						temp = get_and_delete_max(&s);
#endif
						add(&t, temp->id, temp->F);
						for(j = 0; j < id_length; j++) {
							printf("\t%d", temp->id[j]);
						}
						printf(" : %f\n", temp->F);
					}
					printf("\n\n SIZE(S): %d\tSIZE(T): %d\n", size(&s), size(&t));
					s = t;
					printf("\n\n SIZE(S): %d\tSIZE(T): %d\n", size(&s), size(&t));
				}
				break;
			case 5:
				if (is_empty(&s)) printf("\nThe set is empty\n");
				else {
					printf("\nThe previous maximum node was:\n");
					int j;
					temp = get_and_delete_max(&s);
					if (temp != NULL) {
						for(j = 0; j < id_length; j++) {
							printf("\t%d", temp->id[j]);
						}
						printf(" : %f\n", temp->F);
					} else printf("\nNo element with TTL = 0");
				}
				break;
			case 6:
				if (is_empty(&s)) printf("\nThe set is empty\n");
				else {
					printf("\nThe previous minimum node was:\n");
					int j;
					temp = get_and_delete_min(&s);
					for(j = 0; j < id_length; j++) {
						printf("\t%d", temp->id[j]);
					}
					printf(" : %f\n", temp->F);
				}
				break;
#ifdef TTL
			case 7:
				if (is_empty(&s)) printf("\nThe set is empty\n");
				else {
					printf("\nThe previous maximum (no TTL) node was:\n");
					int j;
#ifdef TTL					
						temp = get_and_delete_max_no_TTL(&s);
#else				
						temp = get_and_delete_max(&s);
#endif
					for(j = 0; j < id_length; j++) {
						printf("\t%d", temp->id[j]);
					}
					printf(" : %f\n", temp->F);
				}
				break;
			case 8:
				if (is_empty(&s)) printf("\nThe set is empty\n");
				else {
					temp = get_med(&s);
					int j;
					printf("\nThe median element is %f : ",temp->F);
					for(j = 0; j < id_length; j++) {
						printf("\t%d", temp->id[j]);
					}
					printf("\n");
				}
				break;
			case 9:
				if (is_empty(&s)) printf("\nThe set is empty\n");
				else {
					temp = get_max_no_TTL(&s);
					int j;
					printf("\nThe real maximum element is %f : ",temp->F);
					for(j = 0; j < id_length; j++) {
						printf("\t%d", temp->id[j]);
					}
					printf("\n");
				}
				break;
#endif
		}
		printf("\n\nTESTING MENU:\n");
		printf("\t1) Add a new element to the set\n\n");
#ifdef TTL
		printf("\t2) Show the maximum element (TTL = 0) in the set\n\n");
#else	
		printf("\t2) Show the maximum element in the set\n\n");
#endif
		printf("\t3) Show the minimum element in the set\n\n");
		printf("\t4) Print in order the whole set\n\n");
#ifdef TTL
		printf("\t5) Delete the maximum element (TTL = 0) in the set\n\n");
#else
		printf("\t5) Delete the maximum element in the set\n\n");
#endif
		printf("\t6) Delete the minimum element in the set\n\n");
#ifdef TTL
		printf("\t7) Delete the maximum element (no TTL)\n\n");
		printf("\t8) Show the median value\n\n");
		printf("\t9) Show the real maximum element\n\n");
#endif
		printf("\t0) Exit\n\n");
		scanf("%d",&opt);
	}
	
	printf("\nEnd of the testing process.\n");
}
