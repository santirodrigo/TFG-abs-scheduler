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
	////// END OF FILE
