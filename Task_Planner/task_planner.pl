/* CubeCAT ****************************************************************************************** 
 *  File:   task_planner.pl                                                                         *
 *  Descr.: An on-line Task Planner based on a Multi-Resource Priority Based Elastic algorithm.     *   
 *  Author: Carles Araguz López.                                                                    *
 *  Date:   2014-feb-20                                                                             *
 *  Vers.:  1.3                                                                                     *
 *  Note:   Comments are structured according to PlDoc documentation system.                        *
 *                                                                                                  *
 *  This file is part of the CubeCAT v1.0 project. "CubeCAT" is an educational project developed at *
 *  the Technical University of Catalonia - BarcelonaTech (Universitat Politècnica de Catalunya).   * 
 ****************************************************************************************************
 *  Changelog:                                                                                      *
 *  - v0.1  Araguz C.   Creation.                                                                   *
 *  - v1.0  Araguz C.   Passed verification tests.                                                  *
 *  - v1.1  Araguz C.   Functional corrections.                                                     *
 *  - v1.2  Araguz C.   Time and memory optimizations.                                              *
 *  - v1.3  Araguz C.   Iterator now allows empty list of tasks as a solution.                      *
 ****************************************************************************************************/

/* Modularization ***********************************************************************************/
:- module(task_planner, [
                        planner/2,
                        order_by_priority/2,
                        iterate/4,
                        schedule/4,
                        task_bs/3,
                        task_running/4,
                        subset2/2,
                        selectn/4,
                        resource_capacity_at/3,
                        seek_time/3
                        ]).

/* Modules to include *******************************************************************************/
:- use_module(library(clpfd)).          /* Constraint Logic Programming over Finite Domains.        */
:- use_module(task_planner_globals).    /* Task Planner configuration.                              */

/** <module> Task Planner
TODO doc
*/

%%  planner(+Tasks, +Params, -Outcome)
%   
%   Calls schedule/2 following the priority order defined by iterate/3 and generates the Outcome as 
%   a set of schedulable Tasks. See schedule/2 to see Tasks and Params structure.

planner(Tasks,Outcome) :- % planner/2
    order_by_priority(Tasks,Ts),
    % SRM: For aviding the iteration part, we should do directly: scheduler_param(algorithm_timeout, Timeout),
    															 % (Timeout > 0
    															 % ->  catch(call_with_time_limit(Timeout,schedule(Ts,Outcome)),_,Outcome=[])
    															 % ;   schedule(Ts,Outcome)),!.

/* POSIBILIDADES PARA LA BÚSQUEDA DE MÚLTIPLES SOLUCIONES:
   =============================================================

	Llamar múltiples veces a schedule(Tasklist, Outcome) de manera que obtengamos N >> Delta soluciones parciales y de ahí seleccionemos sólo las Delta soluciones mejores. Puede hacerse directamente en el Prolog o desde fuera (obviamente es mejor la primera solución).
	
	Esto se debería implementar de manera que ~ las N veces que llamemos a schedule:
		1) Exista solución.
		2) La mejora sea incremental o similar (backtracking)
	
	1) se puede garantizar parcialmente probando primero de hacer schedule con una función muy silimar a iterate: cortar de la combinatoria de tareas aquellas combinaciones que contengan un subconjunto de tareas insoluble por sí solo.
	2) se debe estudiar la función F de calificación de las soluciones, para ver cómo se comporta a medida que quitamos o añadimos soluciones de tareas
	
	Como esto es a priori un problema en sí mismo (hacer un "recorrido" en F para probar de manera óptima las N soluciones y obtener las supuestas Delta mejores), se puede comenzar haciendo scheduler con el Iterator original y modificando prioridades a cada vez...
	
******************************************************************/
    iterate(Ts,Outcome,TaskComb,FList),display_list_task_lists(TaskComb,FList),!.


%% order_by_priority(+Tasks:list, -Sorted:list)
%
%   Sorted is the list of task objects Tasks sorted by priority using the Quicksort algorithm. 

order_by_priority(List,Sorted) :-
    quick_sort(List,[],Sorted),!.
    
quick_sort([],Acc,Acc).
quick_sort([H|T],Acc,Sorted) :-
    pivot(H,T,L1,L2),
    quick_sort(L1,Acc,Sorted1),
    quick_sort(L2,[H|Sorted1],Sorted).
    
pivot(_,[],[],[]).
pivot(H,[X|T],[X|L],G) :-
    X = task(_,_,_,_,PrioX,_),
    H = task(_,_,_,_,PrioH,_),
    PrioX =< PrioH,
    pivot(H,T,L,G).
pivot(H,[X|T],L,[X|G]) :-
    X = task(_,_,_,_,PrioX,_),
    H = task(_,_,_,_,PrioH,_),
    PrioX > PrioH,
    pivot(H,T,L,G).
    
% SRM : ------------------------------------------------------------------------
%
% Multiply all elements in a list (L) by a constant value (C) to get result (R)

multiply_list([],C,[]).
multiply_list([ElemL|L],C,[ElemR|R]) :-
	ElemR is ElemL * C,
	multiply_list(L,C,R).
	
% Display a list of task lists TL

display_list_task_lists([],[]).
display_list_task_lists([Elem|TL],[F|FList]) :-
	display_task_list(Elem),write('F: '),writeln(F),
	display_list_task_lists(TL,FList).

display_list([]) :-
	writeln('.').
display_list([Elem|List]) :-
	write(Elem),write(';'),
	display_list(List).
% SRM --------------------------------------------------------------------------

%%  iterate(+Tasks, +Params, -Outcome) is semidet
%
%   Tasks is a list of tasks. Params is a list of all the available resources (elastically defined) 
%   and parameters needed in the predicate schedule/2. Outcome is the list of tasks that are 
%   scheduled. All tasks in Outcome, have succeded schedule/2 and their priority order is kept. 
%   Complexity _|O(x)|_ of this predicate is  _|O(|Tasks|) <= O(x) <= O(2*|Tasks|-1)|_

/*iterate(Tasks,Params,Scheduleds) :-
    include(schedule(Params),Tasks,[T|Ts]),!, % Finds a list of "schedulable alone" tasks
    iterate_(Ts,Params,[T],Scheduleds). % Iterates starting by T.
    
iterate_([],_,Ss,Ss).
iterate_([T|Ts],P,Ss,Os) :-
    append(Ss,[T],Ss1),!,
    schedule(P,Ss1),!,
    iterate_(Ts,P,Ss1,Os).
iterate_([T|Ts],P,Ss,Os) :-
    append(Ss,[T],Ss1),
    write('Can\'t schedule '),write(Ss1),nl,
    iterate_(Ts,P,Ss,Os).
   */ 
    
iterate(Tasks,Scheduleds,TaskComb,FList) :-
    write('Starting the iterator:'),nl,
    (iterate0(Tasks,[],[T|Ts],[],[R|_],[],F0List)     % Finds a list of "schedulable alone" tasks ([T|Ts]) and
    ->  length([T|Ts],NumPosTasks),
    	PondValue is (1 / NumPosTasks),
    	multiply_list(F0List,PondValue,FList0)                         %   their solutions [R|Rs], but we don't care about Rs. % SRM: should we care about it?
    ;   Scheduleds = [],
    	NumPosTasks is 0),                   % If there is no task schedulable alone, there is no solution.
    iterate_(Ts,[T],[],Solutions,NumPosTasks,FList0,FList,[T|Ts],TaskComb),          % Iterates starting by task T. 
    (Solutions = [] 
    ->  Scheduleds = R                      % If the iterator can't find any valid step, the % SRM: En la llamada a iterate0([]... R1 = R0
    ;   Scheduleds = Solutions).            %   solution is the previously found R. 

iterate0([]    ,FS1,FS1,R1,R1,FList,FList).				% SRM: Copy FS0 into FS1; R0 into R1
iterate0([T|Ts],FS0,FS1,R0,R1,FList0,FList) :-
    scheduler_param(algorithm_timeout, Timeout),
    (Timeout > 0
    ->  catch(call_with_time_limit(Timeout,schedule([T],R,0,F)),_,R=[]) % SRM: We take care of the algorithm timeout
    ;   schedule([T],R,0,F)),	% SRM: Schedule of the task list's head
    write('F value for this schedule would be: '),writeln(F),
    (R = [] 
    ->  iterate0(Ts,FS0,FS1,R0,R1,FList0,FList) % SRM: If there is no solution, call recursively without adding T to the list of solved tasks and solutions
    ;   iterate0(Ts,[T|FS0],FS1,[R|R0],R1,[F|FList0],FList)). % SRM: Else, call recursively adding T and its solution
    
iterate_([]    ,_   ,Os,Os,NumPosT,FList,FList,TaskComb,TaskComb).
iterate_([T|Ts],Prev,Ss,Os,NumPosT,FList0,FList,TaskComb0,TaskComb) :-
    scheduler_param(algorithm_timeout, Timeout),
    append(Prev,[T],Step),
    !,
    (Timeout > 0
    ->  catch(call_with_time_limit(Timeout,schedule(Step,Ss1,NumPosT,F)),_,Ss1=[])
    ;   schedule(Step,Ss1,NumPosT,F)),
    !,
    write('F value for this schedule would be: '),writeln(F),
    (Ss1 = []
    ->  iterate_(Ts,Prev,Ss ,Os,NumPosT,FList0,FList,TaskComb0,TaskComb)
    ;   iterate_(Ts,Step,Ss1,Os,NumPosT,[F|FList0],FList,[Step|TaskComb0],TaskComb)).

%-------------------------------------------------------------------------------
% SRM: Preview version of calculate_F function for each solution:

calculate_F(Ts,Rnames,T1,T1,F,F0) :-
    calc_consumptions_at(Ts,Rnames,T1,CapCons),
    ponderate_and_sum(CapCons,F,F0).
calculate_F(Ts,Rnames,T0,T1,F,F0) :-
    T2 is T0+1,
    calculate_F(Ts,Rnames,T2,T1,F1,F0),
    calc_consumptions_at(Ts,Rnames,T0,CapCons),
    ponderate_and_sum(CapCons,F,F1).
    
ponderate_and_sum([Cap|[Cons|Cs]],F,F0) :-
	ponderate_and_sum(Cs,F1,F0),
	F is F1+(1-(Cons/Cap)).
ponderate_and_sum([],F0,F0) :-!.

%-------------------------------------------------------------------------------


schedule([],[],_,_).
schedule(Original,Tasks,NumPosT,F) :-
    % Copies to preserve original variables uninstantiated:
    copy_term(Original,Tasks,Gs),
    maplist(call,Gs),
    
    length(Tasks,TaskNum),
    write('Starting scheduler algorithm for '),write(TaskNum),writeln(' tasks'),
    display_task_list(Tasks),nl,
    
    /* The global start time must be less than the global end time. */
    ((scheduler_param(time_start,T0), scheduler_param(time_end,T1)) -> T1 > T0),
    must_be(integer,T0), 
    must_be(integer,T1),
    
    writeln('Finding subtasks.'),
    time(find_subtasks(Tasks,[],Subtasks)), % SRM:  Subtasks are simpler objects that consume a single resource with a constant value.
    
    writeln('Extracting domain bounds.'),
    time((  maplist(arg(1), Tasks, Starts),
            maplist(arg(2), Tasks, Durations),
            maplist(arg(3), Tasks, Ends),
            maplist(#=<(T0), Starts), % Cut S domains if they are out of bounds
            maplist( #>(T1), Ends),   % Cut E domains if they are out of bounds
            maplist(fd_inf, Starts, MinStarts),
            maplist(fd_sup, Ends, MaxEnds),
            min_list(MinStarts, Start),
            max_list(MaxEnds, End))), % Note that d(capacity)/dt is *never* supposed to be negative
    
    writeln('Calculating Bs.'),
    time(maplist(task_bs(End), Subtasks, Bss)),
    
    writeln('Constraining consumptions.'),
    time(constrain_consumptions(Start, End, Subtasks, Bss)),
    
    writeln('Grouping by overlap probability.'),
    time(overlapping_groups(Starts,Durations,[],Groups)), % Starts in each group have a high probability of overlapping between them whereas overlapping between groups is less likely to happen.
    length(Groups,GL),
    write('  Created '),write(GL),writeln(' groups.'),nl,
    
    writeln('Setting starts.'),
    time(set_starts(Groups)),
    (define(debug,yes)
    ->  findall(R,resource_options(R,_),Rnames),
        flag(dbg_time,Now,Now),
        % SRM: F calculation ---------------------------------------------------
        calculate_F(Subtasks,Rnames,T0,T1,Ftot,0),
        length(Rnames,Rnum),
        Ttot is T1 - T0 + 1,
        F0 is Ftot / (Rnum * Ttot),
        (NumPosT = 0 ->
        	F is F0
        ;	F is F0 * (TaskNum / NumPosT)),
        %write('F value for this schedule would be: '),writeln(F),
        % SRM: -----------------------------------------------------------------
        define(dbg_dir, Dbgdir),
        format_time(string(StrTime),"%Y%m%d_%H%M%S/",Now),
        atomic_list_concat([Dbgdir,StrTime,'stat_dynamic_resources.csv'],DynPath),
        open(DynPath,write,Stream2,[]),
        current_output(CO),
        set_output(Stream2),
        write_csv_dynamic(Subtasks,Rnames,T0,T1), % Dynamic
        close(Stream2),
        set_output(CO)
    ;   true).
% If the first predicate did not succeed then there is no solution, but schedule/2 always succeeds:
schedule(_,[],_,_).

/** DEBUG PURPOSES ** TODO : REMOVE WHEN DONE ******************************************************/
write_subtasks([]).
write_subtasks([K|Ks]) :-
    writeln(K),
    write_subtasks(Ks).

write_csv_dynamic(Ts,Rnames,T0,T1) :-
    write('time'),
    write_csv_header(Rnames),nl,
    dynamic_data(Ts,Rnames,T0,T1).

write_csv_header([R|Rs]) :-
    write(','),
    write(R),write('_cap,'),
    write(R),write('_con'),
    write_csv_header(Rs).
write_csv_header([]).

dynamic_data(Ts,Rnames,T1,T1) :-
    write(T1), write(','),
    calc_consumptions_at(Ts,Rnames,T1,CapCons),
    write_cap_cons(CapCons),nl.
dynamic_data(Ts,Rnames,T0,T1) :-
    write(T0), write(','),
    calc_consumptions_at(Ts,Rnames,T0,CapCons),
    write_cap_cons(CapCons),nl,
    T2 is T0+1,
    dynamic_data(Ts,Rnames,T2,T1).
    

calc_consumptions_at(Ts,[Rname|Rnames],T,[Cap|[Con|Cs]]) :-
    resource_capacity_at(Rname,T,Cap),
    total_consumption_at(Rname,Ts,T,0,Con),
    calc_consumptions_at(Ts,Rnames,T,Cs).
calc_consumptions_at(_,[],_,[]).

total_consumption_at(Rname,[Task|Ts],T,Acc,Con) :-
    Task = subtask(S,_,E,C,_,_),
    ((T >= S, T < E, C =.. [Rname|[V]])
    ->  Acc1 is Acc + V,
        total_consumption_at(Rname,Ts,T,Acc1,Con)  
    ;   total_consumption_at(Rname,Ts,T,Acc, Con)).

total_consumption_at(_,[],_,Con,Con).
    
write_cap_cons([C|Cs]) :- write(C),write(','),write_cap_cons(Cs).
write_cap_cons([]).
/***************************************************************************************************/


find_subtasks([],     Ks, Ks).
find_subtasks([T|Ts], S , Ks) :-
    split_byresource(T,S0s),
    split_tasks(S0s,[],S1s),
    append(S,S1s,S2s),
    find_subtasks(Ts,S2s,Ks).


split_byresource(T,[]) :- T = task(_,_,_,[],_,_).
split_byresource(T,[A|As]) :-
    T = task(S,D,E,[C|Cs],Prio,Id),
    A = task(S,D,E,C,Prio,Id),
    Rest = task(S,D,E,Cs,Prio,Id),
    split_byresource(Rest,As).
    
split_tasks([]    , Ks , Ks).
split_tasks([T|Ts], K0s, Ks) :-
    catch(split_task(T,K1s),scheduler_error(E),write_error(E)),
    append(K0s,K1s,K2s),
    split_tasks(Ts,K2s,Ks).

split_task(T,[K|Ks]) :-
    T = task(S,_,E,C,Prio,Id),
    C =.. [Name|[Pairs]], % name([ value1-time1 , value2-time2, ... ])
    (resource_capacity(Name,_) % Checks whether the resource capacity has been defined
    ->  create_subtasks(S,E,Name,Pairs,0,0,Prio,Id,[K|Ks]),
        K = subtask(Sk,_,_,_,_,_),
        Sk #= S
    ;   throw(scheduler_error(undefined_resource(Name)))).

create_subtasks(Ek,E,Name,[],_,LastC,Prio,_,K) :-
    Ek #= E,

    (resource_options(Name,Opts)
    ->  O =.. [cumulative|[Cumulative]],
        memberchk(O,Opts)
    ;   Cumulative = false),
    
    (Cumulative = true 
    ->  scheduler_param(time_end,TE),
        Dk #= TE-Ek,
        Ck =.. [Name,LastC],
        K = [subtask(Ek,Dk,TE,Ck,Prio,0)]
    ;   K = []
    ).
create_subtasks(Sk,E,Name,[V|Vs],Time0,_,Prio,Id,[K|Ks]) :-
    V = Consumption-Time1,
    Dk #= Time1-Time0,
    Ek #= Sk+Dk,
    Ck =.. [Name,Consumption], % name(consumption)
    K = subtask(Sk,Dk,Ek,Ck,Prio,Id),
    create_subtasks(Ek,E,Name,Vs,Time1,Consumption,Prio,Id,Ks).
    
    

%%  task_bs(+Task, ?Bs)
%
%   Constrain Task variables and generate list of reified variables Bs. After calling this 
%   predicates, task's time variables (i.e. _Start_ and _End_) are constrained to be:
%   
%   ==
%   End #= Start + Duration.
%   == 
%   @see task_running/4.
%   @author Markus Triska (as part of the SWI-Prolog's Constrain Logic Programming over Finite 
%       Domains library)

task_bs(Ub,Subtask,EST-Bs) :-
    Subtask = subtask(S,D,E,_,_,Id),
    D #> 0,         % Adds a constraint.    Consider removing this line.
    E #= S+D,       % Adds a constraint.    Consider removing this line.
    maplist(clpfd:finite_domain,[E,S,D]), % Consider removing this line.
    fd_inf(S,EST),  % Earliest Start Time
    fd_sup(E,LET),  % Latest End Time
    
    (Id = 0
    ->  % Virtual tail for tasks consuming cumulative resource.
        L is Ub-EST
    ;   % Subtask:
        L is LET-EST
    ),
    length(Bs,L),
    task_running(Bs,S,E,EST). % Bs is a list of reified variables.
    

%%  task_running(?Bs, +Start, +End, +EST)
%   
%   Generates a list of reified variables Bs from Start to End. EST specifies the time offset and 
%   stands for _|Earliest Start Time|_.
%
%   @author Markus Triska (as part of the SWI-Prolog's _|CLP(FD)|_ library)

task_running([],_,_,_).
task_running([B|Bs],S,E,T) :-
    ((T #>= S) #/\ (T #< E)) #<==> B, % B will be in 0..1 until S and E are bound to integers.
    T1 is T + 1,
    task_running(Bs,S,E,T1).


constrain_consumptions(T0,T,Subtasks,Bss) :-
    findall(Rname,resource_capacity(Rname,_),Rnames),
    constrain_consumptions_(T0,T,Subtasks,Bss,Rnames),!.
constrain_consumptions_(_ , _, _ , _  , []) :- !.
constrain_consumptions_(T0, T, Ks, Bss, [Rn|Rns]) :-
    resource_limit(T0,T,Ks,Bss,Rn),
    constrain_consumptions_(T0,T,Ks,Bss,Rns).

resource_limit(T,T,_,_,_) :- !.
resource_limit(T0,T,Subtasks,Bss,Rn) :-
    maplist(contribution_at(T0,Rn),Subtasks,Bss,Cs),
    resource_capacity_at(Rn,T0,Cap),
    sum(Cs, #=<, Cap),
    T1 is T0 + 1,
    resource_limit(T1,T,Subtasks,Bss,Rn).


resource_capacity_at(Rn,T,Cap) :-
    resource_capacity(Rn,Caps),
    seek_time(T,Caps,Cap).


/* TODO - Optimize search - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
seek_time(T,[V-T|_],V) :- !.
seek_time(T,[_,  V-T|_],V) :- !.
seek_time(T,[_,  _,  V-T|_],V) :- !.
seek_time(T,[_,  _,  _,  V-T|_],V) :- !.
seek_time(T,[_,  _,  _,  _,  V-T|_],V) :- !.
seek_time(T,[_,  _,  _,  _,  _,  V-T|_],V) :- !.
seek_time(T,[_,  _,  _,  _,  _,  _,  V-T|_],V) :- !.
seek_time(T,[_,  _,  _,  _,  _,  _,  _,  V-T|_],V) :- !.
seek_time(T,[_,  _,  _,  _,  _,  _,  _,  _,  V-T|_],V) :- !.
seek_time(T,[_,  _,  _,  _,  _,  _,  _,  _,  _,  V-T|_],V) :- !.
seek_time(T,[_,  _,  _,  _,  _,  _,  _,  _,  _,  _,  V-T|_],V) :- !.
seek_time(T,[_,  _,  _,  _,  _,  _,  _,  _,  _,  _,  _,  V-T|_],V) :- !.
seek_time(T,[_,  _,  _,  _,  _,  _,  _,  _,  _,  _,  _,  _,  V-T|_],V) :- !.
seek_time(T,[_,  _,  _,  _,  _,  _,  _,  _,  _,  _,  _,  _,    _|P],V) :- seek_time(T,P,V). */

seek_time(S,[V-T|Ps],X) :- seek_time_(S,[V-T|Ps],T,X).
seek_time_(S,[V-_|_],PrevT,V) :-
    S =< PrevT,!.
seek_time_(S,[V-T|_],PrevT,V) :-
    S > PrevT,
    S =< T,!.
seek_time_(S,[_-T|Ps],PrevT,X) :-
    S > PrevT,
    S > T,
    seek_time_(S,Ps,T,X).


%%  contribution_at(+Time, +Resource, +Task, +Bs, ?Contribution)
%
%   Contribution is the Task consumption of Resource at the given Time. If the Task time parameters 
%   are not set (i.e. _Start_ and _End_ times are not bound to integers but to clpfd domains) 
%   Contribution is bound to a domain as well. Contribution domain is an integer if the Task 
%   consumption is constant.
%
%   @author Carles Araguz López (based on clpfd:contribution_at/4 written by Markus Triska as part
%       of the SWI-Prolog's _|CLP(FD)|_ library)

contribution_at(T,Resource,K,Offset-Bs,Contribution) :-
    K = subtask(S,_,E,Con,_,_),
    Con =.. [Rn|[C]], % name(value)
    (Rn = Resource
    ->
        fd_inf(S,InfS),
        fd_sup(E,SupE),
        (   T < InfS  -> Contribution #= 0
        ;   T >= SupE -> Contribution #= 0
        ;   N is T-Offset,
            nth0(N,Bs,B),
            Contribution #= B*C
        )
    ;   % This subtask does not consume `Resource'.
        Contribution #= 0
    ).


overlapping_groups([]    ,[]    ,Gs,Gs) :- !.
overlapping_groups([S|Ss],[D|Ds],Gs,Gs3) :-
    scheduler_param(minimize_overlapping,true),
    scheduler_param(mo_allow_permutation,MOAP),
    scheduler_param(mo_max_group_members,MOMGM),
    nth0(I,Gs,G,Rest),
    (MOAP = true -> length(G,L), L < MOMGM ; true ),
    similar_domains(S,G),
    more_duration_fits(G),
    nth0(I,Gs2,[S-D|G],Rest),
    !,
    overlapping_groups(Ss,Ds,Gs2,Gs3).
overlapping_groups([S|Ss],[D|Ds],Gs,Gs2) :-
    % S does not match to any of Gs groups
    overlapping_groups(Ss,Ds,[[S-D]|Gs],Gs2).
    
similar_domains(_,[]).
similar_domains(A,[B-_|Bs]) :-
    scheduler_param(domain_similarity,Ratio),
    fd_dom(A,DomA),
    fd_dom(B,DomB),
    C in DomA,
    C in DomB,
    fd_size(A,SizeA),
    fd_size(B,SizeB),
    fd_size(C,SizeC),
    SizeC >= Ratio*SizeA,
    SizeC >= Ratio*SizeB,
    similar_domains(A,Bs).

more_duration_fits(Bs) :-
    extract_starts(Bs,Ss),
    maplist(fd_inf,Ss,Infs),
    maplist(fd_sup,Ss,Sups),
    min_list(Infs,Lb),
    min_list(Sups,Ub),
    extract_durations(Bs,Ds),
    sum_list(Ds,TotalD),
    W is Ub-Lb,
    TotalD =< W.

set_starts(Groups) :-
    scheduler_param(minimize_overlapping,true),
    (   scheduler_param(mo_once,true)
    ->  writeln('  Minimizing overlaps (once).'),
        once(minimize_overlapping(Groups))
        
    ;   writeln('  Minimizing overlaps.'),
        minimize_overlapping(Groups)
    ).
% Fallback:
set_starts(Groups) :-
    writeln('  Labeling in fallback mode (no optimization is performed).'),
    extract_starts(Groups,Starts),!,
    scheduler_param(labeling_options,Opts),
    labeling(Opts,Starts).


extract_starts([]      ,[]) :- !.
extract_starts([S-_|Ps],[S|Ss]) :-
    extract_starts(Ps,Ss).
extract_starts([G|Gs],Starts) :-
    is_list(G),
    extract_starts([G|Gs],[],Starts).
extract_starts([],    Starts,Starts).
extract_starts([G|Gs],Starts,Starts2) :-
    extract_starts(G,Ss),
    append(Starts,Ss,Ss2),
    extract_starts(Gs,Ss2,Starts2).

extract_durations([]      ,[]).
extract_durations([_-D|Ps],[D|Ds]) :-
    extract_durations(Ps,Ds).
extract_durations([G|Gs],Durations) :-
    is_list(G),
    extract_durations([G|Gs],[],Durations).
extract_durations([],    Durations,Durations).
extract_durations([G|Gs],Durations,Durations2) :-
    extract_durations(G,Ds),
    append(Durations,Ds,Ds2),
    extract_durations(Gs,Ds2,Durations2).


minimize_overlapping([]).
minimize_overlapping([G|Gs]) :-
    scheduler_param(mo_allow_permutation,true),
    permutation(G,G2),
    %write('.'),ttyflush,
    link_times(G2),
    extract_starts(G2,Starts),
    labeling([ffc,up,step],Starts),
    minimize_overlapping(Gs),
    length(G,L),
    write('    Labeling group of size '),write(L),writeln(' (permutations allowed) ... done').

minimize_overlapping([G|Gs]) :-
    scheduler_param(mo_allow_permutation,false),
    link_times(G),
    extract_starts(G,Starts),!,
    labeling([ffc,up,step],Starts),
    minimize_overlapping(Gs),
    length(G,L),
    write('    Labeling group of size '),write(L),writeln(' (permutations allowed) ... done').

link_times([S-D|Ps]) :-
    link_times_(Ps,S-D).
link_times_([],_) :- !.
link_times_([S-D|Ps],PrevS-PrevD) :-
    S #= PrevS+PrevD,
    link_times_(Ps,S-D).


%%  subset2(+Set, -Subset)
%
%   Subset is a subset of the Set. The predicate will succeed until there is no more possible 
%   subsets. The number of elements in Subset increases from 1 to N, where N is the number of 
%   elements of the original list Set. The choice points will not include duplicates and will 
%   include the original list as the last choice point. Membership test is based on selectn/4.
  
subset2(Ts,Gs) :-
    length(Ts,L),
    N is L-1,
    subset2(Ts,N,Gs).
subset2(Ts,0,Ts).
subset2(Ts,N,Gs) :-
    N > 0,
    selectn(N,_,Ts,Gs).
subset2(Ts,N,Gs) :-
    N0 is N-1,
    N0 >= 0,
    subset2(Ts,N0,Gs).

%%  selectn(+N, ?Elements, ?List1, ?List2)
%
%   Is true when List1 with all Elements, results in List2. List order is maintained and permutations
%   are not considered as solutions.   

selectn(0,[],L1s,L1s).
selectn(N,[E|Es],L1s,L2s) :-
    append(H,[E|T],L1s),
    M is N-1,
    selectn(M,Es,T,S),
    append(H,S,L2s).


/* Debug predicates *********************************************************************************/

%%  mschedule(+Tasks, +Params, -Outcome)
%
%   Mock predicate for schedule/2. Does only contain the type checks. For debug purposes only.

mschedule(Tasks) :-
    write('Mock schedule/2 (1) called for tasks:'),nl,
    display_task_list(Tasks).
    %catch(must_be(list(list), [Tasks]), Error,(write(Error),nl,fail)),!, 
    %catch(must_be(list(list), [Params]),Error,(write(Error),nl,fail)),!,  
    %memberchk(time_horizon(_),Params), /* The time_horizon must be bound to an Integer. */
    %memberchk(resources(Rs),Params),    /* Resources must be bound and a list. */
    %must_be(list,Rs).
% Needed to support the iterator.
mschedule(Task) :-
    write('Mock schedule/2 (2) called for tasks:'),nl,
    display_task_list([Task]),
    must_be(compound,Task),
    mschedule([Task]).
    
display_task_list([]) :- !.
display_task_list([T|Ts]) :-
   %write(T),nl,
   T = task(_,_,_,_,Prio,Id),
   write(' - ('),write(Prio),write(') '),write(Id),nl,
   display_task_list(Ts).
%SRM : --------------------------------------------Added for convenience--------
display_task_list(T) :- % As it is the last occurrence of display_task_list, it is only used for one task lists
   T = task(_,_,_,_,Prio,Id),
   write(' - ('),write(Prio),write(') '),write(Id),nl.
%SRM----------------------------------------------------------------------------
    
display_task_ids([]) :- !.
display_task_ids([T|Ts]) :-
   T = task(_,_,_,_,_,Id),
   write(Id),write(','),
   display_task_ids(Ts).
/*

test_mo :-
    Ss = [A,B,C,D,E,F,G],
    [A,B,C] ins 0..100,
    [D,E,F] ins 50..150,
    [G] ins 100..200,
    display_domain(A,0,200),nl,
    display_domain(B,0,200),nl,
    display_domain(C,0,200),nl,
    display_domain(D,0,200),nl,
    display_domain(E,0,200),nl,
    display_domain(F,0,200),nl,
    display_domain(G,0,200),nl,
    overlapping_groups(Ss,[],Gs),
    writeln(Gs).

overlapping_groups(Durations,Starts,Groups) :-
    maplist(domain_slots,Starts,Slots),
    overlapping_groups_(Durations,Starts,Slots,[],MOGs),
    extract_mog(MOGs,Groups).    

overlapping_groups_([]    ,[]    ,[]      ,Fs,Fs).
overlapping_groups_([D|Ds],[S|Ss],[Xs|Xss],Gs,Fs) :-
    match_group(D,S,Xs,Gs,Gs2),
    overlapping_groups_(Ds,Ss,Xss,Gs2,Fs).


match_group(D,S,Xs,[],[mog([D,S,Xs])]). % There is no group to match with
match_group(D,S,Xs,Gs,Gs2) :-
    % Include it in an existing group
    nth0(I,Gs,mog(MOGs),Rest),
    mog_elems(MOGs,Ds,Ss,Xss),
    similar_domains_all(S,Ss),
    mo_optimizable(D,Xs,Ds,Xss),
    nth0(I,Gs2,mog([[D,S,Xs]|MOGs]),Rest).
match_group(D,S,Xs,Gs,[mog([D,S,Xs])|Gs]). % It doesn't match any group

mog_elems([]        ,[]    ,[]    ,[]).
mog_elems([MOG|MOGs],[D|Ds],[S|Ss],[Xs|Xss]) :-
    MOG = [D,S,Xs],
    mog_elems(MOGs,Ds,Ss,Xss).

mo_optimizable(_ ,_ ,[]      ,[]).
mo_optimizable(D1,Xs,[D2|D2s],[Ys|Yss]) :-
    mo_optimizable(D1,Xs,D2,Ys),
    mo_optimizable(D1,Xs,D2s,Yss).
mo_optimizable(D1,Xs,D2,Ys) :-
    maplist(integer,[D1,D2]),
    mo_optimizable_(D1,Xs,D2,Ys,0,Acc),
    length(Xs,Xslots),
    length(Ys,Yslots),
    Acc >= 0.66 * Xslots * Yslots.

mo_optimizable_(_ ,[]    ,_ ,_ ,Acc,Acc).
mo_optimizable_(D1,[X|Xs],D2,Ys,Acc,Bcc) :-
    include(mo_optimizable_(D1,X,D2),Ys,Yopts),
    length(Yopts,LYopts),
    Acc1 is Acc + LYopts,
    mo_optimizable_(D1,Xs,D2,Ys,Acc1,Bcc).


mo_optimizable_(D1,X,D2,Y) :-
    maplist(clpfd:finite_domain,[X,Y]),
    maplist(domain_continuous,[X,Y]),
    mo_eligible([D1,D2],[X,Y]),
    similar_domains_intersect(X,Y,0.75).

    

test_mo :-
    A mod 10 #= 0,
    A #>= 0,
    A #< 200,
    B in 0..100,
    Da #= 5,
    Db #= 5,
    display_domain(A,0,200), writeln(Da),
    display_domain(B,0,200), writeln(Db),
    domain_slots(A,As),
    length(As,N),
    mo_optimizable_(Da,As,Db,[B],0,Acc),
    (Acc >= 0.66*N -> write('YES : '),writeln(Acc) ; write('NO : '),writeln(Acc)).
    

domain_slots(X,[Z]) :-
    domain_continuous(X),
    fd_dom(X,Dom),
    Z in Dom.
domain_slots(X,[S|Ss]) :-
    domain_slot(X,D,Rest),
    Y in Rest,
    S in D,
    domain_slots(Y,Ss).
domain_slot(X,Y,Rest) :-
    fd_dom(X,Rest\/Y).
    

domain_continuous(X) :-
    clpfd:finite_domain(X),
    \+domain_slot(X,_,_).

similar_domains_all(_,[]    ,_).
similar_domains_all(A,[B|Bs],Ratio) :-
    similar_domains(A,B,Ratio),
    similar_domains_all(A,Bs,Ratio).
    
similar_domains(A,B,Ratio) :-
    fd_dom(A,DomA),
    fd_dom(B,DomB),
    C in DomA,
    C in DomB,
    fd_size(A,SizeA),
    fd_size(B,SizeB),
    fd_size(C,SizeC),
    SizeC >= SizeA*Ratio,
    SizeC >= SizeB*Ratio. 
similar_domains_intersect(A,B,Ratio) :-
    fd_dom(A,DomA),
    fd_dom(B,DomB),
    C in DomA,
    C in DomB,
    fd_size(A,SizeA),
    fd_size(B,SizeB),
    fd_size(C,SizeC),
    min_list([SizeA,SizeB],MinSize),
    SizeC >= MinSize*Ratio.

similar_durations(D1,D2,Ratio) :-
    must_be(list(integer),[D1,D2]),
    (D1 >= D2 -> Ratio =< D2/D1 ; Ratio > D1/D2).

mo_eligible(Ds,Xs) :-
    maplist(fd_size,Xs,Sizes),
    min_list(Sizes,Size),
    sum(Ds,#=<,Size).
*/


/*
% Execution Aborted
?- test(5,360,10),!.
Finding subtasks.
% 27,980 inferences, 0.011 CPU in 0.010 seconds (100% CPU, 2662328 Lips)
Extracting domain bounds.
% 104 inferences, 0.000 CPU in 0.000 seconds (99% CPU, 1783356 Lips)
Calculating Bs.
% 20,001,731 inferences, 2.028 CPU in 2.026 seconds (100% CPU, 9863465 Lips)
Constraining consumptions.
% 164,835,876 inferences, 16.952 CPU in 16.937 seconds (100% CPU, 9723854 Lips)
Labeling.
% 111,233,935,920 inferences, 10877.565 CPU in 11818.714 seconds (92% CPU, 10225996 Lips)
% 2 inferences, 0.002 CPU in 0.002 seconds (100% CPU, 910 Lips)
% 2 inferences, 0.000 CPU in 0.000 seconds (99% CPU, 177336 Lips)
false.

*/


/*

intersection([],_).
intersection([S-_|Ps],I) :-
    fd_dom(S,DomS),
    I in DomS,
    intersection(Ps,I).

sum_durations(G,Sum) :-
    sum_durations_(G,0,Sum).
sum_durations_([]      ,Acc,Acc).
sum_durations_([_-D|Ps],Acc,Sum) :-
    Acc1 is Acc + D,
    sum_durations_(Ps,Acc1,Sum).
    
*/

/**
    fd_inf(S,Lb),
    fd_sup(S,Ub),
    random_between(Lb,Ub,A),
    S #= A,
    set_starts(Ss).
*/
test_random(N) :-
    length([L1,L2|Ls],N),
    L1 in 1..N,
    set_domain([L2|Ls],L1,N),
    set_starts([L1,L2|Ls]),
    writeln([L1,L2|Ls]).

set_domain([],_,_).
set_domain([X|Xs],Y,Z) :-
    X in 1..Z,
    X #> Y,
    set_domain(Xs,X,Z).



write_error(Err) :-
    Err =.. [Type|[Info]],
    (   Type = undefined_resource
    ->  write('ERROR: Resource '),write(Info),write(' has not been defined.'),nl
    
    ;   Type = undefined_error
    ->  write('ERROR: Undefined error.'),nl
    ).



test(NTasks,Time,Duration) :-
    [S,E] ins 0..Time,
    D #= Duration,
    C = [
            power([10-Duration]), 
            %energy([10-1,20-2,30-3,40-4,50-5,60-6,70-7,80-8,90-9,100-Duration]),  
            storage([1-Duration]),
            simultaneity([1-Duration])
        ],
    generate_tasks(NTasks,S,D,E,C,Ts),!,
    
    %cumulative([T1,T2,T3],[limit(1)]),
    %labeling([ff],[S1,S2,S3,E1,E2,E3]),
    
    schedule(Ts),
    
    maplist(display_task_info,Ts),
    maplist(arg(1),Ts,Starts),
    maplist(arg(3),Ts,Ends),
    maplist(display_domain(S,0,200),Starts,Ends).

generate_tasks(0,_,_,_,_,[]).
generate_tasks(N,S,D,E,C,[T|Ts]) :-
    fd_dom(S,DomS),
    fd_dom(E,DomE),
    fd_dom(D,DomD),
    NewS in DomS,
    NewE in DomE,
    NewD in DomD,
    T = task(NewS,NewD,NewE,C,N,N),
    
    N0 is N - 1,
    generate_tasks(N0,S,D,E,C,Ts).

display_task_info(T) :-
    T = task(S,_,E,C,_,Id),
    write('Task '),write(Id),write(': ==>\t'),
    write('S: '),  write(S) ,write('\t'),
    write('E: '),  write(E) ,write('\t'),
    write('C: '),  write(C) ,nl.
    
    

%%  display_domain(+Var, +LB, +UB) is det
%
%   Prints a representation of Var domain as a stream of characters from LB (lower bound) to UB 
%   (upper bound). Char '-' represents the "included in domain" points. Char ' ' (space) represents 
%   the "not included in domain" points.
%   
%   @see display_domain/5

display_domain(V,Lb,Ub) :-
    fd_dom(V,Dom),
    domain_bs(Dom,Ub,Lb,Bs),!,
    sweep_bs(Bs,write(' '),write('-')),!,nl.    
    
%%  display_domain(+Var, +LB0, +UB0, +Dom2_Inf, +Dom2_Sup) is det
%   
%   Like display_domain/3 but prints two domains at the same time. Domain 1 is that of Var. Domain 2 
%   is =|Dom2_Inf..Dom2_Sup|=. This predicate might be useful to display task times and Start-End 
%   intervals. These are the possible outputs:
%
%   ==
%     -----------------  [#########]    No overlapping
%           -------------{***######]    Partial overlapping
%           --{*********}----           Total overlapping #1
%       [###*****************###]       Total overlapping #2
%   ==

display_domain(V,Lb,Ub,S,E) :-
    fd_dom(V,Dom),
    domain_bs(Dom,Ub,Lb,Bs),!,
    sweep_bs(Bs,Lb,Ub,S,E),!,nl.

domain_bs(_,T,T,[]).
domain_bs(Dom,Tstop,T,[B|Bs]) :-
    T in Dom #==> B,
    T1 is T+1,
    domain_bs(Dom,Tstop,T1,Bs).

sweep_bs([],_,_):- !.
sweep_bs([B|Bs],A0,A1) :-
    (B=0 ->  call(A0)
    ;        call(A1)),
    sweep_bs(Bs,A0,A1).
    
sweep_bs(_,T,T,_,_):- !.
sweep_bs(Bs,T,Tstop,S,E) :-
    nth0(T,Bs,B),
    (B=0 
    ->  ( T=S         -> write('[')
        ; T=E         -> write(']')
        ; (T>=S, T<E) -> write('#')
        ;                write(' ')
        )
    ;   ( T=S         -> write('{')
        ; T=E         -> write('}')
        ; (T>=S, T<E) -> write('*')
        ;                write('-')
        )
    ),
    T1 is T+1,
    sweep_bs(Bs,T1,Tstop,S,E).
    
    
    
    
    
    
    
    
    





