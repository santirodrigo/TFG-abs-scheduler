/* CubeCAT ****************************************************************************************** 
 *  File:   stat.pl                                                                                 *
 *  Descr.: Predicates to save statistical information.                                             *   
 *  Author: Carles Araguz López.                                                                    *
 *  Date:   2014-aug-04                                                                             *
 *  Vers.:  1.1                                                                                     *
 *  Note:   Comments are structured according to PlDoc documentation system.                        *
 *                                                                                                  *
 *  This file is part of the CubeCAT v1.0 project. "CubeCAT" is an educational project developed at *
 *  the Technical University of Catalonia - BarcelonaTech (Universitat Politècnica de Catalunya).   * 
 ****************************************************************************************************
 *  Changelog:                                                                                      *
 *  - v1.0  Araguz C.   Creation.                                                                   *
 *  - v1.1  Araguz C.   Added generate_csv/0. Statistics are saved in a diff. folder for each test. *
 ****************************************************************************************************/
 
 
/* Modularization ***********************************************************************************/
:- module(stat, [
                    stat_mem/0,
                    stat_cpu/0,
                    stat_info/2,
                    stat_init/1,
                    stat_start/1,
                    stat_tick/2,
                    stat_stop/0,
                    stat_param/2
                ]).

/* Modules to include *******************************************************************************/
:- use_module(library(time)).           /* Time and alarm library.                                  */
:- use_module(task_database).           /* Database containing description of each task.            */
:- use_module(task_planner_globals).    /* Task Planner configuration.                              */
:- use_module(task_planner).            /* Task Planner core.                                       */

/** <module> stat Statistics Aiding Module
TODO doc
*/

stat_param(mem_file     , 'stat_mem.csv').
stat_param(cpu_file     , 'stat_cpu.csv').
stat_param(inf_file     , 'stat_inf.txt').
stat_param(sch_file     , 'stat_sched.csv').
stat_param(sta_file     , 'stat_static_constraints.csv').
stat_param(dyn_file     , 'stat_dynamic_resources.csv').

stat_init(DbgDir) :-
    stat_param(mem_file, MemF),
    stat_param(cpu_file, CPUF),
    stat_param(inf_file, InfF),
    
    atomic_list_concat([DbgDir,MemF],MemPath),
    atomic_list_concat([DbgDir,CPUF],CPUPath),
    atomic_list_concat([DbgDir,InfF],InfPath),
    
    (exists_directory(DbgDir)   -> true                 ; catch(make_directory(DbgDir),_,true)),
    (exists_file(MemPath)       -> delete_file(MemPath) ; true),
    (exists_file(CPUPath)       -> delete_file(CPUPath) ; true),
    (exists_file(InfPath)       -> delete_file(InfPath) ; true),
    
    statistics(walltime,_),
    
    open(MemPath,write,Stream0,[]),
    write(Stream0,'timestamp,global_allocated,global_used,local_allocated,local_used,trail_allocated,trail_used,stack\n'),
    close(Stream0),
    open(CPUPath,write,Stream1,[]),
    write(Stream1,'timestamp,cputime,inferences,gc_count,gc_gained,gc_time,gc_left\n'),
    close(Stream1).
    
stat_start(T) :-
    flag(stat_stop,_,false),
    statistics(walltime,[W,_]),
    flag(start_walltime,_,W),
    alarm(T,stat_tick(T,Id),Id,[remove(false),install(true)]).

stat_stop :- 
    flag(stat_stop,_,true).

stat_tick(T,Id) :-
    uninstall_alarm(Id),
    flag(stat_stop,Stop,Stop),
    (Stop = false 
    ->  install_alarm(Id,T)
    ;   remove_alarm(Id)),
    stat_mem.
    
stat_cpu :-
    stat_param(cpu_file,CPUF),
    flag(dbg_time,Now,Now),
    define(dbg_dir,DbgDir),
    format_time(string(StrTime),"%Y%m%d_%H%M%S/",Now),
    atomic_list_concat([DbgDir,StrTime,CPUF],CPUPath),
    open(CPUPath,append,Strm,[]),
    
    statistics(inferences,Inferences),
    statistics(garbage_collection,[GCCount,GCGained,GCTime,GCLeft]),
    statistics(cputime,CPUTime),
    get_time(Time),
    
    
    write(Strm,Time),write(Strm,','),
    write(Strm,CPUTime),write(Strm,','),
    write(Strm,GCCount),write(Strm,','),
    write(Strm,GCGained),write(Strm,','),
    write(Strm,GCTime),write(Strm,','),
    write(Strm,GCLeft),write(Strm,','),
    write(Strm,Inferences),write(Strm,'\n'),
    
    close(Strm).

stat_mem :-
    stat_param(mem_file, MemF),
    flag(dbg_time,Now,Now),
    define(dbg_dir,DbgDir),
    format_time(string(StrTime),"%Y%m%d_%H%M%S/",Now),
    atomic_list_concat([DbgDir,StrTime,MemF],MemPath),
    open(MemPath,append,Strm,[]),
    
    get_time(Time),
    statistics(global,GA),
    statistics(globalused,GU),
    statistics(local,LA),
    statistics(localused,LU),
    statistics(trail,TA),
    statistics(trailused,TU),
    statistics(stack,St),
    
    flag(ga_max,GAMax,GAMax),
    flag(gu_max,GUMax,GAMax),
    flag(la_max,LAMax,LAMax),
    flag(lu_max,LUMax,LUMax),
    flag(ta_max,TAMax,TAMax),
    flag(tu_max,TUMax,TUMax),
    flag(st_max,StMax,StMax),
    
    (GA > GAMax -> flag(ga_max,_,GA) ; true),
    (GU > GUMax -> flag(gu_max,_,GU) ; true),
    (LA > LAMax -> flag(la_max,_,LA) ; true),
    (LU > LUMax -> flag(lu_max,_,LU) ; true),
    (TA > TAMax -> flag(ta_max,_,TA) ; true),
    (TU > TUMax -> flag(tu_max,_,TU) ; true),
    (St > StMax -> flag(st_max,_,St) ; true), 
    
    write(Strm,Time),write(Strm,','),
    write(Strm,GA),write(Strm,','),
    write(Strm,GU),write(Strm,','),
    write(Strm,LA),write(Strm,','),
    write(Strm,LU),write(Strm,','),
    write(Strm,TA),write(Strm,','),
    write(Strm,TU),write(Strm,','),
    write(Strm,St),write(Strm,'\n'),
    
    close(Strm).

stat_info(Ts,TaskCount) :-
    stat_param(inf_file, InfF),
    flag(dbg_time,Now,Now),
    define(dbg_dir,DbgDir),
    format_time(string(StrTime),"%Y%m%d_%H%M%S/",Now),
    atomic_list_concat([DbgDir,StrTime,InfF],InfPath),
    open(InfPath,write,Stream,[]),
    current_output(CO),
    set_output(Stream),
    write_info(Ts,TaskCount),
    set_output(CO),
    close(Stream),
    write_info(Ts,TaskCount),
    generate_csv(Ts).
    
write_info(Ts,TaskCount) :-
    flag(ga_max,GAMax,GAMax),
    flag(gu_max,GUMax,GAMax),
    flag(la_max,LAMax,LAMax),
    flag(lu_max,LUMax,LUMax),
    flag(ta_max,TAMax,TAMax),
    flag(tu_max,TUMax,TUMax),
    flag(st_max,StMax,StMax),
    statistics(inferences,Inferences),
    statistics(garbage_collection,[GCCount,GCGained,GCTime,GCLeft]),
    statistics(walltime,[Walltime,_]),
    statistics(system_time,[Systime,_]),
    statistics(cputime,CPUtime),
    statistics(global,GA),
    statistics(globalused,GU),
    statistics(local,LA),
    statistics(localused,LU),
    statistics(trail,TA),
    statistics(trailused,TU),
    statistics(stack,St),
    
    CPUtime_ms is CPUtime*1000,    
    flag(start_walltime,SWalltime,SWalltime),
    Elapsed is (Walltime-SWalltime),
    
    format('~`=t STATISTICS ~`=t~100|~n~n'),
    format('CPU time: ~`.t ~3f ms ~50|  Inferences: ~`.t ~D~100|~n',[CPUtime_ms, Inferences]),
    format('System time: ~`.t ~D ms ~50|  Run time: ~`.t ~2f ms~100|~n',[Systime, Elapsed]),
    format('GC. count: ~`.t ~d ~50|  GC. time: ~`.t ~2f ms~100|~n',[GCCount, GCTime]),
    format('GC. gained: ~`.t ~D Bytes ~50|  GC. left: ~`.t ~D Bytes~100|~n~n',[GCGained, GCLeft]),
    
    format('Global stack alloc.: ~`.t ~D Bytes ~50|  Local stack alloc.: ~`.t ~D Bytes~100|~n',[GA, LA]),
    format('Global stack used: ~`.t ~D Bytes ~50|  Local stack used: ~`.t ~D Bytes~100|~n',[GU, LU]),
    format('Trail stack alloc.: ~`.t ~D Bytes ~50|  C stack alloc.: ~`.t ~D Bytes~100|~n',[TA, St]),
    format('Trail stack used: ~`.t ~D Bytes ~50|~n~n',[TU]),
    
    format('Max. Gstack alloc.: ~`.t ~D Bytes ~50|  Max. Lstack alloc.: ~`.t ~D Bytes~100|~n',[GAMax, LAMax]),
    format('Max. Gstack used: ~`.t ~D Bytes ~50|  Max. Lstack used: ~`.t ~D Bytes~100|~n',[GUMax, LUMax]),
    format('Max. Tstack alloc.: ~`.t ~D Bytes ~50|  Max. C stack alloc.: ~`.t ~D Bytes~100|~n',[TAMax, StMax]),
    format('Max. Tstack used: ~`.t ~D Bytes ~50|~n~n~n~n',[TUMax]),
    
    
    %write_options,
    format('~`=t PLANNER CONFIGURATION ~`=t~100|~n~n'),
    
    scheduler_param(mission_start       , MStart),
    scheduler_param(time_start          , T0),
    scheduler_param(time_end            , T1),
    scheduler_param(domain_similarity   , DomSim),
    scheduler_param(minimize_overlapping, MO),
    scheduler_param(mo_allow_permutation, MOAP),
    scheduler_param(mo_max_group_members, MOMGM),
    scheduler_param(mo_once             , MOO),
    scheduler_param(labeling_options    , Labeling),
    scheduler_param(algorithm_timeout   , Timeout),
    T2 is T1-T0,
    DomSimP is DomSim*100,
    format('~32t~20|Mission start time ~`.t ~D~80|~n',[MStart]),
    format('~32t~20|Horizon start time ~`.t ~D~80|~n',[T0]),
    format('~32t~20|Horizon end time ~`.t ~D~80|~n',[T1]),
    format('~32t~20|Horizon window ~`.t ~D points~80|~n',[T2]),
    format('~32t~20|Enable optimization: Minimize Overlaps ~`.t ~w~80|~n',[MO]),
    format('~32t~20|(M.O.) Domain similarity ~`.t ~0f %~80|~n',[DomSimP]),
    format('~32t~20|(M.O.) Allow group permutations ~`.t ~w~80|~n',[MOAP]),
    format('~32t~20|(M.O.) Maximum group members ~`.t ~w~80|~n',[MOMGM]),
    format('~32t~20|(M.O.) Do not create choicepoints ~`.t ~w~80|~n',[MOO]),
    format('~32t~20|Labeling options ~`.t ~w~80|~n',[Labeling]),
    format('~32t~20|Algorithm timeout ~`.t ~d s~80|~n~n',[Timeout]),
    
    format('List of resources found in task_planner_globals.pl~n'),
    format('+~`-t~20|+~`-t~60|+~`-t~80|+~`-t+~100|~n'),
    format('| Resource name ~32t~20|| Options ~32t~60|| Points ~32t~80|| First pair ~32t|~100|~n'),
    format('+~`-t~20|+~`-t~60|+~`-t~80|+~`-t+~100|~n'),
    findall(R,resource_options(R,_),Rs),
    write_resource(Rs),
    format('+~`-t~20|+~`-t~60|+~`-t~80|+~`-t+~100|~n~n'),
    
    length(Ts,TaskN),
    TaskM is TaskCount - TaskN,
    format('List of built tasks (showing ~d, skipped ~d tasks without solution):~n',[TaskN,TaskM]),
    format('+~`-t~20|+~`-t~30|+~`-t~50|+~`-t~65|+~`-t~85|+~`-t+~100|~n'),
    format('| Task ID~32t~20||  Prio.~32t~30||  Start~32t~50||  Duration~32t~65||  End~32t~85||  N. cons.~32t|~100|~n'),
    format('+~`-t~20|+~`-t~30|+~`-t~50|+~`-t~65|+~`-t~85|+~`-t+~100|~n'),
    write_task(Ts),
    format('+~`-t~20|+~`-t~30|+~`-t~50|+~`-t~65|+~`-t~85|+~`-t+~100|~n'),
    %write_solution(Os).
    !.
    
write_task([]).
write_task([T|Ts]) :-
    T = task(S,D,E,C,Prio,Id),
    length(C,CN),
    format('| ~w~32t~20||  ~d~32t~30||  ~D~32t~50||  ~D~32t~65||  ~D~32t~85||  ~d~32t|~100|~n',[Id,Prio,S,D,E,CN]),
    write_task(Ts).
    
write_resource([]).
write_resource([R|Rs]) :-
    resource_options(R,[Opts]),
    resource_capacity(R,[V-T|Ps]),
    length([V-T|Ps],L),
    format('| ~w ~32t~20|| ~w ~32t~60|| ~d ~32t~80|| ~w ~32t|~100|~n',[R,Opts,L,V-T]),
    write_resource(Rs).
    
    
generate_csv([]).
generate_csv(Ts) :-
    scheduler_param(pred_radiation,RadPath),
    read_outfile(RadPath,RadPoints),
    scheduler_param(pred_temperature,TempPath),
    read_outfile(TempPath,TempPoints),
    scheduler_param(time_start, Tstart),
    scheduler_param(time_end,   Tend),
    current_output(CO),
    flag(dbg_time,Now,Now),
    define(dbg_dir, Dbgdir),
    format_time(string(StrTime),"%Y%m%d_%H%M%S/",Now),
    !,
    % ----------------------------------------------
    stat_param(sch_file, SchF),
    atomic_list_concat([Dbgdir,StrTime,SchF],SchPath),
    open(SchPath,write,Stream1,[]),
    set_output(Stream1),
    write_csv_schedule(Ts,Tstart,Tend), % Schedule
    close(Stream1),
    % ----------------------------------------------
    !,
    % ----------------------------------------------
    %findall(R,resource_options(R,_),Rnames),
    %stat_param(dyn_file, DynF),
    %atomic_list_concat([Dbgdir,StrTime,DynF],DynPath),
    %open(DynPath,write,Stream2,[]),
    %set_output(Stream2),
    %write_csv_dynamic(Ts,Rnames,Tstart,Tend), % Dynamic
    %close(Stream2),
    % ----------------------------------------------
    %!,
    % ----------------------------------------------
    stat_param(sta_file, StaF),
    atomic_list_concat([Dbgdir,StrTime,StaF],StaPath),
    open(StaPath,write,Stream3,[]),
    set_output(Stream3),    
    write_csv_static(RadPoints,TempPoints), % Static
    close(Stream3),
    % ----------------------------------------------
    set_output(CO).
    
write_csv_schedule(Ts,T0,T1) :-
    maplist(arg(6),Ts,IDs),
    write('Time,'),
    write_ids(IDs),nl,
    write_csv_schedule_(Ts,T0,T1).

write_csv_schedule_(_,T1,T1).
write_csv_schedule_(Ts,T0,T1) :-
    write(T0),write(','),
    task_states_at(Ts,T0),
    nl,
    T2 is T0+1,
    write_csv_schedule_(Ts,T2,T1).

write_ids([]).
write_ids([I|Is]) :- 
    write(I),write(','),
    write_ids(Is). 

task_states_at([],_).
task_states_at([Task|Ts],T) :-
    Task = task(S,_,E,_,Prio,_),
    ((T >= S, T < E) -> write(Prio) ; write(0)),
    write(','),
    task_states_at(Ts,T).

    
write_csv_static(As,Bs) :-
    writeln('radiation,temperature'),
    static_data(As,Bs).
static_data([[A,_]|As],[[B,_]|Bs]) :-
    write(A),write(','),writeln(B),
    static_data(As,Bs).
static_data([],[]).



write_csv_dynamic(Ts,Rnames,T0,T1) :-
    write('time'),
    write_csv_header(Rnames),nl,
    length([energy],L),
    zeros(Zs,L), % Initialize consumptions (needed for cumulative)
    dynamic_data(Ts,[energy],Zs,T0,T1).

write_csv_header([R|Rs]) :-
    write(','),
    write(R),write('_cap,'),
    write(R),write('_con'),
    write_csv_header(Rs).
write_csv_header([]).


dynamic_data(Ts,Rnames,PrevCapCons,T1,T1) :-
    write(T1), write(','),
    calc_consumptions_at(Ts,Rnames,T1,PrevCapCons,CapCons),
    write_cap_cons(CapCons),nl.
dynamic_data(Ts,Rnames,PrevCapCons,T0,T1) :-
    write(T0), write(','),
    calc_consumptions_at(Ts,Rnames,T0,PrevCapCons,CapCons),
    write_cap_cons(CapCons),nl,
    T2 is T0+1,
    dynamic_data(Ts,Rnames,CapCons,T2,T1).
    

calc_consumptions_at(Ts,[Rname|Rnames],T,[PCon|Ps],[Cap|[Con|Cs]]) :-
    resource_capacity_at(Rname,T,Cap),
    total_consumption_at(Rname,Ts,T,PCon,0,Con),
    write(Cap),write(','),
    write(Con),write(','),
    calc_consumptions_at(Ts,Rnames,T,Ps,Cs).
calc_consumptions_at(_,[],_,[],[]).

total_consumption_at(Rname,[Task|Ts],T,PrevCon,Acc,Con) :-
    Task = task(S,_,E,Cs,_,_),
    ((T >= S, T < E)
    ->  C =.. [Rname|[Pairs]],
        memberchk(C,Cs),
        I is T-S,
        seek_time(I,Pairs,V),
        Acc1 is Acc + V,  
        total_consumption_at(Rname,Ts,T,PrevCon,Acc1,Con)
    ;   total_consumption_at(Rname,Ts,T,PrevCon,Acc, Con)).

total_consumption_at(Rname,[],_,PrevCon,Acc,Con) :-
    resource_options(Rname,Opts),
    O =.. [cumulative|[Cumulative]],
    memberchk(O,Opts),
    (Cumulative = true -> Con is PrevCon+Acc ; Con=Acc).
    
write_cap_cons([C|Cs]) :- write(C),write(','),write_cap_cons(Cs).
write_cap_cons([]).
    
    
    
    
    
    
% EOF
    
