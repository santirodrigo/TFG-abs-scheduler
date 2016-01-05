/* CubeCAT ****************************************************************************************** 
 *  File:   task_planner_prepare.pl                                                                 *
 *  Descr.: Task object builder and domain constrainor.                                             *   
 *  Author: Carles Araguz López.                                                                    *
 *  Date:   2014-feb-20                                                                             *
 *  Vers.:  1.1                                                                                     *
 *  Note:   Comments are structured according to PlDoc documentation system.                        *
 *                                                                                                  *
 *  This file is part of the CubeCAT v1.0 project. "CubeCAT" is an educational project developed at *
 *  the Technical University of Catalonia - BarcelonaTech (Universitat Politècnica de Catalunya).   *
 ****************************************************************************************************
 *  Changelog:                                                                                      *
 *  - v0.1  Araguz C.   Creation.                                                                   *
 *  - v0.2  Araguz C.   Added input loader predicates and modularized.                              *
 *  - v1.0  Araguz C.   Optimized for memory reduction.                                             *
 *  - v1.1  Araguz C.   Modularization changed.                                                     *
 ****************************************************************************************************/

/* Modularization ***********************************************************************************/
:- module(task_planner_prepare, [
    fill/2, 
    zeros/2,
    build_tasks/1,
    constrain_all/4,
    constrain_time/4,
    constrain_temperature/4,
    constrain_radiation/4,
    constrain_position/4,
    read_outfile/2
    ]).

/* Modules to include *******************************************************************************/
:- use_module(library(clpfd)).          /* Constraint Logic Programming over Finite Domains.        */
:- use_module(task_database).           /* Database containing description of each task.            */
:- use_module(task_planner_globals).    /* Task Planner configuration.                              */
:- use_module(task_planner).            /* Task Planner.                                            */

/* Foreign libraries ********************************************************************************/
% SRM: Think do not need it% :- load_foreign_library('/usr/lib/skywalker/libtaskresprolog').

/* TODO:
 *  - Allow parametrization of prediction files' path and names. 
 *  - Enable/disable/register resource types. 
 *  
 */
    
/** <module> Prepare Task Planner

This module describes predicates that will be used together with Task Planner's predicates (see 
planner/3). The basic functionality of the module is to prepare task objects, and their domains, so 
that they can be passed to the scheduler (see schedule/2).

@author Carles Araguz López
*/

%%  prepare(-Tasks,-DynamicRs, -TimeH) is det
%
%   Calls the Energy Scheduler, reads its output files and prepares a list of Tasks. DynamicRs is 
%   bound to a list of dynamic resources. Dynamic resources are defined in task_planner_globals.pl
%   using the functor scheduler_resource/3. TimeH is the end time (or time horizon) for the planner 
%   algorithm and is also defined in task_planner_globals.pl (time_end/1).
%   Static resources are also checked through build_tasks/2.
%
%   Input files are:
%
%   * task_database.pl
%     A term-based database that contains task's scheduling parameters.
%   * task_planner_globals.pl
%     A term-based database that contains global variables (like current time).
%   * energy_scheduler.out _|(interpolated data)|_
%     A file containing the outcome of an Energy Scheduler. The Energy Scheduler algorithm has to 
%     calculate the energy prediction for a given time window and output the data in the following format 
%     ==
%     Energy1. % At time1
%     Energy2.
%     Energy3.
%     ...
%     EnergyN. % At timeN
%     ==
%     Time comments are not compulsory.  
%     The list *|must be|* complete, meaning that the duration of the scheduling time window has 
%     to match the length of the data (i.e. _|Duration_in_secs = N|_). If the Energy Scheduler 
%     algorithm does not provide this resolution, the missing points have to be *previously*
%     interpolated.
%   * predictor_temperature.out
%     A file containing a temperature prediction. No interpolation is needed, but a minimum at 
%     least one point has to be defined. Format has to be:
%     ==
%     [Temperature1,Time1].
%     [Temperature2,Time2].
%     [Temperature3,Time3].
%     ...
%     [TemperatureN,TimeN].
%     ==
%   * predictor_radiation.out
%     Same as temperature but with estimated radiation dose data.
%   * predictor_accesstime.out
%     Same as temperature but with estimated access times (0=link can't be established, 1=link is 
%     possible).
%   * orbit_propagator.out
%     Contains the trajectory of the satellite in the following format:
%     ==
%     [p(Latitude1,Longitude1),Time1].
%     [p(Latitude2,Longitude2),Time2].
%     [p(Latitude3,Longitude3),Time3].
%     ...
%     [p(LatitudeN,LongitudeN),TimeN].
%     ==
%     Interpolation is not required. 
%
%
%   These files are supposed to be created by the Energy Scheduler program, which will be called by
%   a binary named _|energy_scheduler_launcher|_ that must be located in the same directory. 
%   If one of the following situations occur, prepare/2 succeeds and Tasks is an empty list [] 
%   and an exception is thrown.
%
%   Errors thown:
%   *   *|_|second_stage_error|_|* if the predicate prepare_dynres/3 failed; 
%   *   *|_|file_not_found|_|* if task_planner_globals.pl or task_database.pl were not found; 
%   *   *|_|database_missing|_|* if some terms can not be found in the database; 
%   *   *|_|energy_scheduler_error|_|* if Energy Scheduler could not be launched or returned an 
%       error.
%
%   @throws error(Error) Where Error is one of the mentioned above. 

/*
    (true -> (use_module('/home/Skywalker/Syscore/Task_Manager/Task_Planner/dbg/task_planner_globals'),
      use_module('/home/Skywalker/Syscore/Task_Manager/Task_Planner/dbg/task_database'))
    ->  % Task Planner files successfully loaded:
        scheduler_param(time_start, Time),
        scheduler_param(time_end,   Th),
        write('Time start\t'),writeln(Time),
        write('Time end\t'),writeln(Th),
        % Call the Energy Scheduler and Predictors
        process_create(
            '/home/Skywalker/Syscore/Task_Manager/Task_Planner/dbg/energy_scheduler_launcher',
            [Time,Th],[process(PID)]
            ),
        process_wait(PID,exit(Status)),
        
        (Status = 0
        ->  % Energy Scheduler succeeded:
            %Window is abs(Th-Time),
            
            findall(ID,task(ID),Tasks)
            
            %findall(R,scheduler_resource(R,_,_),Rnames),
            %(prepare_dynres(Rnames,DynamicRs,Window) -> true ; throw(error(second_stage_error))),
            %TimeH = time_horizon(Th)
            
        ;   % Energy Scheduler returned an error:
            Tasks = [],
            %DynamicRs = [],
            throw(error(energy_scheduler_error))
        )
        
    ;   % Can't read task_scheduler_globals.pl or task_database.pl:
        Tasks = [],
        %DynamicRs = [],
        throw(error(file_not_found))
    ).
*/

%%  prepare_dynres(+Names, -DynRes, +Length)
%
%   When Names is a list of available resources (i.e. their names) and Length is the expected number
%   of data points, DynRes is a list of dynamic resources prepared for schedule/2. Resources are
%   defined in task_planner_globals.pl (see scheduler_resource/3). This predicate will read the 
%   database and generate the data points for those defined with =value= or read the data points 
%   from the file defined in =file= if that is the case. There is an implicit length check. 
/*
prepare_dynres(Rns,Rs,Lenght) :-
    prepare_dynres_(Rns,Ds,Lenght),
    Rs = resources(Ds).
    
prepare_dynres_([],[],_) :- !.
prepare_dynres_([Rname|Rns],[D|Ds],Lenght) :-
    scheduler_resource(Rname,cumulative(Cumulative),Opt),    
    (   Opt =.. [file|[FilePath]] ->
        % File is defined:
        read_outfile(FilePath,Data),
        length(Data,Lenght),
%        Y = [Data,cumulative(Cumulative)],
%        D =.. [Rname|[Y]]
        D =.. [Rname,Data,[cumulative(Cumulative)]]
        
    ;   Opt =.. [value|[ConstantValue]] ->
        % Constant value is defined:
        length(Data,Lenght),
        fill(Data,ConstantValue),
%        Y = [Data,cumulative(Cumulative)],
%        D =.. [Rname|[Y]]
        D =.. [Rname,Data,[cumulative(Cumulative)]]
        
    ;   % Else
        !,fail
    ),
    prepare_dynres_(Rns,Ds,Lenght).
*/
%%  fill(+List, +Element) is det
%   
%   Fills List with Element. List has to be previously created but their objects uninstantiated (for 
%   instance using =|length(List,Lenght)|=). fill/2 can be used to check whether a list contains 
%   only the same Element repeatedly.

fill([L|Ls],X) :-
    L = X,
    fill(Ls,X).
fill([],_) :- !.

%%  zeros(-List, +Length) is det
%
%   List is a list of zeros with Lenght elements.

zeros([0|Zs],Acc) :-
    Acc > 0,
    Acc0 is Acc-1,
    zeros(Zs,Acc0),!.
zeros([],0).

%%  build_tasks(+TaskIDs:list,-TaskObjs:list)
%
%   TaskObjs is a list of task objects created from the list TaskIDs. Both list have the same length 
%   if every task has a valid domain. Domains are time intervals in which every task's static 
%   constraints are fulfilled (i.e. periods of time in which a task can be scheduled a priori). 
%   Task domains are based on constrain_all/4.
%   
%   *IMPORTANT*: build_tasks/2 expects task_planner_globals module to be fully loaded. If not, the 
%   predicate will fail.
%   
%   @see schedule/2 for a detailed description of the task object. 

build_tasks(Tasks) :-
    findall(ID,task(ID),TaskIDs), % SRM: Creates a list of defined task IDs
    build_tasks_(TaskIDs,Tasks).
build_tasks_([ID|IDs],[Task|Tasks]) :- % SRM: [Tasks is an outgoing parameter]
    scheduler_param(time_start, Tstart), % SRM: "Saves" time_start defined in task_planner_globals in the Tstart variable
    scheduler_param(time_end,   Tend),
    % Constrain domains:
    write('******* Calling constrain_all/4:'),writeln(ID), %SRM: debug
    (constrain_all(Dom,ID,Tstart,Tend)	% SRM: Obtains Time Domain in which the task can be executed (see definition below)
    ->  % Build Task Objects:
        S in Dom,						% SRM: Without instantiating S, adds the information of S being in constrained Domain
        E in Dom,
        %task_timing(ID,_,duration(D1,H1,M1,_),_,_,_), % TODO TODO TODO TODO TODO TODO TODO TODO TODO Fix and allow to use previous predicate % SRM: Esto ya está en la memoria...
        write('******* Calling task_timing/7:'),writeln(ID), %SRM: debug
        task_timing(ID,_,duration(D),_,_,_,_),
        write('******* Calling task_dynamic_resources/6:'),writeln(ID), %SRM: debug
        task_dynamic_resources(ID,resources(Rs)),
        task_priority(ID,Prio),
        Task = task(S,D,E,Rs,Prio,ID), % SRM: Starttime, Duration, Endtime, Rsources(list), Priority, ID
        build_tasks_(IDs,Tasks)
    ;   build_tasks_(IDs,[Task|Tasks]) % SRM: If there is no constrained domain for this task, no task has been initialized
    ).
build_tasks_([],[]) :- !.



%%  prepare_task_dynres(+Resources:list, -DynRes:list, +Duration)
%
%   DynRes is a list of dynamic resource consumption correctly formatted. Formating means checking
%   that the length matches the task's Duration for those Resources defined like a list:
%   ==
%   resource_name([0,2,0,2,2,1]) % Where the number is the actual consumption versus time.
%   ==
%   For those Resources defined as a single value (e.g. =|resource_name(3)|=), prepare_task_dynres/3
%   will create a full list of length Duration.

prepare_task_dynres([],[],_).
prepare_task_dynres([R|Rs],[Dy|Dys],D) :-
    prepare_task_dynres_(R,Dy,D),
    prepare_task_dynres(Rs,Dys,D).

prepare_task_dynres_(R,Dy,L) :-
    R =.. [Name|[Value]],
    (is_list(Value)
    ->  length(Value,L),
        Dy = R
    ;   length(Points,L),
        fill(Points,Value),
        Dy =.. [Name|[Points]]).

%%  constrain_all(-Domain, +TaskID, +Time0, +Time1)
%
%   Domain is the CLP(FD) domain where task TaskID meets all constraining requirements. These 
%   requirements are defined by task's timing, temperature, radiation and position and are declared 
%   in task_timing/7, task_static_constraints/2 and task_positioning/3. The domain is bounded by
%   Time0 and Time1.

constrain_all(Dom,Task,Time0,Time1) :-
    write('******* Calling constrain_time/4:'),writeln(Task), %SRM: debug	
    (constrain_time(DomTime,Task,Time0,Time1)        -> true ; D in inf..sup, fd_dom(D,DomTime)), 
    write('******* Calling constrain_position/4:'),writeln(Task), %SRM: debug
    (constrain_position(DomPos,Task,Time0,Time1)     -> true ; D in inf..sup, fd_dom(D,DomPos)),
    write('******* Calling constrain_temperature/4:'),writeln(Task), %SRM: debug
    (constrain_temperature(DomTemp,Task,Time0,Time1) -> true ; D in inf..sup, fd_dom(D,DomTemp)),
    write('******* Calling constrain_radiation/4:'),writeln(Task), %SRM: debug
    (constrain_radiation(DomRad,Task,Time0,Time1)    -> true ; D in inf..sup, fd_dom(D,DomRad)),
    % SRM: ------ CREATE XTRA DOMAIN -------------------------------------------
    Y in DomTime,
    Y in DomPos,
    Y in DomTemp,
    Y in DomRad,
    fd_dom(Y,DomNorm),
    task_timing(Task,_,duration(Duration),_,_,_,_),
    TimeXtra is Time1 + Duration + 1,
    writeln(TimeXtra),
    T in Time1..TimeXtra, fd_dom(T,XDom),
    X in XDom \/ DomNorm,
    fd_dom(X,Dom),
    %% SRM: Create Extra Domain (Dom2), with task_timing and so...
    % For domains union: X in Dom1 \/ Dom2
    % --------------------------------------------------------------------------
    (define(debug,yes)
    ->  flag(dbg_time,Now,Now),
        define(dbg_dir,Dir),
        format_time(string(StrT),"%Y%m%d_%H%M%S",Now),
        scheduler_param(satellite_id,SatID),
    	format(string(StrSatID),"~d/",[SatID]),
    	string_concat(StrT,StrSatID,StrTime),
        string_concat(Dir,StrTime,Tmp),
        string_concat(Tmp,'domains.out',Path),
        open(Path,append,Stream,[]),
        write(Stream,Task),write(Stream,' '),write(Stream,'time_dom '),write(Stream,DomTime),write(Stream,'\n'),
        write(Stream,Task),write(Stream,' '),write(Stream,'pos_dom '), write(Stream,DomPos), write(Stream,'\n'),
        write(Stream,Task),write(Stream,' '),write(Stream,'rad_dom '), write(Stream,DomRad), write(Stream,'\n'),
        write(Stream,Task),write(Stream,' '),write(Stream,'temp_dom '),write(Stream,DomTemp),write(Stream,'\n'),
        write(Stream,Task),write(Stream,' '),write(Stream,'intersec '),write(Stream,Dom),    write(Stream,'\n'),
        close(Stream),
        write_csv_domain(X,Task,Time0,Time1)
    ;   true).


write_csv_domain(X,ID,T0,T1) :-
    task_priority(ID,P),
    current_output(CO),
    flag(dbg_time,Now,Now),
    define(dbg_dir, Dbgdir),
    format_time(string(StrT),"%Y%m%d_%H%M%S",Now),
    scheduler_param(satellite_id,SatID),
	format(string(StrSatID),"~d/",[SatID]),
	string_concat(StrT,StrSatID,StrTime),
    atomic_list_concat([Dbgdir,StrTime,'domain_',ID,'.csv'],Path),
    open(Path,append,Stream,[]),
    set_output(Stream),
    write_csv_domain_(X,P,T0,T1),
    close(Stream),
    set_output(CO).
write_csv_domain_(X,P,T2,T2) :-
    fd_dom(X,Dom),
    Copy in Dom,
    (Copy #= T2 -> write(P),writeln(',') ; write(0),writeln(',')).
write_csv_domain_(X,P,T0,T2) :-
    fd_dom(X,Dom),
    Copy in Dom,
    (Copy #= T0 -> write(P),writeln(',') ; write(0),writeln(',')), 
    T1 is T0+1,
    write_csv_domain_(X,P,T1,T2).


%%  constrain_time(-Domain, +TaskID, +Time0, +Time1)
%
%   Domain is the CLP(FD) domain where task TaskID meets timing requirements. These 
%   requirements are defined in task_timing/7. The domain is bounded by Time0 and Time1. Mission 
%   start time *must* be defined prior to calling this predicate (see mission_start/1 in 
%   task_planner_globals.pl)
%   
%   constrain_time/4 uses a shared library written in C, that connects to the SQLite task database.
%   
%   @see SDB/Resource_Handler/task_resources.c

constrain_time(Dom,Task,Time,Th) :-
    Th > Time,
    scheduler_param(mission_start,MStime),
    Time > MStime,
    
    %task_timing(Task,period(D0,H0,M0,_),duration(D1,H1,M1,_),init_delay(D2,H2,M2,_),Iterations,Drift), % TODO TODO TODO TODO TODO TODO TODO TODO TODO Fix and allow to use previous predicate
	task_timing(Task, _, _, init_delay(Delay), _, _, deadline(Deadline)),
    T in Time..Th,
    T #> Delay,
    T #=< Deadline,
    fd_dom(T, Dom).
%    
%    task_iteration(Task,I_curr),
%    task_last_start(Task,LS),!,
%    
%    (   Iterations = infinity 
%    ->  (   I_curr > 0
%        ->  I_left #= (Th-(LS+Period))/Period       % If it has been run: I_left is all the iterations fitting within its next start time and the horizon.
%        ;   I_left #= (Th-(MStime+Delay))/Period    % If it has never been run: I_left is all the iterations since the theoretical start, until the time horizon.
%        ) 
%    ;   I_left #= Iterations-I_curr),               % If it has to run a finite number of times: I_left is the amount of iterations left to do.
%    
%    (   Time < MStime+Delay
%        % Td: "time delay"
%    ->  Td #= MStime+Delay          % If task does not have to start at Time, but later: Td is set to the time at which it should start.
%    ;   (   I_curr = 0              
%        ->  Td #= MStime+Delay      % If task has not started: Td is also the time at which it should start.
%        ;   Td #= LS+Period)),      % If the task has alreay started: Td is the next start time.
%    
%    /* 
%    The domain is found following the algorithm below:
%    - EST = Earliest Start Time = Initial_delay - Drift.
%    - LET = Latest Start Time = End_time + Drift = Initial_delay + Duration + Drift. % SRM: ???
%    - Drift is calculated as a fraction of the Period.
%    - T_0 is: all time values inside one time slot.
%    - T_i is: all time values inside all time slots.
%      Finding all possible T_i the actual domain can be created as a union of points.  
%    */
%    EST is round(-Period*Drift)+Td, 

%%===============================================================================    
%    % SRM: Con deadline habría que hacer:
%    % LET is min(round(Duration+Period*Drift)+Td, Deadline), CREO...
%%===============================================================================    
%    LET is round(Duration+Period*Drift)+Td, 
%    
%    T_0 in EST..LET,!,  
%    
%    findall(T_i,(
%        T_i #= T_0 + K*Period,
%        K in 0..I_left,
%        T_i in Time..Th,
%        label([T_i,K,T_0]) 
%    ),T_is),
%    
%    union_points(T_is,Dom).

union_points(Ts,Dom) :-
    X #= -1,
    fd_dom(X,Void),
    union_points_(Ts,Void,Dom0),
    Y in Dom0 #/\ Y #\= -1,
    fd_dom(Y,Dom).
union_points_([],Dom,Dom) :- !.
union_points_([T|Ts],Dom,U) :-
    X in Dom \/ T,
    fd_dom(X,NewDom),
    union_points_(Ts,NewDom,U).
    


%%  constrain_temperature(-Domain, +TaskID, +Time0, +Time1)
%
%   Domain is the CLP(FD) domain where task TaskID meets temperature requirements. These 
%   requirements are defined in task_static_constraints/2. The domain is bounded by Time0 and Time1.
%   Input comes from the file predictor_temperature.out, which *must* exist prior to calling this 
%   predicate.

constrain_temperature(Dom,Task,Time,Th) :-
    Th > Time,
    task_static_constraints(Task,resources(Rs)),
    Term =.. [temperature_range|[TempMin|[TempMax]]],
    (memberchk(Term,Rs)
    ->  scheduler_param(pred_temperature,Path),
        read_outfile(Path,TempPoints),
        bounded_domain(TempMin,TempMax,TempPoints,Dom,Time)
    ;   Dom #= inf..sup
    ).
    
%%  constrain_radiation(-Domain, +TaskID, +Time0, +Time1)
%
%   Domain is the CLP(FD) domain where task TaskID meets radiation requirements. These 
%   requirements are defined in task_static_constraints/2. The domain is bounded by Time0 and Time1.
%   Input comes from the file predictor_radiation.out, which *must* exist prior to calling this 
%   predicate.

constrain_radiation(Dom,Task,Time,Th) :-
    Th > Time,
    task_static_constraints(Task,resources(Rs)),
    Term =.. [radiation_range|[RadMin|[RadMax]]],
    (memberchk(Term,Rs)
    ->  scheduler_param(pred_radiation,Path),
        read_outfile(Path,RadPoints),
        bounded_domain(RadMin,RadMax,RadPoints,Dom,Time)
    ;   Dom #= inf..sup
    ).
    

bounded_domain(Min,Max,Ps,Domain,Inf) :-
    bounded_domain_(Ps,Min,Max,0,0,Dom),!,
    X #\= 0, % 0 is never part of the domain 
    X #>= Inf,
    X in Dom,
    fd_dom(X,Domain).
bounded_domain_([],_,_,_,Dom,Dom).
bounded_domain_([[P|[T]]|Ps],Min,Max,LastT,DomP,DomN) :-
    (   (P >= Min, P =< Max)
    ->  (   LastT in DomP
        ->  X in DomP\/LastT..T,
            fd_dom(X,NewDom),
            bounded_domain_(Ps,Min,Max,T,NewDom,DomN)
        ;   X in DomP\/T,
            fd_dom(X,NewDom),
            bounded_domain_(Ps,Min,Max,T,NewDom,DomN)
        )   
    ;   (   LastT in DomP
        ->  Tf is T-1,
            X in DomP\/LastT..Tf,
            fd_dom(X,NewDom),
            bounded_domain_(Ps,Min,Max,T,NewDom,DomN)
        ;   bounded_domain_(Ps,Min,Max,T,DomP,DomN)
        )
    ).


%%  constrain_position(-Domain, +TaskID, +Time0, +Time1)
%
%   Domain is the CLP(FD) domain where task TaskID meets position requirements. These 
%   requirements are defined in task_positioning/3. The domain is bounded by Time0 and Time1.
%   Input comes from the file orbit_propagator.out, which *must* exist prior to calling this 
%   predicate.

constrain_position(Dom,Task,Time,Th) :- 
    Th > Time,
    task_positioning(Task,positions([]),_), % No points have been defined
    X in Time..Th,
    fd_dom(X,Dom).
constrain_position(Dom,Task,Time,Th) :-
    Th > Time,
    task_positioning(Task,positions(Spots),Tolerance),
    scheduler_param(orbit_propagator,Path),
    read_outfile(Path,Trajectory),
    spots(Spots,Tolerance,Trajectory,Dom).
    
spots(Spots,Tol,Trajs,Dom) :-
    spots_(Trajs,Spots,Tol,0,0,Dom).
spots_([],_,_,_,Dom,Dom) :- !.
spots_([[Traj|[Time]]|Trajs],Spots,Radius,LastT,DomP,DomN) :-
    maplist(distance(Traj),Spots,Distances),
    include(>=(Radius),Distances,Ds),
    (   Ds = []
    ->  % The point Traj is not part of the domain
        (   LastT in DomP
        ->  Tf is Time-1,
            X in DomP\/LastT..Tf,
            fd_dom(X,NewDom),
            spots_(Trajs,Spots,Radius,Time,NewDom,DomN)   
        ;   spots_(Trajs,Spots,Radius,Time,DomP,DomN)
        )
    ;   % The point Traj is part of the domain
        (   LastT in DomP
        ->  X in DomP\/LastT..Time,
            fd_dom(X,NewDom),
            spots_(Trajs,Spots,Radius,Time,NewDom,DomN)
        ;   X in DomP\/Time,
            fd_dom(X,NewDom),
            spots_(Trajs,Spots,Radius,Time,NewDom,DomN)
        )
    ).
    
distance(P1,P2,D) :-
    P1 = p(A1,B1),
    P2 = p(A2,B2),
    A is A1-A2,
    B is B1-B2,
    D is sqrt(A^2+B^2).


task_state(_,halt).                   % Debug purposes
task_state(T,S) :-
    task_db(T,state,S),!.             % Foreign (task_resources.so)

task_last_start(_,-1000).             % Debug purposes
task_last_start(T,LS) :-
    task_db(T,last_start_time,LS),!.  % Foreign (task_resources.so)
task_last_start(_,0).

task_iteration(_,0).                  % Debug purposes
task_iteration(T,I) :-
    task_db(T,current_iteration,I),   % Foreign (task_resources.so)
    must_be(nonneg,I),!.
task_iteration(_,0).

task_last_change(_,-1000).            % Debug purposes
task_last_change(T,LC) :-
    task_db(T,last_change,LC),!.      % Foreign (task_resources.so)


%%  read_outfile(+File, -Elements) is det
%
%   Reads a term-based File and saves each term in the Elements list.

read_outfile(File,Elems) :-
    seeing(Input),
    see(File),
    inquire([],Elems),
    seen,
    see(Input).
inquire(In,Out) :-
    read(Data),
    (   Data == end_of_file
    ->  Out = In
    ;   append(In,[Data],In2),
        inquire(In2,Out)
    ).









