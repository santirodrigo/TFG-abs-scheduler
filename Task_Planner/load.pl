/* CubeCAT ******************************************************************************************
 *  File:   load.pl                                                                                 *
 *  Descr.: Loads the Task Planner environment.                                                     *    
 *  Author: Carles Araguz López.                                                                    *
 *  Date:   2014-feb-27                                                                             *
 *  Vers.:  1.1.2                                                                                   *
 *  Note:   Comments are structured according to PlDoc documentation system.                        *
 *                                                                                                  *
 *  This file is part of the CubeCAT v1.0 project. "CubeCAT" is an educational project developed at *
 *  the Technical University of Catalonia - BarcelonaTech (Universitat Politècnica de Catalunya).   *
 ****************************************************************************************************
 *  Changelog:                                                                                      *
 *  - v1.0  Araguz C.   Creation.                                                                   *
 *  - v1.1  Araguz C.   Modularization changed.                                                     *
 *  - v1.2  Araguz C.   A different folder for each drun/0 execution is now atomatically created.   *
 ****************************************************************************************************/

:- style_check(+singleton).
:- style_check(-no_effect).

/* Modules to include *******************************************************************************/
:- use_module(library(clpfd)).          /* Constraint Logic Programming over Finite Domains         */
:- use_module(task_planner_prepare).    /* Task Planner previous predicates.                        */
:- use_module(task_planner).            /* Task Planner.                                            */
:- use_module(stat).                    /* Statistics generator (used as in drun/0)                 */


%% define(Parameter,Value)
%

define(debug     , yes ).               /* Whether to debug or not.                                 */
define(out_dir   ,'out/').              /* Output directory (should end with "/")                   */
define(out_file  ,'task_planner.out').  /* Output file name.                                        */
define(out_time	 ,'last_path.out').		/* Time depending output path								*/
define(global_file  ,'satellite.out').  /* Output file name.                                        */
define(dbg_dir   ,'out/').              /* Debug directory (should end with "/")                    */
define(use_folder, yes).                /* Whether to use a different folder preceding these paths. */
define(dbg_sampling_freq, 0.1).         /* Sampling frequency for drun stats (in seconds).          */
         
%%  drun
%

drun :-
    get_time(Now), 
    flag(dbg_time,_,Now),
    define(out_dir, OutDir),
    define(out_file,OutFile),
    define(global_file,GlobalFileTmp),
    define(out_dir, DbgDir),
    define(out_time, OutTime),
    (define(use_folder,yes) 
    ->  format_time(string(StrT),"%Y%m%d_%H%M%S",Now),
        scheduler_param(satellite_id,SatID),
    	string_concat(OutDir,OutTime,OutT),
    	format(string(StrSatID),"~d/",[SatID]),
    	string_concat(StrT,StrSatID,StrTime),
    	writeln(StrTime),
        string_concat(OutDir,StrTime,OutTmp),
        string_concat(DbgDir,StrTime,DbgPath),
        string_concat(OutDir,StrSatID,GlobalPath),
        catch(make_directory(OutDir),_,true),
        catch(make_directory(DbgDir),_,true),
        catch(make_directory(DbgPath),_,true),
        catch(make_directory(GlobalPath),_,true),
        catch(make_directory(OutTmp),_,true),
        string_concat(OutTmp,OutFile,OutPath),
        string_concat(GlobalPath,GlobalFileTmp,GlobalFile)
    ;   
        string_concat(OutDir,OutFile,OutPath),
        DbgPath = DbgDir
    ),
    
    % Clean files:
    (exists_file(OutPath) -> delete_file(OutPath) ; true),
    (exists_file(DbgPath) -> delete_file(DbgPath) ; true),
    (exists_file(GlobalFile) -> delete_file(GlobalFile) ; true),
    
    (define(dbg_sampling_freq,StatRate) -> true ; StatRate = 1),
    stat_init(DbgPath),
    stat_start(StatRate),
    stat_cpu,
    !,
    
    writeln('******* Calling build_tasks/2:'),
    time(build_tasks(Tasks)),nl,
    
    writeln('******* Calling planner/3:'),
    time((
            planner(Tasks,Outcomes),
            (Outcomes = []
            ->  writeln('******* There is no solution.'),
                open(OutT,write,Stream,[]),
                write(Stream,StrTime),
                close(Stream)                
            ;   writeln('******* Plan successfully generated.'),
                write_list_outcomes(Outcomes,OutPath),
                open(OutT,write,Stream,[]),
                write(Stream,StrTime),
                close(Stream),
                send_answer_to_global(Outcomes,GlobalFile)
            )
         )),!,
    
    stat_cpu,
    stat_mem,
    stat_stop,

    length(Tasks,L),
    list_stat_info(Outcomes,L),
    !.
    
list_stat_info([],_) :- !.
list_stat_info([F-Outcome|Outcomes],L) :-
	stat_info(F-Outcome,L),
	list_stat_info(Outcomes,L).
	
write_list_outcomes([],_) :- !.
write_list_outcomes([F-Outcome|Outcomes],OutPath) :-
	write_outcome(Outcome,OutPath),
	write_list_outcomes(Outcomes,OutPath).

% SRM: Passing the answer to the global (via a message written in a local file)-
write_answer_fraction([],Stream) :-
	write(Stream,'\n').
write_answer_fraction([Task|Ts],Stream) :-
	Task = task(Start,_,_,_,_,Id),
	scheduler_param(time_end,TEnd),
	(Start >= TEnd -> true ; write(Stream,Id),write(Stream,':')),
	write_answer_fraction(Ts,Stream).

send_answer_to_global([],GlobalFile).
send_answer_to_global([F-Outcome|Outcomes],GlobalFile) :-
	open(GlobalFile,append,Stream,[]),
	F = [Cij, Gij, Uij, Eij, Dij],
	scheduler_param(weights,[Wc,Wg,Wu,We,Wd]),
	Fpond is ((Wc * Cij) + (Wg * Gij) + (Wu * Uij) + (We * Eij) + (Wd * Dij)),
	write(Stream,'F:'),write(Stream,Fpond),write(Stream,':'),
    write_answer_fraction(Outcome,Stream),
    close(Stream),
	send_answer_to_global(Outcomes,GlobalFile).

% ------------------------------------------------------------------------------

write_outcome(Outcome,Path) :-
    open(Path,append,Stream,[]),
    write_task(Outcome,Stream),
    close(Stream).
write_task([],_) :- !.
write_task([T|Ts],Strm) :-
    T = task(S,_,E,_,_,Id),
    write(Strm,'TASK='), write(Strm,Id),write(Strm,','),
    write(Strm,'START='),write(Strm,S),write(Strm,','),
    write(Strm,'DEADLINE='), write(Strm,E),write(Strm,'.\n'),
    write_task(Ts,Strm).
    

    
    
    
% EOF
