/* CubeCAT ****************************************************************************************** 
 *  File:   task_planner_globals.pl                                                                 *
 *  Descr.: Parameter and resource database.                                                        *   
 *  Author: Carles Araguz López.                                                                    *
 *  Date:   2014-feb-20                                                                             *
 *  Vers.:  1.3                                                                                     *
 *  Note:   Comments are structured according to PlDoc documentation system.                        *
 *                                                                                                  *
 *  This file is part of the CubeCAT v1.0 project. "CubeCAT" is an educational project developed at *
 *  the Technical University of Catalonia - BarcelonaTech (Universitat Politècnica de Catalunya).   * 
 ****************************************************************************************************
 *  Changelog:                                                                                      *
 *  - v1.3  Araguz C.   Header comments included.                                                   *
 ****************************************************************************************************/


/* MODULARIZATION ***********************************************************************************/
:- module(task_planner_globals_1, [
                                scheduler_param/2,
                                resource_options/2,
                                resource_capacity/2
                                ]).

/** <module> Global Parameters and Resources

This module includes the Task Scheduler parameters which configure the reduction of task domains and 
the solver algorithm. It also contains all the available resources, listed in facts, their options
and their capacities.

@author Carles Araguz López
*/


%%  scheduler_param(+Param,-Value)
%
%   

scheduler_param(satellite_id		, 1).
scheduler_param(mission_start       , -1000).
scheduler_param(time_start          , 0).
scheduler_param(time_end            , 5).
scheduler_param(priority_periods	, 10). % N_s
scheduler_param(weights,[1,1,1,1,1]). % Weights for F: 	scheduler_param(weights,[Wc,Wg,Wu,We,Wd]),
scheduler_param(domain_similarity   , 0.70).
scheduler_param(minimize_overlapping, false).
scheduler_param(mo_allow_permutation, false).
scheduler_param(mo_max_group_members, 10).
scheduler_param(mo_once             , true).
scheduler_param(labeling_options    , [ffc,bisect]). %random_value(376)).
scheduler_param(algorithm_timeout   , 3600).
scheduler_param(delta_solutions		, 10).
scheduler_param(pred_temperature    , 'predictor_temperature.out'). % SRM: NEEDED
scheduler_param(pred_radiation      , 'predictor_radiation.out'). % SRM: NEEDED
scheduler_param(orbit_propagator    , 'orbit_propagator.out'). % SRM: NEEDED


%%  resource_options(+Rname, -Options)
%

resource_options(simultaneity,[cumulative(false)]).
resource_options(storage     ,[cumulative(true)]).


%%  resource_capacity(+Rname, -Capacity)
%

resource_capacity(storage     ,[   6-4, 	% Definir hasta el time_end menos 1 los recursos reales
								  80-7]). 	% A continuación definimos hasta el time_end + max_duración + 1 de las tareas los recursos "infinitos"
resource_capacity(simultaneity,[     1-4,
								  10-7]). % ntasks

                                
                                
                                
                                
                                
                                
% EOF
