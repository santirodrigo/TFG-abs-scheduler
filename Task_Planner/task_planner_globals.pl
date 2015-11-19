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
:- module(task_planner_globals, [
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

scheduler_param(mission_start       , -1000).
scheduler_param(time_start          , 0).
scheduler_param(time_end            , 100).
scheduler_param(domain_similarity   , 0.70).
scheduler_param(minimize_overlapping, false).
scheduler_param(mo_allow_permutation, false).
scheduler_param(mo_max_group_members, 10).
scheduler_param(mo_once             , true).
scheduler_param(labeling_options    , [ffc,bisect]). %random_value(376)).
scheduler_param(algorithm_timeout   , 3600).
scheduler_param(pred_temperature    , 'predictor_temperature.out'). % SRM: NEEDED
scheduler_param(pred_radiation      , 'predictor_radiation.out'). % SRM: NEEDED
scheduler_param(orbit_propagator    , 'orbit_propagator.out'). % SRM: NEEDED


%%  resource_options(+Rname, -Options)
%

resource_options(power       ,[cumulative(false)]).
resource_options(simultaneity,[cumulative(false)]).
resource_options(storage     ,[cumulative(true)]).
resource_options(energy      ,[cumulative(true)]).


%%  resource_capacity(+Rname, -Capacity)
%

resource_capacity(storage     ,[   1000-100]). % 1000 MB
resource_capacity(simultaneity,[     1-100]). % 10 tasks
resource_capacity(power       ,[   1000-100]). % 1 Watt
resource_capacity(energy      ,[2400000-40,    % 2 W/s = 120 mJ/min (starting @ 2400 J)  
                                2520000-41,
                                2640000-42,
                                2760000-43,
                                2880000-44,
                                3000000-45,
                                3120000-46,
                                3240000-47,
                                3360000-48,
                                3480000-49,
                                3600000-50,
                                3720000-51,
                                3840000-52,
                                3960000-53,
                                4080000-54,
                                4200000-55,
                                4320000-56,
                                4440000-57,
                                4560000-58,
                                4680000-59,
                                4800000-100 ]).
                                
                                
                                
                                
                                
                                
% EOF
