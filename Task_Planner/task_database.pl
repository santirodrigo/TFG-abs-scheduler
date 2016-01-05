:- module(task_database, [
                            task/1,
                            task_priority/2,
                            task_timing/7,
                            task_positioning/3,
                            task_static_constraints/2,
                            task_dynamic_resources/2
                         ]).

:- discontiguous task/1, task_priority/2, task_timing/6, task_positioning/3, task_static_constraints/2, task_dynamic_resources/2.


%% Tasks generated by task_planner_tester v1.0

task(1).
task_priority(1,1).
task_timing(1, period(0,0,15,0), duration(1), init_delay(2), infinity, 0.000000, deadline(5)).
%% Añadir task_deadline
task_positioning(1, positions([]), 5).
task_static_constraints(1,resources([
    temperature_range(10,300),
    radiation_range(0,1000)
    ])).
task_dynamic_resources(1,resources([
    simultaneity([1-1]),
    storage([2-1])
    ])).
    
task(2).
task_priority(2,2).
task_timing(2, period(0,0,15,0), duration(1), init_delay(0), infinity, 0.000000, deadline(10)).
task_positioning(2, positions([]), 5).
task_static_constraints(2,resources([
    temperature_range(10,300),
    radiation_range(0,1000)
    ])).
task_dynamic_resources(2,resources([
    simultaneity([1-1]),
    storage([2-1])
    ])).
    
task(3).
task_priority(3,2).
task_timing(3, period(0,0,15,0), duration(1), init_delay(3), infinity, 0.000000, deadline(5)).
task_positioning(3, positions([]), 5).
task_static_constraints(3,resources([
    temperature_range(10,300),
    radiation_range(0,1000)
    ])).
task_dynamic_resources(3,resources([
    camera([1-1]),
    storage([10-1])
    ])).
