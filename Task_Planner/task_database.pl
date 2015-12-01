:- module(task_database, [
                            task/1,
                            task_priority/2,
                            task_timing/6,
                            task_positioning/3,
                            task_static_constraints/2,
                            task_dynamic_resources/2
                         ]).

:- discontiguous task/1, task_priority/2, task_timing/6, task_positioning/3, task_static_constraints/2, task_dynamic_resources/2.


%% Tasks generated by task_planner_tester v1.0

task(t1).
task_priority(t1,1).
task_timing(t1, period(0,0,15,0), duration(1), init_delay(0,0,0,0), infinity, 0.000000).
task_positioning(t1, positions([]), 5).
task_static_constraints(t1,resources([
    temperature_range(10,300),
    radiation_range(0,1000)
    ])).
task_dynamic_resources(t1,resources([
    simultaneity([1-1]),
    storage([2-1])
    ])).
    
task(t2).
task_priority(t2,2).
task_timing(t2, period(0,0,15,0), duration(1), init_delay(0,0,0,0), infinity, 0.000000).
task_positioning(t2, positions([]), 5).
task_static_constraints(t2,resources([
    temperature_range(10,300),
    radiation_range(0,1000)
    ])).
task_dynamic_resources(t2,resources([
    simultaneity([1-1]),
    storage([2-1])
    ])).
    
task(t3).
task_priority(t3,2).
task_timing(t3, period(0,0,15,0), duration(1), init_delay(0,0,0,0), infinity, 0.000000).
task_positioning(t3, positions([]), 5).
task_static_constraints(t3,resources([
    temperature_range(10,300),
    radiation_range(0,1000)
    ])).
task_dynamic_resources(t3,resources([
    simultaneity([1-1]),
    storage([10-1])
    ])).
