
:- [task_planner].
:- [task_planner_globals].
:- [task_planner_prepare].
:- [task_database].
:- [load].
:- [stat].


:- doc_server(4000).    % Start PlDoc at port 4000
:- portray_text(true).  % Enable portray of strings
:- doc_save(.,[recursive(true),title('Task Planner Documentation')]).
