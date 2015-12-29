-module(market).
-export([start/7, stop/1]).

start(NSatellites, [Name|Names], Time0, CommCost, ProcReleaseT, [Resources|ResList], [C|Cts]) ->
    P = satellite:start(Name, Time0, CommCost, ProcReleaseT, Resources, C),
    register(Name, P),
    start(NSatellites-1, Names, P, Time0, CommCost, ProcReleaseT, ResList, Cts).
    
start(0, [], _, _, _, _, [], []) -> ok;
start(NSatellites, [Name|Names], Leader, Time0, CommCost, ProcReleaseT, [Resources|ResList], [C|Cts]) ->
	register(Name, satellite:start(Name, Time0, CommCost, ProcReleaseT, Resources, C, Leader)),
    start(NSatellites-1, Names, Leader, Time0, CommCost, ProcReleaseT, ResList, Cts).

stop([]) -> ok;

stop([Name|Names]) ->
    stop(Name),
    stop(Names);

stop(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop
    end.

