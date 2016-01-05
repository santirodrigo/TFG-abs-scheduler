-module(market).
-export([start/8, stop/1]).

start(NSatellites, [Name|Names], Time0, TimeEnd, [Position|PosList], ProcReleaseT, [Energy|EnergyList], [C|Cts]) ->
    P = satellite:start(Name, Time0, TimeEnd, Position, ProcReleaseT, Energy, C),
    register(Name, P),
    start(NSatellites-1, Names, P, Time0, TimeEnd, PosList, ProcReleaseT, EnergyList, Cts).
    
start(0, [], _, _, _, [], _, [], []) -> ok;
start(NSatellites, [Name|Names], Leader, Time0, TimeEnd, [Position|PosList], ProcReleaseT, [Energy|EnergyList], [C|Cts]) ->
	register(Name, satellite:start(Name, Time0, TimeEnd, Position, ProcReleaseT, Energy, C, Leader)),
    start(NSatellites-1, Names, Leader, Time0, TimeEnd, PosList, ProcReleaseT, EnergyList, Cts).

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

