-module(market_split).
-export([start/9, stop/2]).

% We use the name of the module (i.e. gms3) as the parameter Module to the start procedure. Sleep stands for up to how many milliseconds the workers should wait until the next message is sent.

start(NSatellites, [_|Hosts], [Name|Names], Time0, TimeEnd, [Position|PosList], ProcReleaseT, [Energy|EnergyList], [C|Cts]) ->
    register(market, spawn(fun() ->
    	P = satellite:start(Name, Time0, TimeEnd, Position, ProcReleaseT, Energy, C),
	    register(Name, P),
	    start(NSatellites-1, Hosts, Names, P, Time0, TimeEnd, PosList, ProcReleaseT, EnergyList, Cts) end)).
    
start(0, [], [], _, _, _, [], _, [], []) -> ok;
start(NSatellites, [Host|Hosts], [Name|Names], Leader, Time0, TimeEnd, [Position|PosList], ProcReleaseT, [Energy|EnergyList], [C|Cts]) ->
	spawn(Host, fun() -> group_leader(whereis(user), self()),
				register(Name, satellite:start(Name, Time0, TimeEnd, Position, ProcReleaseT, Energy, C, Leader))
				end),
    start(NSatellites-1, Hosts, Names, Leader, Time0, TimeEnd, PosList, ProcReleaseT, EnergyList, Cts).

stop([], []) -> ok;

stop([Host|Hosts], [Name|Names]) ->
    stop({Name, Host}),
    stop(Hosts, Names).

stop(Name) ->
    if
    	is_tuple(Name) ->
    		Name ! stop;
    	true ->
    		case whereis(Name) of
        		undefined ->
           			ok;
        		Pid ->
            		Pid ! stop
    		end
    end.
