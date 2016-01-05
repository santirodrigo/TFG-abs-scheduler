-module(market).
-export([start/6, stop/1, random_sphere_point/1]).

start(NSatellites, [Name|Names], Time0, [Position|PosList], [Energy|EnergyList], [C|Cts]) ->
    P = satellite:start(Name, Time0, Position, Energy, C),
    register(Name, P),
    start(NSatellites-1, Names, P, Time0, PosList, EnergyList, Cts).
    
start(0, [], _, _, [], [], []) -> ok;
start(NSatellites, [Name|Names], Leader, Time0, [Position|PosList], [Energy|EnergyList], [C|Cts]) ->
	register(Name, satellite:start(Name, Time0, Position, Energy, C, Leader)),
    start(NSatellites-1, Names, Leader, Time0, PosList, EnergyList, Cts).

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
    
random_sphere_point(Radius) ->
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	X = (random:uniform()-0.5)*Radius*2,
	MaxY = math:sqrt(math:pow(Radius,2)-math:pow(X,2)),
	Y = (random:uniform()-0.5)*MaxY*2,
	UnsignedZ = math:sqrt(math:pow(Radius,2)-math:pow(X,2)-math:pow(Y,2)),
	SignZ = random:uniform()-0.5,
	if
		SignZ < 0 -> Z = -1*UnsignedZ;
		true -> Z = UnsignedZ
	end,
	{X,Y,Z}.
