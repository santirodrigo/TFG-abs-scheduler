-module(satellite).
-export([start/6, start/7]).
-define(epsylon, 0.00001).
-define(k, 2).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A Satellite is defined by:
%  -  Name: A standard string, kind of an ID
%  -  Schedule: The result of the scheduling distributed process, formed as:
%		[{1,TaskID},
%		 {2,TaskID},
%		 ...,
%		 {N,TaskID}]
%  -  Time0: The beginning timestamp
%  -  CommCost: the cost of migrating the output of one task
%  -  ProcReleaseT: the time at which the task execution at node will finish
%  -  Energy: The satellite energy profile, formed as
%		[{1,Value1},
%        {2,Value2},
%		 ...,
%		 {N,ValueN}]
%
%  -  Cts: algorithm constants -> [A, B, L]
	
%% LEADER START
start(Name, Time0, CommCost, ProcReleaseT, Energy, Cts) ->
	spawn(fun() -> 
		Schedule = tuple_list_from_lists(lists:seq(Time0,max_key(Energy)),lists_repeat(0,max_key(Energy)-Time0+1)),
		waiting_leader(Name, Schedule, Time0, CommCost, ProcReleaseT, Energy, Cts, [])
		end).

%% SLAVE START
start(Name, Time0, CommCost, ProcReleaseT, Energy, Cts, Leader) ->
	spawn(fun() ->
		Self = self(),
		Leader ! {join, Self},
		receive
			{view, NewLeader, PeersAndMe} ->
				Peers = lists:delete(Self, PeersAndMe),
				Schedule = tuple_list_from_lists(lists:seq(Time0,max_key(Energy)),lists_repeat(0,max_key(Energy)-Time0+1)),
				waiting_slave(Name, Schedule, Time0, CommCost, ProcReleaseT, Energy, Cts, NewLeader, Peers)
		end
	end).
	
%% LEADER FUNCTIONS (only differ in Slaves / Peers arguments)
waiting_leader_no_schedule(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Slaves) ->
	receive
        {join, Peer} ->
            NewSlaves = lists:append(Slaves, [Peer]),           
            bcast({view, self(), NewSlaves}, NewSlaves),
            waiting_leader_no_schedule(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, NewSlaves);
        {goodbye, Peer} ->
        	NewSlaves = lists:delete(Peer, Slaves),
        	if 
        		NewSlaves == [] ->
        			say_goodbye(Name, Schedule, Energy);
        		true ->
		        	bcast({view, self(), NewSlaves}, NewSlaves),
		        	waiting_leader_no_schedule(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, NewSlaves)
		    end;        	
        {task, Now, TaskInfo} -> %% Now = timestamp
			if
				Now < Time -> 
					io:format("leader ~s: strange timestamp ~w~n", [Name, Now]),
					waiting_leader_no_schedule(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Slaves);
				true ->
					bcast({task, Now, TaskInfo}, Slaves), %%??
					MaxTime = max_key(Schedule),
					if 
						Now > MaxTime ->
							io:format("leader ~s: invalid time ~w~n", [Name, Now]),
							waiting_leader_no_schedule(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Slaves);
						true ->
							waiting_leader_no_schedule(Name, Schedule, Now, CommCost, ProcReleaseT, Energy, Cts, Slaves)
					end
            end;
        stop ->
            ok;
        Error ->
            io:format("leader ~s: strange message ~w~n", [Name, Error]),
            waiting_leader_no_schedule(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Slaves)
    end.

waiting_leader(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Slaves) ->
	MaxTime = max_key(Schedule),
	if 
		Time > MaxTime ->
			io:format("leader ~s: I can't execute more tasks, but I will be here for whatever you need me~n", [Name]),
			waiting_leader_no_schedule(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Slaves);
		true ->
			io:format("leader ~s: Waiting for messages...~nSchedule = ~w~nTime = ~w~nCommCost = ~w~nProcReleaseT = ~w~nEnergy = ~w~nCts = ~w~nSlaves = ~w~n~n", [Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Slaves])
	end,
	receive
        {join, Peer} ->
            NewSlaves = lists:append(Slaves, [Peer]),           
            bcast({view, self(), NewSlaves}, NewSlaves),
            waiting_leader(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, NewSlaves);
        {goodbye, Peer} ->
        	NewSlaves = lists:delete(Peer, Slaves),
        	if 
        		NewSlaves == [] ->
        			say_goodbye(Name, Schedule, Energy);
        		true ->
		        	bcast({view, self(), NewSlaves}, NewSlaves),
		        	waiting_leader(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, NewSlaves)
		    end;
        {task, Now, TaskInfo} -> %% Now = timestamp
			if
				Now < Time -> 
					io:format("leader ~s: strange timestamp ~w~n", [Name, Now]),
					waiting_leader(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Slaves);
				true ->
					bcast({task, Now, TaskInfo}, Slaves), %%??
            		calculate_bid(Name, Schedule, Time, Now, CommCost, ProcReleaseT, Energy, Cts, Slaves, TaskInfo)
            end;
        stop ->
            ok;
        Error ->
            io:format("leader ~s: strange message ~w~n", [Name, Error]),
            waiting_leader(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Slaves)
    end.

calculate_bid(Name, Schedule, TimePrev, Time, CommCost, ProcReleaseT, Energy, [A,B,L], Slaves, [TID, TaskSize, TaskDeadline]) ->
	Ej = energy_at(max(Time,ProcReleaseT),Energy),
	io:format("leader ~s: Energy at time ~w = ~w~n", [Name, max(Time,ProcReleaseT), Ej]),
	if
		Ej == nothing -> 
			io:format("leader ~s: invalid time ~w~n", [Name, max(Time,ProcReleaseT)]),
			waiting_leader(Name, Schedule, TimePrev, CommCost, ProcReleaseT, Energy, [A,B,L], Slaves); %% Nos quedábamos con el Time incorrecto...
		true -> 
			BPij = TaskSize * (A/(1 - math:exp(Ej/B))),
			DL = TaskDeadline, %%??
			RT = TaskSize + max(Time,ProcReleaseT), %%??
			Lambda = lambda(Time,DL),
			io:format("leader ~s: Lambda ~w~n", [Name, Lambda]),
			Gamma = gamma(Time,RT),
			io:format("leader ~s: Gamma ~w~n", [Name, Gamma]),
			if
				Gamma == 0 -> Pij = infinity, Tw = infinity;
				true ->
					Pij = (CommCost + BPij) * (1 + math:exp(Lambda/Gamma)),
					if
						Pij < 0 -> Tw = 0;
						true ->	Tw = round(Pij*1000) * L
					end,
					io:format("leader ~s: Tw ~w~n", [Name, Tw])
			end,
			listening(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, [A,B,L], Slaves, [TID, TaskSize, TaskDeadline], Pij, Tw)
	end.
	
listening(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Slaves, TaskInfo, Pij, Tw) ->
	receive
		{bid, Sender, Pkj} ->
			if
				Pkj =< Pij ->
					waiting_leader(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Slaves);
				Pkj > Pij ->
					io:format("leader ~s: ~w has sent a worst bid (~w) before me!", [Name, Sender, Pkj]),
					waiting_leader(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Slaves)
    		end;
    	Error ->
        	io:format("leader ~s: strange message ~w~n", [Name, Error]),
        	waiting_leader(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Slaves)
	after
    	Tw ->
        	io:format("leader ~s: I have won with a bid of ~w~n", [Name, Pij]),
        	bcast({bid, Name, Pij}, Slaves),
        	add_task(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Slaves, TaskInfo)
	end.

add_task(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Slaves, TaskInfo) ->
	[NewProcReleaseT, NewEnergy] = refresh_resources(max(Time,ProcReleaseT), Energy, TaskInfo),%% Modificar Energy conforme al consumo de la tarea aceptada
	[TID, TS, _] = TaskInfo,
	NewSchedule = schedule_task(Schedule, max(Time,ProcReleaseT), TID, TS),
	waiting_leader(Name, NewSchedule, Time, CommCost, NewProcReleaseT, NewEnergy, Cts, Slaves).

%% SLAVE FUNCTIONS
waiting_slave(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers) ->
	MaxTime = max_key(Schedule),
	if 
		Time > MaxTime ->
			say_goodbye(Name, Schedule, Energy),
			Leader ! {goodbye, self()};
		true ->
			io:format("slave ~s: Waiting for messages...~nSchedule = ~w~nTime = ~w~nCommCost = ~w~nProcReleaseT = ~w~nEnergy = ~w~nCts = ~w~nLeader = ~w~nPeers = ~w~n~n", [Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers])
	end,
	receive
        {join, Peer} ->
            Leader ! {join, Peer},
            waiting_slave(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers);
        {task, Now, TaskInfo} ->
            if
				Now < Time -> 
					io:format("slave ~s: strange timestamp ~w~n", [Name, Now]),
					waiting_slave(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers);
				true ->
            		calculate_bid(Name, Schedule, Time, Now, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers, TaskInfo)
            end;
        {view, NewLeader, NewPeersAndMe} ->
        	NewPeers = lists:delete(self(),NewPeersAndMe),
        	waiting_slave(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, NewLeader, NewPeers);
        stop ->
            ok;
        Error ->
            io:format("slave ~s: strange message ~w~n", [Name, Error]),
            waiting_slave(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers)
    end.

calculate_bid(Name, Schedule, TimePrev, Time, CommCost, ProcReleaseT, Energy, [A,B,L], Leader, Peers, [TID, TaskSize, TaskDeadline]) ->
	Ej = energy_at(max(Time,ProcReleaseT),Energy),
	io:format("slave ~s: Energy at time ~w = ~w~n", [Name, max(Time,ProcReleaseT), Ej]),
	if
		Ej == nothing -> 
			io:format("slave ~s: invalid time ~w~n", [Name, max(Time,ProcReleaseT)]),
			waiting_slave(Name, Schedule, TimePrev, CommCost, ProcReleaseT, Energy, [A,B,L], Leader, Peers);
		true -> 
			BPij = TaskSize * (A/(1 - math:exp(Ej/B))),
			DL = TaskDeadline, %%??
			RT = TaskSize + max(Time,ProcReleaseT), %%??
			Lambda = lambda(Time,DL),
			io:format("slave ~s: Lambda ~w~n", [Name, Lambda]),
			Gamma = gamma(Time,RT),
			io:format("slave ~s: Gamma ~w~n", [Name, Gamma]),
			if
				Gamma == 0 -> Pij = infinity, Tw = infinity;
				true ->
					Pij = (CommCost + BPij) * (1 + math:exp(Lambda/Gamma)),
					if
						Pij < 0 -> Tw = 0;
						true ->	Tw = round(Pij*1000) * L
					end,
					io:format("slave ~s: Tw ~w~n", [Name, Tw])
			end,
			listening(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, [A,B,L], Leader, Peers, [TID, TaskSize, TaskDeadline], Pij, Tw)
	end.
	
listening(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers, TaskInfo, Pij, Tw) ->
	receive
		{bid, Sender, Pkj} ->
			if
				Pkj =< Pij ->
					waiting_slave(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers);
				Pkj > Pij ->
					io:format("slave ~s: ~w has sent a worst bid (~w) before me!", [Name, Sender, Pkj]),
					waiting_slave(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers)
    		end;
    	Error ->
        	io:format("slave ~s: strange message ~w~n", [Name, Error]),
        	waiting_slave(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers)
	after
    	Tw ->
        	io:format("slave ~s: I have won with a bid of ~w~n", [Name, Pij]),
        	bcast({bid, Name, Pij}, Peers),
        	Leader ! {bid, Name, Pij},
        	add_task(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers, TaskInfo)
	end.

add_task(Name, Schedule, Time, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers, TaskInfo) ->
	[NewProcReleaseT, NewEnergy] = refresh_resources(max(Time,ProcReleaseT), Energy, TaskInfo),%% Modificar Energy conforme al consumo de la tarea aceptada
	[TID,TS,_] = TaskInfo,
	NewSchedule = schedule_task(Schedule, max(Time, ProcReleaseT), TID, TS),
	waiting_slave(Name, NewSchedule, Time, CommCost, NewProcReleaseT, NewEnergy, Cts, Leader, Peers).
    
%% USEFUL FUNCTIONS
bcast(Msg, Nodes) ->
	lists:foreach(fun(Node) -> Node ! Msg end, Nodes).

energy_at(Time,Energy) ->
	find_value(Time,Energy).
	
find_value(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Result} -> Result;
        false -> nothing
    end.
    
refresh_resources(TimeExecBegin, Energy, [_, TaskSize, _]) -> % El primer y tercer parámetros (TaskID, TaskDeadline) no lo usamos
	RT = TaskSize + TimeExecBegin, %%??
	NewEnergy = take_N_to_list(1, Energy, TimeExecBegin, TaskSize, []),
	[RT,NewEnergy].

schedule_task([], _, _, _) -> [];
schedule_task([{T0,Task}|Schedule], TimeExecBegin, TaskID, TaskSize) ->
	if
		(T0 >= TimeExecBegin) and (T0 < TimeExecBegin + TaskSize) ->
			[{T0,TaskID}|schedule_task(Schedule, TimeExecBegin, TaskID, TaskSize)];
		true ->
			[{T0,Task}|schedule_task(Schedule, TimeExecBegin, TaskID, TaskSize)]
	end.

say_goodbye(Name, Schedule, Energy) ->
	io:format("~s: Goodbye! I can't execute more tasks~nSchedule = ~w~nEnergy = ~w~n~n", [Name, Schedule, Energy]),
	file:write_file(io_lib:fwrite("out/~s.out",[Name]), io_lib:fwrite("~s: Goodbye! I can't execute more tasks~nSchedule = ~w~nEnergy = ~w~n~n", [Name, Schedule, Energy])).

take_N_to_list(_, [], _, _, ReturnList) -> ReturnList;
take_N_to_list(N, [{Time,Value}|Energy], TimeExecBegin, TaskSize, ReturnList) ->
	if
		Time < TimeExecBegin ->
			take_N_to_list(N, Energy, TimeExecBegin, TaskSize, lists:append(ReturnList,[{Time,Value}]));
		Time == TimeExecBegin + TaskSize - 1 ->
			NewEnergy = Value - N,
			take_N_to_list(N, Energy, TimeExecBegin, TaskSize, lists:append(ReturnList,[{Time,NewEnergy}]));
		Time < TimeExecBegin + TaskSize ->
			NewEnergy = Value - N,
			take_N_to_list(N + 1, Energy, TimeExecBegin, TaskSize, lists:append(ReturnList,[{Time,NewEnergy}]));
		true ->
			NewEnergy = Value - N,
			take_N_to_list(N, Energy, TimeExecBegin, TaskSize, lists:append(ReturnList,[{Time,NewEnergy}]))
	end.

tuple_list_from_lists([],[]) -> [];
tuple_list_from_lists([Elem1|List1],[Elem2|List2]) ->
	[{Elem1,Elem2}|tuple_list_from_lists(List1,List2)].
	
lists_repeat(_,0) -> [];
lists_repeat(Term,Length) ->
	[Term|lists_repeat(Term,Length-1)].

%% Returns the maximum Key value for a TupleList
max_key([{Key,_}]) -> Key;
max_key([{Key,_}|Tail]) ->
	Max = max_key(Tail),
	if
		Key > Max -> Key;
		true -> Max
	end.

lambda(Time,DL) ->
	if
		Time < DL ->
			?epsylon;
		true ->
			?k * (Time - DL)
	end.
	
gamma(Time,RT) ->
	if
		Time < RT ->
			?epsylon;
		true ->
			Time - RT
	end.
