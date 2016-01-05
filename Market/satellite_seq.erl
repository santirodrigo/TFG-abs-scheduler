-module(satellite).
-export([start/7, start/8]).
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
%  -  Time0: The beginning timestamp, as given by erlang:now()
%  -  TimeEnd: The Last timestamp
%  -  Position: a {X,Y,Z} tuple (measured in kilometers)
%		    Obs: we will suppose the position always as pertaining to an sphere 
%				of radius == 7350 km (a viable LEO orbit)
%  -  CommCost: the cost of migrating the output of one task (CommCost = 1 ==> 
%			1000 km)
%  -  ProcReleaseT: the time at which the task execution at node will finish
%  -  Energy: The satellite energy value at the start of the schedule, scalar
%  -  Cts: algorithm constants -> [A, B, L]
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%------------------------------- LEADER START -------------------------------%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(Name, Time0, TimeEnd, Position, ProcReleaseT, Energy, Cts) ->
	spawn(fun() -> 
		Scheduling = yes,
		Schedule = tuple_list_from_lists(lists:seq(Time0,TimeEnd),lists_repeat(0,TimeEnd-Time0+1)),
		EnergyVector = tuple_list_from_lists(lists:seq(Time0,TimeEnd),lists_repeat(Energy,TimeEnd-Time0+1)),
		AssignedTasks = [],
		Slaves = [],
		SlavesPositions = [],
		CommCost = [{self(),0}],
		waiting_leader(Scheduling, Name, Schedule, AssignedTasks, Time0, Position, SlavesPositions, CommCost, ProcReleaseT, EnergyVector, Cts, Slaves)
		end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%------------------------------- SLAVE START -------------------------------%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(Name, Time0, TimeEnd, Position, ProcReleaseT, Energy, Cts, Leader) ->
	spawn(fun() ->
		Self = self(),
		Leader ! {join, Self, Position},
		receive
			{view, NewLeader, PeersWithMeAndPosTuple} ->
				PeersAndPosTuple = lists:keydelete(Self, 1, PeersWithMeAndPosTuple),
				{PeersWithLeader, _} = lists:unzip(PeersAndPosTuple),
				Peers = lists:delete(Leader,PeersWithLeader),
				CommCostWithoutMe = calculate_distance_list(Position, PeersAndPosTuple, []),
				CommCost = lists:append(CommCostWithoutMe,[{self(),0}]),
				Schedule = tuple_list_from_lists(lists:seq(Time0,TimeEnd),lists_repeat(0,TimeEnd-Time0+1)),
				EnergyVector = tuple_list_from_lists(lists:seq(Time0,TimeEnd),lists_repeat(Energy,TimeEnd-Time0+1)),
				AssignedTasks = [],
				waiting_slave(Name, Schedule, AssignedTasks, Time0, Position, CommCost, ProcReleaseT, EnergyVector, Cts, NewLeader, Peers)
		end
	end).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------- LEADER WAITING STATE ---------------------------%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
waiting_leader(Scheduling, Name, Schedule, AssignedTasks, Time, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves) ->
	MaxTime = max_key(Schedule),
	if
		Scheduling == yes ->
			if 
				ProcReleaseT >= MaxTime ->
					io:format("leader ~s: I can't execute more tasks, but I will be here for whatever you need me~n~n", [Name]),
					NewScheduling = no,
					waiting_leader(NewScheduling, Name, Schedule, AssignedTasks, Time, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves);
				true ->
					io:format("leader ~s: Waiting for messages...~nSchedule = ~w~nAssignedTasks = ~w~nTime = ~w~nPosition = ~w~nSlavesPositions = ~w~nCommCost = ~w~nProcReleaseT = ~w~nEnergy = ~w~nCts = ~w~nSlaves = ~w~n~n", [Name, Schedule, AssignedTasks, Time, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves])
			end;
		true -> io:format("leader ~s: I can't execute more tasks, but I will be here for whatever you need me~n~n", [Name])
	end,
	receive
        {join, Peer, SlavePosition} ->
            NewSlaves = lists:append(Slaves, [Peer]),
            NewPositions = lists:append(SlavesPositions,[SlavePosition]),
            NewDistance = calculate_distance(Position, SlavePosition),
            NewCommCost = lists:append(CommCost, [{Peer,NewDistance}]),
            NewSlavePosTuple = tuple_list_from_lists(NewSlaves, NewPositions),           
            bcast({view, self(), lists:append(NewSlavePosTuple,[{self(),Position}])}, NewSlaves),
            waiting_leader(Scheduling, Name, Schedule, AssignedTasks, Time, Position, NewPositions, NewCommCost, ProcReleaseT, Energy, Cts, NewSlaves);
        {goodbye, Peer, SlavePosition} ->
        	NewSlaves = lists:delete(Peer, Slaves),
        	NewPositions = lists:delete(SlavePosition, SlavesPositions),
        	NewCommCost = lists:keydelete(Peer, 1, CommCost),
        	NewSlavePosTuple = tuple_list_from_lists(NewSlaves, NewPositions),
        	if 
        		NewSlaves == [] ->
        			say_goodbye(Name, Schedule, Energy);
        		true ->
		        	bcast({view, self(), lists:append(NewSlavePosTuple,[{self(),Position}])}, NewSlaves),
		        	waiting_leader(Scheduling, Name, Schedule, AssignedTasks, Time, Position, NewPositions, NewCommCost, ProcReleaseT, Energy, Cts, NewSlaves)
		    end;
        {task, Now, TaskInfo} -> %% Now = timestamp
			if
				Now < Time -> 
					io:format("leader ~s: strange timestamp ~w~n", [Name, Now]),
					waiting_leader(Scheduling, Name, Schedule, AssignedTasks, Time, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves);
				true ->
					bcast({task, Now, TaskInfo}, Slaves), %%??
					MaxTime = max_key(Schedule),
					if 
						Now > MaxTime ->
							io:format("leader ~s: invalid time ~w~n", [Name, Now]),
							waiting_leader(Scheduling, Name, Schedule, AssignedTasks, Time, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves);
						true ->
							if
								Scheduling == yes ->
									listening_leader(Name, Schedule, AssignedTasks, Time, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves, TaskInfo);
								true ->
									waiting_leader(Scheduling, Name, Schedule, AssignedTasks, Now, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves)
							end
					end
            end;
        {bid, Sender, TaskID, _} ->
        	if
		    	Scheduling == no ->
		    		NewAssignedTasks = lists:append(AssignedTasks, [{TaskID,Sender}]),
			    	waiting_leader(Scheduling, Name, Schedule, NewAssignedTasks, Time, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves);
	        	true ->
	        		waiting_leader(Scheduling, Name, Schedule, AssignedTasks, Time, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves)
	        end;
        stop ->
            ok;
        Error ->
            io:format("leader ~s: strange message ~w~n", [Name, Error]),
            waiting_leader(yes, Name, Schedule, AssignedTasks, Time, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves)
    end.
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%-------------------------- LEADER LISTENING STATE --------------------------%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
listening_leader(Name, Schedule, AssignedTasks, Time, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves, TaskInfo) ->
	[TaskID, TaskSize, TaskDeadline, PrevTask] = TaskInfo,
	{Pij, Tw} = calculate_bid(Name, AssignedTasks, Time, CommCost, ProcReleaseT, Energy, Cts, TaskSize, TaskDeadline, PrevTask),

	receive
		{bid, Sender, TaskID, Pkj} ->
			if
				Pkj =< Pij ->
					NewAssignedTasks = lists:append(AssignedTasks,[{TaskID,Sender}]),
					waiting_leader(yes, Name, Schedule, NewAssignedTasks, Time, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves);
				Pkj > Pij ->
					io:format("leader ~s: ~w has sent a worst bid (~w) before me!", [Name, Sender, Pkj]),
					NewAssignedTasks = lists:append(AssignedTasks,[{TaskID,Sender}]),
					waiting_leader(yes, Name, Schedule, NewAssignedTasks, Time, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves)
			end
	after
    	Tw ->
        	io:format("leader ~s: I have won with a bid of ~w~n", [Name, Pij]),
        	bcast({bid, self(), TaskID, Pij}, Slaves),
        	NewAssignedTasks = lists:append(AssignedTasks,[{TaskID,self()}]),
        	{NewProcReleaseT, NewEnergy, NewSchedule} = add_task(Schedule, Time, ProcReleaseT, Energy, TaskInfo),
        	waiting_leader(yes, Name, NewSchedule, NewAssignedTasks, Time, Position, SlavesPositions, CommCost, NewProcReleaseT, NewEnergy, Cts, Slaves)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%---------------------------- SLAVE WAITING STATE ---------------------------%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
waiting_slave(Name, Schedule, AssignedTasks, Time, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers) ->
	MaxTime = max_key(Schedule),
	if 
		ProcReleaseT >= MaxTime ->
			say_goodbye(Name, Schedule, Energy),
			Leader ! {goodbye, self(), Position},
			self() ! stop;
		true ->
			io:format("slave ~s: Waiting for messages...~nSchedule = ~w~nAssignedTasks = ~w~nTime = ~w~nPosition = ~w~nCommCost = ~w~nProcReleaseT = ~w~nEnergy = ~w~nCts = ~w~nLeader = ~w~nPeers = ~w~n~n", [Name, Schedule, AssignedTasks, Time, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers])
	end,
	receive
        {join, Peer, PeerPosition} ->
            Leader ! {join, Peer, PeerPosition},
            waiting_slave(Name, Schedule, AssignedTasks, Time, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers);
        {task, Now, TaskInfo} ->
			if
				Now < Time -> 
					io:format("slave ~s: strange timestamp ~w~n", [Name, Now]),
					waiting_slave(Name, Schedule, AssignedTasks, Time, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers);
				true ->
					MaxTime = max_key(Schedule),
					if 
						Now > MaxTime ->
							io:format("slave ~s: invalid time ~w~n", [Name, Now]),
							waiting_slave(Name, Schedule, AssignedTasks, Time, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers);
						true ->
							listening_slave(Name, Schedule, AssignedTasks, Time, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers, TaskInfo)
					end
            end;
        {view, NewLeader, SlavesWithMeAndPosTuple} ->
			SlavesAndPosTuple = lists:keydelete(self(), 1, SlavesWithMeAndPosTuple),
			{NewPeersWithLeader, _} = lists:unzip(SlavesAndPosTuple),
			NewPeers = lists:delete(Leader,NewPeersWithLeader),
			CommCostWithoutMe = calculate_distance_list(Position, SlavesAndPosTuple, CommCost),
			NewCommCost = lists:append(CommCostWithoutMe,[{self(),0}]),
			waiting_slave(Name, Schedule, AssignedTasks, Time, Position, NewCommCost, ProcReleaseT, Energy, Cts, NewLeader, NewPeers);
        stop ->
            ok;
        Error ->
            io:format("slave ~s: strange message ~w~n", [Name, Error]),
            waiting_slave(Name, Schedule, AssignedTasks, Time, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------- SLAVE LISTENING STATE --------------------------%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
listening_slave(Name, Schedule, AssignedTasks, Time, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers, TaskInfo) ->
	[TaskID, TaskSize, TaskDeadline, PrevTask] = TaskInfo,
	{Pij, Tw} = calculate_bid(Name, AssignedTasks, Time, CommCost, ProcReleaseT, Energy, Cts, TaskSize, TaskDeadline, PrevTask),
	receive
		{bid, Sender, TaskID, Pkj} ->
			if
				Pkj =< Pij ->
					NewAssignedTasks = lists:append(AssignedTasks,[{TaskID,Sender}]),
					waiting_slave(Name, Schedule, NewAssignedTasks, Time, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers);
				Pkj > Pij ->
					io:format("slave ~s: ~w has sent a worst bid (~w) before me!", [Name, Sender, Pkj]),
					NewAssignedTasks = lists:append(AssignedTasks,[{TaskID,Sender}]),
					waiting_slave(Name, Schedule, NewAssignedTasks, Time, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers)
			end
	after
    	Tw ->
        	io:format("slave ~s: I have won with a bid of ~w~n", [Name, Pij]),
        	bcast({bid, self(), TaskID, Pij}, Peers),
        	Leader ! {bid, self(), TaskID, Pij},
        	NewAssignedTasks = lists:append(AssignedTasks,[{TaskID,self()}]),
        	{NewProcReleaseT, NewEnergy, NewSchedule} = add_task(Schedule, Time, ProcReleaseT, Energy, TaskInfo),
        	waiting_slave(Name, NewSchedule, NewAssignedTasks, Time, Position, CommCost, NewProcReleaseT, NewEnergy, Cts, Leader, Peers)
	end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%------------------------------- CALCULATE BID ------------------------------%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calculate_bid(Name, AssignedTasks, Time, CommCost, ProcReleaseT, Energy, [A,B,L], TaskSize, TaskDeadline, PrevTask) ->
	Ej = energy_at(max(Time,ProcReleaseT),Energy),
	io:format("~s: Energy at time ~w = ~w~n", [Name, max(Time,ProcReleaseT), Ej]),
	if
		(Ej == 0) or (Ej < TaskSize) ->
			BPij = 1 + A;
		true ->
			BPij = (1 + A*math:exp(-1*math:pow((1-TaskSize/Ej),2)/B)) %% B >>, A "innecesaria", (1 + )?? --> Es una gaussiana
	end,
	io:format("~s: BPij= ~w~n", [Name, BPij]),
	DL = TaskDeadline, %%??
	RT = TaskSize + max(Time,ProcReleaseT), %%??
	Lambda = lambda(Time,DL),
	io:format("~s: Lambda ~w~n", [Name, Lambda]),
	Gamma = gamma(Time,RT),
	io:format("~s: Gamma ~w~n", [Name, Gamma]),
	if
		Gamma == 0 -> Pij = infinity, Tw = infinity;
		true ->
			CC = find_value(find_value(PrevTask,AssignedTasks),CommCost),
			if
				CC == nothing -> CCij = 0;
				true -> CCij = CC
			end,
			Pij = (CCij + BPij),% * (1 + math:exp(Lambda/Gamma)),
			if
				Pij < 0 -> Tw = 0;
				true ->	Tw = round(Pij*1000) * L
			end,
			io:format("~s: Tw ~w~n", [Name, Tw])
	end,
	{Pij, Tw}.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%-------------------------- BROADCAST MSG TO NODES --------------------------%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
bcast(Msg, Nodes) ->
	lists:foreach(fun(Node) -> Node ! Msg end, Nodes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%------------------------------ ENERGY AT TIME ------------------------------%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
energy_at(Time,Energy) ->
	find_value(Time,Energy).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%------------------------- FIND VALUE OF A GIVEN KEY ------------------------%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
find_value(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Result} -> Result;
        false -> nothing
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------- ADD TASK TO SCHEDULE ---------------------------%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_task(Schedule, Time, ProcReleaseT, Energy, TaskInfo) ->
	[NewProcReleaseT, NewEnergy] = refresh_resources(max(Time,ProcReleaseT), Energy, TaskInfo),%% Modificar Energy conforme al consumo de la tarea aceptada
	[TID, TS, _, _] = TaskInfo,
	NewSchedule = schedule_task(Schedule, max(Time,ProcReleaseT), TID, TS),
	{NewProcReleaseT, NewEnergy, NewSchedule}.
	    
refresh_resources(TimeExecBegin, Energy, [_, TaskSize, _, _]) -> % El primer y tercer parámetros (TaskID, TaskDeadline) no lo usamos
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
	
say_goodbye(Name, Schedule, Energy) ->
	io:format("~s: Goodbye! I can't execute more tasks~nSchedule = ~w~nEnergy = ~w~n~n", [Name, Schedule, Energy]),
	file:write_file(io_lib:fwrite("out/~s.out",[Name]), io_lib:fwrite("~s: Goodbye! I can't execute more tasks~nSchedule = ~w~nEnergy = ~w~n~n", [Name, Schedule, Energy])).

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
	
calculate_distance(MyPos, OtherPos) ->
	{X1,Y1,Z1} = MyPos,
	{X2,Y2,Z2} = OtherPos,
	math:sqrt(math:pow((X1-X2),2)+math:pow((Y1-Y2),2)+math:pow((Z1-Z2),2)).
	
calculate_distance_list(_, [], _) -> [];
calculate_distance_list(Position, [{Slave,Pos}|SlavesAndPosTuple], PrevCommCost) ->
	CommCost = calculate_distance_list(Position,SlavesAndPosTuple,PrevCommCost),
	IsAlready = find_value(Slave, PrevCommCost), %% Suponemos que nunca varía la posición de los satélites
	if
		IsAlready == nothing ->
			NewPos = calculate_distance(Position, Pos),
			NewCommCost = lists:append(CommCost,[{Slave,NewPos}]);
		true ->
			NewCommCost = lists:append(CommCost,[lists:keyfind(Slave,1,PrevCommCost)])
	end,
		NewCommCost.
