-module(satellite).
-export([start/5, start/6]).
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
start(Name, Time0, Position, Energy, Cts) ->
	spawn(fun() -> 
		{A1,A2,A3} = now(),
		random:seed(A1, A2, A3),
		Scheduling = yes,
		Schedule = [],
		EnergyVector = [{0,Energy}],
		AssignedTasks = [],
		Slaves = [],
		SlavesPositions = [],
		CommCost = [{self(),0}],
		ProcReleaseT = 0,
		TimeElapsed = 0,
		waiting_leader(Scheduling, Name, Time0, TimeElapsed, Schedule, AssignedTasks, Position, SlavesPositions, CommCost, ProcReleaseT, EnergyVector, Cts, Slaves)
		end).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%------------------------------- SLAVE START -------------------------------%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start(Name, Time0, Position, Energy, Cts, Leader) ->
	spawn(fun() ->
		{A1,A2,A3} = now(),
		random:seed(A1, A2, A3),
		Self = self(),
		Leader ! {join, Self, Position},
		receive
			{view, NewLeader, PeersWithMeAndPosTuple} ->
				PeersAndPosTuple = lists:keydelete(Self, 1, PeersWithMeAndPosTuple),
				{PeersWithLeader, _} = lists:unzip(PeersAndPosTuple),
				Peers = lists:delete(Leader,PeersWithLeader),
				CommCostWithoutMe = calculate_distance_list(Position, PeersAndPosTuple, []),
				CommCost = lists:append(CommCostWithoutMe,[{self(),0}]),
				Schedule = [],
				EnergyVector = [{0,Energy}],
				AssignedTasks = [],
				ProcReleaseT = 0,
				waiting_slave(Name, Time0, Schedule, AssignedTasks, Position, CommCost, ProcReleaseT, EnergyVector, Cts, NewLeader, Peers)
		end
	end).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------- LEADER WAITING STATE ---------------------------%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
waiting_leader(Scheduling, Name, Time0, TimeElapsed, Schedule, AssignedTasks, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves) ->
	if
		Scheduling == yes ->
			Ej = energy_at(max_key(Energy),Energy),
			if 
				Ej =< 0 ->
					%io:format("leader ~s: I can't execute more tasks, but I will be here for whatever you need me~n~n", [Name]),
					NewScheduling = no,
					waiting_leader(NewScheduling, Name, Time0, TimeElapsed, Schedule, AssignedTasks, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves);
				true ->
					ok,
					say_goodbye_extended(Name, AssignedTasks, TimeElapsed)
					%io:format("leader ~s: Waiting for messages...~nSchedule = ~w~nAssignedTasks = ~w~nPosition = ~w~nSlavesPositions = ~w~nCommCost = ~w~nProcReleaseT = ~w~nEnergy = ~w~nCts = ~w~nSlaves = ~w~nTimeElapsed = ~w~n~n", [Name, Schedule, AssignedTasks, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves, TimeElapsed])
			end;
		true -> ok %io:format("leader ~s: I can't execute more tasks, but I will be here for whatever you need me~n~n", [Name])
	end,
	receive
        {join, Peer, SlavePosition} ->
            NewSlaves = lists:append(Slaves, [Peer]),
            NewPositions = lists:append(SlavesPositions,[SlavePosition]),
            NewDistance = calculate_distance(Position, SlavePosition),
            NewCommCost = lists:append(CommCost, [{Peer,NewDistance}]),
            NewSlavePosTuple = tuple_list_from_lists(NewSlaves, NewPositions),           
            bcast({view, self(), lists:append(NewSlavePosTuple,[{self(),Position}])}, NewSlaves),
            waiting_leader(Scheduling, Name, Time0, TimeElapsed, Schedule, AssignedTasks, Position, NewPositions, NewCommCost, ProcReleaseT, Energy, Cts, NewSlaves);
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
		        	waiting_leader(Scheduling, Name, Time0, TimeElapsed, Schedule, AssignedTasks, Position, NewPositions, NewCommCost, ProcReleaseT, Energy, Cts, NewSlaves)
		    end;
        {task, TimeStamp, TaskInfo} ->
			Diff = (timer:now_diff(now(), TimeStamp))/1000,
			if
				Diff < 0 -> 
					io:format("leader ~s: strange TimeStamp ~w~n", [Name, TimeStamp]),
					waiting_leader(Scheduling, Name, Time0, TimeElapsed, Schedule, AssignedTasks, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves);
				true ->
					bcast({task, TimeStamp, TaskInfo}, Slaves),
					io:format("leader ~s: Scheduling = ~w~n", [Name, Scheduling]),
					if
						Scheduling == yes ->
							TimeDiff = (timer:now_diff(TimeStamp,Time0))/1000,
							listening_leader(Name, Time0, TimeElapsed, Schedule, AssignedTasks, TimeDiff, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves, TaskInfo);
						true ->
							waiting_leader(Scheduling, Name, Time0, TimeElapsed, Schedule, AssignedTasks, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves)
					end
            end;
        {bid, Sender, TaskID, _} ->
        	if
		    	Scheduling == no ->
		    		NewAssignedTasks = lists:append(AssignedTasks, [{TaskID,Sender}]),
			    	waiting_leader(Scheduling, Name, Time0, TimeElapsed, Schedule, NewAssignedTasks, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves);
	        	true ->
	        		waiting_leader(Scheduling, Name, Time0, TimeElapsed, Schedule, AssignedTasks, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves)
	        end;
        stop ->
            io:format("leader ~s: stopping...~n", [Name]),
            say_goodbye(Name, Schedule, Energy),
            say_goodbye_extended(Name, AssignedTasks, TimeElapsed);
        Error ->
            io:format("leader ~s: strange message ~w~n", [Name, Error]),
            waiting_leader(yes, Name, Time0, TimeElapsed, Schedule, AssignedTasks, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves)
    end.
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%-------------------------- LEADER LISTENING STATE --------------------------%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%		
listening_leader(Name, Time0, TimeElapsed, Schedule, AssignedTasks, TimeDiff, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves, TaskInfo) ->
	Time = now(),
	[TaskID, TaskSize, TaskDeadline, PrevTask] = TaskInfo,
	{Pij, Tw} = calculate_bid(Name, AssignedTasks, TimeDiff, Time0, CommCost, ProcReleaseT, Energy, Cts, TaskSize, TaskDeadline, PrevTask),

	receive
		{bid, Sender, TaskID, Pkj} ->
			NewTime = (timer:now_diff(now(), Time)) / 1000000,
    		NewTimeElapsed = TimeElapsed + NewTime,
			if
				Pkj =< Pij ->
					NewAssignedTasks = lists:append(AssignedTasks,[{TaskID,Sender}]),
					waiting_leader(yes, Name, Time0, NewTimeElapsed, Schedule, NewAssignedTasks, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves);
				Pkj > Pij ->
					io:format("leader ~s: ~w has sent a worst bid (~w) before me!", [Name, Sender, Pkj]),
					NewAssignedTasks = lists:append(AssignedTasks,[{TaskID,Sender}]),
					waiting_leader(yes, Name, Time0, NewTimeElapsed, Schedule, NewAssignedTasks, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves)
			end
	after
    	Tw ->
    		NewTime = (timer:now_diff(now(), Time)) / 1000000,
    		NewTimeElapsed = TimeElapsed + NewTime,
    		if
    			Pij == infinity ->
    				waiting_leader(yes, Name, Time0, NewTimeElapsed, Schedule, AssignedTasks, Position, SlavesPositions, CommCost, ProcReleaseT, Energy, Cts, Slaves);
				true ->
					io:format("leader ~s: I have won with a bid of ~w~n", [Name, Pij]),
					bcast({bid, self(), TaskID, Pij}, Slaves),
					NewAssignedTasks = lists:append(AssignedTasks,[{TaskID,self()}]),
					{NewProcReleaseT, NewEnergy, NewSchedule} = add_task(Schedule, TimeDiff, ProcReleaseT, Energy, TaskInfo),
					waiting_leader(yes, Name, Time0, NewTimeElapsed, NewSchedule, NewAssignedTasks, Position, SlavesPositions, CommCost, NewProcReleaseT, NewEnergy, Cts, Slaves)
			end
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%---------------------------- SLAVE WAITING STATE ---------------------------%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
waiting_slave(Name, Time0, Schedule, AssignedTasks, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers) ->
	Ej = energy_at(max_key(Energy),Energy),
	if 
		Ej =< 0 ->
			say_goodbye(Name, Schedule, Energy),
			Leader ! {goodbye, self(), Position},
			self() ! stop;
		true ->
			ok
			%io:format("slave ~s: Waiting for messages...~nSchedule = ~w~nAssignedTasks = ~w~nPosition = ~w~nCommCost = ~w~nProcReleaseT = ~w~nEnergy = ~w~nCts = ~w~nLeader = ~w~nPeers = ~w~n~n", [Name, Schedule, AssignedTasks, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers])
	end,
	receive
        {join, Peer, PeerPosition} ->
            Leader ! {join, Peer, PeerPosition},
            waiting_slave(Name, Time0, Schedule, AssignedTasks, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers);
        {task, TimeStamp, TaskInfo} ->
        	Diff = (timer:now_diff(now(), TimeStamp))/1000,
			if
				Diff < 0 -> 
%					TimeDiff = (timer:now_diff(TimeStamp,Time0))/1000,
%					listening_slave(Name, Time0, Schedule, AssignedTasks, TimeDiff, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers, TaskInfo),
					io:format("slave ~s: strange TimeStamp ~w~n", [Name, TimeStamp]),
					waiting_slave(Name, Time0, Schedule, AssignedTasks, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers);
				true ->
					TimeDiff = (timer:now_diff(TimeStamp,Time0))/1000,
					listening_slave(Name, Time0, Schedule, AssignedTasks, TimeDiff, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers, TaskInfo)
            end;
        {view, NewLeader, SlavesWithMeAndPosTuple} ->
			SlavesAndPosTuple = lists:keydelete(self(), 1, SlavesWithMeAndPosTuple),
			{NewPeersWithLeader, _} = lists:unzip(SlavesAndPosTuple),
			NewPeers = lists:delete(Leader,NewPeersWithLeader),
			CommCostWithoutMe = calculate_distance_list(Position, SlavesAndPosTuple, CommCost),
			NewCommCost = lists:append(CommCostWithoutMe,[{self(),0}]),
			waiting_slave(Name, Time0, Schedule, AssignedTasks, Position, NewCommCost, ProcReleaseT, Energy, Cts, NewLeader, NewPeers);
        stop ->
            io:format("slave ~s: stopping...~n", [Name]),
            say_goodbye(Name, Schedule, Energy);
        Error ->
            io:format("slave ~s: strange message ~w~n", [Name, Error]),
            waiting_slave(Name, Time0, Schedule, AssignedTasks, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------- SLAVE LISTENING STATE --------------------------%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
listening_slave(Name, Time0, Schedule, AssignedTasks, TimeDiff, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers, TaskInfo) ->
	[TaskID, TaskSize, TaskDeadline, PrevTask] = TaskInfo,
	{Pij, Tw} = calculate_bid(Name, AssignedTasks, TimeDiff, Time0, CommCost, ProcReleaseT, Energy, Cts, TaskSize, TaskDeadline, PrevTask),
	receive
		{bid, Sender, TaskID, Pkj} ->
			if
				Pkj =< Pij ->
					NewAssignedTasks = lists:append(AssignedTasks,[{TaskID,Sender}]),
					waiting_slave(Name, Time0, Schedule, NewAssignedTasks, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers);
				Pkj > Pij ->
					io:format("slave ~s: ~w has sent a worst bid (~w) before me!", [Name, Sender, Pkj]),
					NewAssignedTasks = lists:append(AssignedTasks,[{TaskID,Sender}]),
					waiting_slave(Name, Time0, Schedule, NewAssignedTasks, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers)
			end
	after
    	Tw ->
        	if
        	Pij == infinity ->
        		waiting_slave(Name, Time0, Schedule, AssignedTasks, Position, CommCost, ProcReleaseT, Energy, Cts, Leader, Peers);
        	true ->
		    	io:format("slave ~s: I have won with a bid of ~w~n", [Name, Pij]),
		    	bcast({bid, self(), TaskID, Pij}, Peers),
		    	Leader ! {bid, self(), TaskID, Pij},
		    	NewAssignedTasks = lists:append(AssignedTasks,[{TaskID,self()}]),
		    	{NewProcReleaseT, NewEnergy, NewSchedule} = add_task(Schedule, (timer:now_diff(now(),Time0))/1000, ProcReleaseT, Energy, TaskInfo),
		    	waiting_slave(Name, Time0, NewSchedule, NewAssignedTasks, Position, CommCost, NewProcReleaseT, NewEnergy, Cts, Leader, Peers)
		    end
	end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%------------------------------- CALCULATE BID ------------------------------%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
calculate_bid(Name, AssignedTasks, TimeDiff, Time0, CommCost, ProcReleaseT, Energy, [A,B,L], TaskSize, TaskDeadline, PrevTask) ->
	Ej = energy_at(max_key(Energy),Energy),
	io:format("~s: Energy remaining = ~w~n", [Name, Ej]),
	if
		Ej < TaskSize ->
			BPij = infinity;
		true ->
			BPij = (A*math:exp(-1*math:pow((1-TaskSize/Ej),2)/B)) %% B >>, A "innecesaria", acotado entre 0 y A
	end,
	io:format("~s: BPij= ~w~n", [Name, BPij]),
	DL = TaskDeadline + TimeDiff, %% El TaskDeadline es relativo al tiempo de inicio de la tarea, y está en ms
	RT = TaskSize + max(timer:now_diff(now(),Time0)/1000,ProcReleaseT),
	Timing = calculate_timing(DL, RT),
	io:format("~s: DL ~w, RT ~w, Timing ~w~n", [Name, DL, RT, Timing]),
	CCmax = 14700, %% En una órbita LEO de 7350 km, podemos suponer que dos satélites nunca estarán más lejos de 14700 km
	if
		BPij == infinity -> Pij = infinity, Tw = (2 + A)*1000 + 100 + 100; %% Tw = Tw_max + max_random= 1 + A
		Timing == infinity -> Pij = infinity, Tw = (2 + A)*1000 + 100 + 100;
		true ->
			CC = find_value(find_value(PrevTask,AssignedTasks),CommCost),
			if
				CC == nothing -> CCij = 0;
				true -> CCij = CC
			end,
			io:format("~s: CCij ~w~n", [Name, CCij]),
			Pij = (CCij/CCmax + BPij + Timing),
			Tw = round(Pij*1000 + random:uniform()*100) * L
	end,
	io:format("~s: Tw ~w~n", [Name, Tw]),
	{Pij, Tw}.

calculate_timing(DL, RT) ->
	if
		DL > RT ->
			(1/(DL - RT + 1));
		DL == RT ->
			1;
		true ->
			infinity
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
        {_, Result} -> Result;
        false -> nothing
    end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%--------------------------- ADD TASK TO SCHEDULE ---------------------------%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_task(Schedule, Time, ProcReleaseT, Energy, TaskInfo) ->
	[NewProcReleaseT, NewEnergy] = refresh_resources(max(Time,ProcReleaseT), Energy, TaskInfo),%% Modificar Energy conforme al consumo de la tarea aceptada
	[TID, _, _, _] = TaskInfo,
	NewSchedule = lists:append(Schedule, [{max(Time,ProcReleaseT), TID}]),
	{NewProcReleaseT, NewEnergy, NewSchedule}.
	    
refresh_resources(TimeExecBegin, Energy, [_, TaskSize, _, _]) -> % El primer y tercer parámetros (TaskID, TaskDeadline) no lo usamos
	RT = TaskSize + TimeExecBegin,
	Ej = energy_at(max_key(Energy),Energy),
	NewEnergy = lists:append(Energy, [{TimeExecBegin, Ej-TaskSize}]),
	[RT,NewEnergy].
	
say_goodbye(Name, Schedule, Energy) ->
	%io:format("~s: Goodbye! I can't execute more tasks~nSchedule = ~w~nEnergy = ~w~n~n", [Name, Schedule, Energy]),
	file:write_file(io_lib:fwrite("out/~s.out",[Name]), io_lib:fwrite("~s: Goodbye! I can't execute more tasks~nSchedule = ~w~nEnergy = ~w~n~n", [Name, Schedule, Energy])).
	
say_goodbye_extended(Name, AssignedTasks, TimeElapsed) ->
	file:write_file("out/results.out", io_lib:fwrite("~s: Test results~nAssignedTasks = ~w~n~n", [Name, AssignedTasks])),
	L = length(AssignedTasks),
	file:write_file("out/timing.out", io_lib:fwrite("~p-~p", [TimeElapsed, L])).	

tuple_list_from_lists([],[]) -> [];
tuple_list_from_lists([Elem1|List1],[Elem2|List2]) ->
	[{Elem1,Elem2}|tuple_list_from_lists(List1,List2)].

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
