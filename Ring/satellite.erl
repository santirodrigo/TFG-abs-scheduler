-module(satellite).
-export([start/5]).

%% LEADER START
start(ring_builder, Name, Time0, Energy, RingLength) ->
	spawn(fun() -> 
		LocalSchedule = tuple_list_from_lists(lists:seq(Time0,max_key(Energy)),lists_repeat(0,max_key(Energy)-Time0+1)),
		build_ring(Name, LocalSchedule, Time0, Energy, nil, [self()], RingLength)
		end);

%% SLAVE START
start(ring_node, Name, Time0, Energy, Leader) ->
	spawn(fun() ->
		Self = self(),
		Leader ! {join, Self},
		receive
			{successor, Successor} ->
				LocalSchedule = tuple_list_from_lists(lists:seq(Time0,max_key(Energy)),lists_repeat(0,max_key(Energy)-Time0+1)),
				waiting(Name, LocalSchedule, 0, Energy, Successor)
		end
	end).
	
build_ring(Name, LocalSchedule, Time, Energy, Successor, RingNodes, RingLength) ->
	receive
		{join, NewNode} ->
			NewRingNodes = lists:append(RingNodes,[NewNode]),
			LastNode = lists:last(RingNodes),
			if
				LastNode == self() -> NewSuccessor = NewNode;
				true -> 
					NewSuccessor = Successor,
					LastNode ! {successor, NewNode}
			end,
			if
				RingLength == length(NewRingNodes) ->
					NewNode ! {successor, self()},
					waiting(Name, LocalSchedule, Time, Energy, NewSuccessor);		
				true -> build_ring(Name, LocalSchedule, Time, Energy, NewSuccessor, RingNodes, RingLength)
			end
	end.
	
waiting(Name, LocalSchedule, LastSeen, Energy, Successor) -> %% Es útil el parámetro Time? Y Energy?
	receive
		{schedule, GlobalSchedule} ->
			if
				LastSeen == 0 ->
					[NewGlobalSchedule, NewLocalSchedule] = vote_schedule(GlobalSchedule); % "Vota" las tareas que incluya su solución local con la nota correspondiente
				true ->
					[NewGlobalSchedule, NewLocalSchedule] = set_schedule(GlobalSchedule, LocalSchedule) % Borra sus votos en las tareas donde "ha perdido" y reafirma en su schedule las demás
			end,
			NewLastSeen = LastSeen + 1,
			Successor ! {schedule, NewGlobalSchedule},
			waiting(Name, NewLocalSchedule, NewLastSeen, Energy, Successor)
	end.
	
vote_schedule(GlobalSchedule) ->
	[Tasks,Votes] = GlobalSchedule,
	[MyVotes,LocalSchedule] = schedule(Tasks),
	NewVotes = vote(MyVotes,Votes,1),
	[[Tasks,NewVotes],LocalSchedule].

schedule(Tasks) ->
	%% Podría ser simplemente aprovechar el output del TaskPlanner programado en swipl
	[Tasks,[]].%[Votes,LocalSchedule].
	
vote(_, [], _) -> [];
vote(MyVotes, [Vote|Votes], Pos) ->
	PrevVotes = vote(MyVotes, Votes, Pos+1),
	Result = lists:keyfind(Pos, 1, MyVotes),
	if
		is_tuple(Result) ->
			{_, Value} = Result,
			NewVotes = lists:append(lists:append(Vote,[{self(), Value}]),PrevVotes);
		true ->
			NewVotes = PrevVotes
	end,
	NewVotes.

set_schedule([[],[]],LocalSchedule) ->
	[[[],[]],LocalSchedule];
set_schedule([[Task|Tasks],[Vote|Votes]], LocalSchedule) ->
	[[TailT,TailV],PrevLS] = set_schedule([Tasks,Votes], LocalSchedule),
	MaxVote = max_key(Vote),
	MyVote = lists:keyfind(self(),1,Vote),
	if
		MyVote == MaxVote ->
			[[lists:append([Task],TailT),lists:append(MyVote,TailV)],PrevLS];
		true ->
			[[lists:append([Task],TailT),lists:append(lists:keydelete(self(),1,Vote),TailV)],lists:keydelete(TaskID,2,PrevLS)]
	end.
	
max_key([{Key,_}]) -> Key;
max_key([{Key,_}|Tail]) ->
	Max = max_key(Tail),
	if
		Key > Max -> Key;
		true -> Max
	end.
	
find_value(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        {Key, Result} -> Result;
        false -> nothing
    end.
    
tuple_list_from_lists([],[]) -> [];
tuple_list_from_lists([Elem1|List1],[Elem2|List2]) ->
	[{Elem1,Elem2}|tuple_list_from_lists(List1,List2)].
	
lists_repeat(_,0) -> [];
lists_repeat(Term,Length) ->
	[Term|lists_repeat(Term,Length-1)].
