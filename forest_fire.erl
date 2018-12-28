-module(forest_fire).
-compile([export_all]).

% forest fire simulation
% World is list of tuples example:
% [{1,1,on_fire},{1,2,empty},{1,3,empty},{2,1,empty},{2,2,tree},{2,3,empty},{3,1,on_fire},{3,2,tree},{3,3,on_fire}]
main(X, Y) ->
	% generate random forest
	World = [{XX, YY, random_field()} || XX <- lists:seq(1,Y),  YY <- lists:seq(1, X)],
	loop(World, 5000).

% generate random forest fields: empty, on_fire or tree
random_field() ->
	case rand:uniform(3) of
	1 -> on_fire;
	2 -> empty;
	3 -> tree
	end.

loop(World, Delay) ->
	print(World),
	M = next_generation(World),
	timer:sleep(Delay),
	loop(M, Delay).

% TODO: use pararel spawn
next_generation(World) ->
	lists:map(fun({X,Y,_}) -> next_cell(X,Y, World) end, World).

% find neighbourhood 3x3
nhood(Xin, Yin, World) ->
	Indexes = [{X+Xin, Y+Yin} || X <- [-1,0,1], Y <- [-1,0,1] ],
	lists:filter(fun({X,Y,_}) ->
									 lists:any(fun(Elem) -> {X,Y} == Elem end, Indexes) % {X,Y} in Indexes
							 end, World).

%% Rules:
% A burning cell turns into an empty cell
% A tree will burn if at least one neighbor is burning
% A tree ignites with probability f even if no neighbor is burning
% An empty space fills with a tree with probability p

next_cell(Xin, Yin, World) ->
	{Xin, Yin, rules(Xin, Yin, nhood(Xin, Yin, World))}.

% Nhood is matrix 3x3 like: {1, 1, on_fire}
rules(Xin, Yin, Nhood) ->
	{_, {_, _, Middle_field}} = lists:search(fun({X,Y,_}) -> {X, Y}=={Xin,Yin} end, Nhood),
	case Middle_field of
		on_fire -> empty;
		empty -> maybe_tree(0.1);
		tree -> Fire_nearby = lists:any(fun({_,_,Field}) -> Field == on_fire end, Nhood),
						if
							Fire_nearby -> on_fire;
							true -> maybe_fire(0.1)
						end;
		false -> error(indexes_mismatch)
	end.

% P - propability of growing tree (0 to 1)
maybe_tree(P) ->
	R = rand:uniform(),
	case R > P of
		false -> tree;
		true -> empty
	end.

% P - propability of igniting fire (0 to 1)
maybe_fire(F) ->
	R = rand:uniform(),
	case R > F of
		false -> on_fire;
		true -> tree
	end.

% formating and printing
print(L) when is_list(L) ->
		io:format("["),
		fnl(L),
		io:format("]"),
		io:format("\n").

fnl([H]) ->
		io:format("~p", [H]);
fnl([H|T]) ->
		io:format("~p,", [H]),
		fnl(T);
fnl([]) ->
		ok.

