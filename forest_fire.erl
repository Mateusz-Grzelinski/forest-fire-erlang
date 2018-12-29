-module(forest_fire).
-compile([export_all]).

% X - x world length
% Y - y world length
% Delay - in ms between computing generations
% P - propability of growing tree in empty space (0 to 1)
% F - propability of igniting fire (0 to 1), typically P >> F
main(X, Y, Delay, P, F) ->
   % generate random forest
   World = [{XX, YY, random_field()} || XX <- lists:seq(1,Y),  YY <- lists:seq(1, X)],
   loop(World, Delay, P, F).

% generate random forest fields: empty, on_fire or tree
random_field() ->
   case rand:uniform(3) of
      1 -> tree; % on_fire;
      2 -> empty;
      3 -> tree
   end.

loop(World, Delay, P, F) ->
   print({clear}),
   print_world(World),
   New_world = next_generation(World, P, F),
   timer:sleep(Delay),
   loop(New_world, Delay, P, F).

next_generation(World, P, F) ->
   % spawn jobs: 1 job for each cell
   % TODO: redundant - we do not use PIDs
   PIDs = lists:foldl(
            fun({X, Y, _}, PIDs) ->
                  [spawn(forest_fire, next_cell, [self(), X, Y, World, P, F]) | PIDs]
            end,
            [], World),
   receive_loop(length(World), []).

% receive done jobs
receive_loop(Max_length, New_world) ->
   receive
      Field ->
         case length(New_world) + 1 == Max_length  of
            true -> [Field | New_world];
            false -> receive_loop(Max_length, [Field | New_world])
         end
   end.

% helper for job spawning (I want Nhood to be calculated in spawned job)
next_cell(PID, Xin, Yin, World, P, F) ->
   PID!{Xin, Yin, rules(Xin, Yin, nhood(Xin, Yin, World), P, F)}.

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

% Nhood is matrix 3x3 like: {1, 1, on_fire}
rules(Xin, Yin, Nhood, P, F) ->
   {_, {_, _, Middle_field}} = lists:search(fun({X,Y,_}) -> {X, Y}=={Xin,Yin} end, Nhood),
   case Middle_field of
      on_fire -> empty;
      empty -> maybe_tree(P);
      tree -> Fire_nearby = lists:any(fun({_,_,Field}) -> Field == on_fire end, Nhood),
              if
                 Fire_nearby -> on_fire;
                 true -> maybe_fire(F)
              end;
      false -> error(indexes_mismatch)
   end.

% P - propability of growing tree (0 to 1)
maybe_tree(P) ->
   R = rand:uniform(),
   if
      R < P -> tree;
      R >= P -> empty
   end.

% F - propability of igniting fire (0 to 1)
maybe_fire(F) ->
   R = rand:uniform(),
   if
      R < F -> on_fire;
      R >= F -> tree
   end.

% formating and printing
print_world([Elem]) ->
   print(Elem),
   io:format("\n");

print_world([Elem | World]) ->
   print(Elem),
   print_world(World).

print({X, Y, Field}) ->
   io:format("\e[~p;~pH~p",[Y,X*2,field_acronym(Field)]);

print({clear}) ->
   io:format("\e[2J",[]).

field_acronym(on_fire) -> f;
field_acronym(empty) -> o ;
field_acronym(tree) -> t.

