-module(forest_fire).
-compile([export_all]).

% X - x world length
% Y - y world length
% Delay - in ms between computing generations
% P - propability of growing tree in empty space (0 to 1)
% F - propability of igniting fire (0 to 1), typically P >> F
main(X, Y, Delay, P, F) ->
   World = [{XX, YY, random_field()} || XX <- lists:seq(1,Y),  YY <- lists:seq(1, X)],
   % spawn jobs: 1 job for each cell
   Workers = lists:foldl(
            fun(_, Worker_acc) ->
                  [spawn(forest_fire, cell_job, [self(), P, F]) | Worker_acc]
            end,
            [], World),
   loop(World, Delay, Workers).

% generate start forest
random_field() -> field(rand:uniform(2)).

field(1) -> tree;
field(2) -> empty;
field(3) -> on_fire.

loop(World, Delay, Workers) ->
   print({clear}),
   print_world(World),
   New_world = next_generation(World, Workers),
   timer:sleep(Delay),
   loop(New_world, Delay, Workers).

next_generation(World, Workers) ->
   % send job to workers
   Zipped = lists:zip(World, Workers),
   lists:map(fun({Elem, PID}) -> PID!{Elem, World} end, Zipped),
   receive_cell_job(length(World), []).

% receive done jobs
receive_cell_job(Max_length, New_world) ->
   receive
      Field ->
         case length(New_world) + 1 of
            Max_length -> [Field | New_world];
            _ -> receive_cell_job(Max_length, [Field | New_world])
         end
   end.

% helper for job spawning (I want Nhood to be calculated in spawned job)
cell_job(PID, P, F) ->
   % io:format("self: ~p, PID: ~p, P: ~p, F: ~p\n", [self(), PID, P, F]),
   receive
      {{Xin, Yin, Middle_field}, World} ->
         Nhood = nhood(Xin, Yin, World),
         PID!{Xin, Yin, rules(Middle_field, Nhood, P, F)},
         % do not exeit, wait for another job
         cell_job(PID, P, F);
      stop -> true
   end.

% find neighbourhood 3x3, without middle element
nhood(Xin, Yin, World) ->
   Indexes = [{X+Xin, Y+Yin} || X <- [-1,0,1], Y <- [-1,0,1]],
   lists:filter(fun({X,Y,_}) ->
                      lists:any(fun(Elem) -> {X,Y} == Elem end, Indexes) % {X,Y} in Indexes
                end, World).

%% Rules:
% A burning cell turns into an empty cell
% A tree will burn if at least one neighbor is burning
% A tree ignites with probability f even if no neighbor is burning
% An empty space fills with a tree with probability p

% Nhood is matrix 3x3 like: {1, 1, on_fire}
rules(Middle_field, Nhood, P, F) ->
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

print_world([]) -> io:format(' ');
print_world([Elem | World]) ->
   print(Elem),
   print_world(World).

print({X, Y, Field}) ->
   io:format("\e[~p;~pH~p",[Y, X*2, field_acronym(Field)]);

print({clear}) ->
   io:format("\e[2J",[]).

field_acronym(on_fire) -> f;
field_acronym(empty) -> o ;
field_acronym(tree) -> t.

