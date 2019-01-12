-module(forest_fire).
-compile([export_all]).

% wczytanie z pliku, stan początkowy zadany
% interaktywny interfejs
% statystyki?

% {ok, Stream} = file:open(File, write),
% foreach(fun(X) -> io:format(Stream,"~p~n",[X]) end, List),
% file:close(Stream).

% X - x world length
% Y - y world length
% Delay - in ms between computing generations
% P - propability of growing tree in empty space (0 to 1)
% F - propability of igniting fire (0 to 1), typically P >> F
main(X, Y, P, F) ->
   World = [{XX, YY, random_field()} || XX <- lists:seq(1,Y),  YY <- lists:seq(1, X)],
   PIDs = start_processes(World),
   loop(World, Y+1, P, F, PIDs, 0).

main(X, Y) ->
   P = 0.1,
   F = 0.01,
   World = [{XX, YY, random_field()} || XX <- lists:seq(1,Y),  YY <- lists:seq(1, X)],
   PIDs = start_processes(World),
   loop(World, Y+1, P, F, PIDs, 0).

% generate start forest
random_field() -> field(rand:uniform(2)).

% allowed world values
field(1) -> tree;
field(2) -> empty;
field(3) -> on_fire.

loop(World, World_Y, P, F, PIDs, Generation) ->
   print({clear}),
   Header_height = print({header, Generation}),
   print_world(World, World_Y, Header_height),
   print({foot}),

   % create func here to get access to all variables
   % Input_loop_function is hacky way of making loop
   Input_loop = fun(Input_loop_function) ->
                      Input = io:get_line(">>"),
                      case string:trim(Input) of
                         "help" ->
                            print({help}),
                            Input_loop_function(Input_loop_function);
                         "stop"->
                            stop_processes(World, PIDs),
                            exit;
                         "cp"->
                            New_P = get_probability('P >'),
                            loop(World, World_Y, New_P, F, PIDs, Generation);
                         "cf"->
                            New_F = get_probability('F >'),
                            loop(World, World_Y, P, New_F, PIDs, Generation);
                         [] ->
                            New_world = next_generation(World, P, F, PIDs),
                            loop(New_world, World_Y, P, F, PIDs, Generation+1);
                         _ ->
                            io:format("Nieznana komenda\n"),
                            Input_loop_function(Input_loop_function)
                      end
                end,
   Input_loop(Input_loop).

% accept float between 0 and 1 (boundaries not included)
get_probability(Prompt) ->
   Input = string:trim(io:get_line(Prompt)),
   case string:to_float(Input) of
      {error,no_float} -> io:format("This is not float\n"),
                          get_probability(Prompt);
      {Number, _Rest} -> if
                            Number =< 0 -> io:format("Must be greater than 0\n"),
                                           get_probability(Prompt);
                            Number >= 1 -> io:format("Must be less or equal 1\n"),
                                           get_probability(Prompt);
                            true -> Number
                         end
   end.

% calculate next gen - send and receive job
next_generation(World, P, F, PIDs) ->
   % send job to workers
   send_job(World, P, F, PIDs),
   receive_cell_job(length(World), []).

% spawn jobs: 1 job for each cell
start_processes(World) ->
   lists:foldl(
     fun(_, Workers) ->
           [spawn(forest_fire, cell_job, [self()]) | Workers]
     end, [], World).

stop_processes(World, PIDs) ->
   Zipped = lists:zip(World, PIDs),
   lists:map(fun({_Elem, PID}) -> PID!{stop} end, Zipped).

send_job(World, P, F, PIDs) ->
   Zipped = lists:zip(World, PIDs),
   lists:map(fun({Elem, PID}) -> PID!{Elem, World, P, F} end, Zipped).

% receive done jobs
receive_cell_job(Max_length, New_world) ->
   receive
      Field ->
         case length(New_world) + 1 of
            Max_length -> [Field | New_world];
            _ -> receive_cell_job(Max_length, [Field | New_world])
         end
   end.

% helper for job spawning (nhood will be calculated in spawned job)
cell_job(PID) ->
   % io:format("self: ~p, PID: ~p, P: ~p, F: ~p\n", [self(), PID, P, F]),
   receive
      {{Xin, Yin, Middle_field}, World, P, F} ->
         Nhood = nhood(Xin, Yin, World),
         PID!{Xin, Yin, rules(Middle_field, Nhood, P, F)},
         % do not exit, wait for another job
         cell_job(PID);
      stop -> true
   end.

% find neighbourhood 3x3
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

% Nhood is matrix 3x3
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

%% formating and printing %%

% return number of occupied lines
print_world([Elem], World_Y, Height_offset) ->
   print({Elem, Height_offset}),
   io:format("\n"),
   io:format("\e[~p;~pH", [World_Y+Height_offset, 0]);

% print_world([], _, World_height) -> io:format(' '), World_height;

print_world([Elem | World], World_Y, Height_offset) ->
   print({Elem, Height_offset}),
   print_world(World, World_Y, Height_offset).

% go to X,Y, print symbol and return number of occupied lines
print({{X, Y, Field}, Height_offset}) ->
   io:format("\e[~p;~pH",[Y+Height_offset, X*2]),
   case Field of
      on_fire -> io:format("f");
      empty -> io:format(" ");
      tree -> io:format("T")
   end;

print({clear}) ->
   io:format("\e[2J",[]);

% return number of occupied lines for correct world printing
print({header, Pokolenie}) -> io:format("\e[~p;~pHSymulacja pożaru lasu
o - miejsce puste
t - drzewo
f - pożar
Pokolenie ~p
\n", [0,0,Pokolenie]),
                              5;

print({foot}) -> io:format("type 'help' for help, 'stop' to exit\n");

print({help}) -> io:format("\\n - kolejne pokolenie (znak nowej lini)
cp - zmień prawdopodobieństwo, że wyrośnie drzewo na pustym oszarze (zakres 0 do 1)
cf - zmień prawdopodobieństwo, że wyrośnie drzewo zapłonie (zakres 0 do 1)
save - zapisz bieżące pokolenie do pliku
stats - wypisz statystyki
stop - wyjdź
").

