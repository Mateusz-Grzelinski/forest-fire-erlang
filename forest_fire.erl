-module(forest_fire).
-compile([export_all]).

% X - x world length
% Y - y world length
% Delay - in ms between computing generations
% P - propability of growing tree in empty space (0 to 1)
% F - propability of igniting fire (0 to 1), typically P >> F
main(X, Y, P, F) ->
   World = [{XX, YY, random_field()} || XX <- lists:seq(1,Y),  YY <- lists:seq(1, X)],
   PIDs = start_processes(World),
   loop(World, X, Y, P, F, PIDs, 0).

% X - x world length
% Y - y world length
% P and F are set to default values
main(X, Y) ->
   P = 0.1,
   F = 0.01,
   World = [{XX, YY, random_field()} || XX <- lists:seq(1,Y),  YY <- lists:seq(1, X)],
   PIDs = start_processes(World),
   loop(World, X, Y, P, F, PIDs, 0).

% get all initial values from text file
main(Filename) ->
   try file:open(Filename, [read]) of
      {ok, File_handle} ->
         {P, F, World, X, Y} = parse_file(File_handle),
         file:close(File_handle),
         PIDs = start_processes(World),
         loop(World, X, Y, P, F, PIDs, 0)
   catch
      {error, E} -> io:format("file error ~p", [E])
   end.

% generate start forest
random_field() ->
   case rand:uniform(2) of
      1 -> tree;
      2 -> empty
   end.


%% Reading and parsing file %%
% Example of file:
% P = 0.1
% F = 0.01
% X = 2
% Y = 2
% ff
% ft


parse_file(File_handle) ->
   P = parse(File_handle, {p}),
   F = parse(File_handle, {f}),
   X = parse(File_handle, {x}),
   Y = parse(File_handle, {y}),
   World = parse_world(File_handle, X, Y, 0, 0, []),
   io:format("P: ~p, F: ~p, X: ~p, Y: ~p, World: ~p", [P, F, X, Y, World]),
   {P, F, World, X, Y}.

% get P variable from single line
parse(File_handle, {p}) ->
   {ok, Line} = file:read_line(File_handle),
   [Word | Rest] = string:split(Line, "="),
   case string:trim(Word) of
      "P"  -> parse_value(Rest, {float_0_to_1});
      true -> throw(fist_line_must_define_p)
   end;

% get F variable from single line
parse(File_handle, {f}) ->
   {ok, Line} = file:read_line(File_handle),
   [Word | Rest] = string:split(Line, "="),
   case string:trim(Word) of
      "F"  -> parse_value(Rest, {float_0_to_1});
      true -> throw(second_line_must_define_f)
   end;

% get X variable from single line
parse(File_handle, {x}) ->
   {ok, Line} = file:read_line(File_handle),
   [Word | Rest] = string:split(Line, "="),
   case string:trim(Word) of
      "X"  -> parse_value(Rest, {int_0_to_inf});
      true -> throw(third_line_must_define_x)
   end;

% get Y variable from single line
parse(File_handle, {y}) ->
   {ok, Line} = file:read_line(File_handle),
   [Word | Rest] = string:split(Line, "="),
   case string:trim(Word) of
      "Y"  -> parse_value(Rest, {int_0_to_inf});
      true -> throw(fourth_line_must_define_y)
   end.

% get world map from file from multiple lines
parse_world(File_handle, X_max, Y_max, X_current, Y_current, New_world) ->
   io:format("~p", [New_world]),
   if
      Y_current == Y_max -> New_world;
      true ->
         {ok, Line} = file:read_line(File_handle),
         New_row = parse_world_row(Line, X_max, X_current, Y_current, []),
         parse_world(File_handle, X_max, Y_max, X_current, Y_current+1,
                     lists:merge(New_row, New_world))
   end.

% helper for reading world
parse_world_row([Sign | Rest], X_max, X_current, Y_current, World_row) ->
   case Sign of
      $f -> parse_world_row(Rest, X_max, X_current+1, Y_current,
                         [{X_current+1, Y_current, on_fire} | World_row]);
      $t -> parse_world_row(Rest, X_max, X_current+1, Y_current,
                         [{X_current+1, Y_current, tree} | World_row]);
      $o -> parse_world_row(Rest, X_max, X_current+1, Y_current,
                         [{X_current+1, Y_current, empty} | World_row]);
      $\n ->
         if
            X_max /= X_current -> throw(row_number_mismatch);
            true -> World_row
         end;
      true -> throw(world_illegal_character)
   end.

% helper for parsing integers range from 0 to inf
parse_value(Value, {int_0_to_inf}) ->
   Num = string:trim(Value),
   case string:to_integer(Num) of
      {error,no_float} ->
         throw(int_parsing_error);
      {Number, Rest} ->
         Rest_is_empty = string:is_empty(string:trim(Rest)),
         if
            not Rest_is_empty ->
               io:format("Must be integer"),
               throw(int_parsing_error);
            Number =< 0 -> io:format("Must be greater than 0\n"),
                           throw(int_parsing_error);
            true -> Number
         end
   end;

% helper for parsing float range from 0 to 1
parse_value(Value, {float_0_to_1}) ->
   Num = string:trim(Value),
   case string:to_float(Num) of
      {error,no_float} -> throw(float_parse_error);
      {Number, _Rest} -> if
                            Number =< 0 ->
                               io:format("Must be greater than 0\n"),
                               throw(float_parse_error);
                            Number >= 1 ->
                               io:format("Must be less or equal 1\n"),
                               throw(float_parse_error);
                            true -> Number
                         end
   end.


%% main loop and interactive interface %%


loop(World, World_X, World_Y, P, F, PIDs, Generation) ->
   print({clear}),
   Header_height = print({header, Generation, P, F, World_X, World_Y}),
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
                            loop(World, World_X, World_Y, New_P, F, PIDs, Generation);
                         "cf"->
                            New_F = get_probability('F >'),
                            loop(World, World_X, World_Y, P, New_F, PIDs, Generation);
                         [] ->
                            New_world = next_generation(World, P, F, PIDs),
                            loop(New_world, World_X, World_Y, P, F, PIDs, Generation+1);
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


%% Process management %%


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


%% Rules and logic %%
% A burning cell turns into an empty cell
% A tree will burn if at least one neighbor is burning
% A tree ignites with probability f even if no neighbor is burning
% An empty space fills with a tree with probability p


% find neighbourhood 3x3
nhood(Xin, Yin, World) ->
   Indexes = [{X+Xin, Y+Yin} || X <- [-1,0,1], Y <- [-1,0,1]],
   lists:filter(fun({X,Y,_}) ->
                      lists:any(fun(Elem) -> {X,Y} == Elem end, Indexes) % {X,Y} in Indexes
                end, World).

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
   io:format("\e[~p;~pH", [World_Y+Height_offset+1, 0]);

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
print({header, Pokolenie, P, F, X, Y}) -> io:format("\e[~p;~pHSymulacja pożaru lasu
T - drzewo
f - pożar
Pokolenie ~p
Prawd. że wyrośnie drzewo: ~p, Prawd. pożaru: ~p, Rozmiar X: ~p, Rozmiar Y: ~p
\n", [0,0,Pokolenie, P, F, X, Y]),
7;

print({foot}) -> io:format("type 'help' for help, 'stop' to exit\n");

print({help}) -> io:format("\\n - kolejne pokolenie (znak nowej lini)
cp - zmień prawdopodobieństwo, że wyrośnie drzewo na pustym oszarze (zakres 0 do 1)
cf - zmień prawdopodobieństwo, że wyrośnie drzewo zapłonie (zakres 0 do 1)
save - zapisz bieżące pokolenie do pliku
stats - wypisz statystyki
stop - wyjdź
").

