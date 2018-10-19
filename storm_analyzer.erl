%%%
%%% storm_analyzer.erl
%%% Author: Max Greenwald
%%% Date: October 19, 2018
%%%
%%% Leader for the storm analyzer. Generate workers to complete analysis and
%%% delegate analysis to ready workers, as well as print all output.
%%%

-module(storm_analyzer).

-include_lib("storms.hrl").
-export([start/0, start/1, start/2, stop/1]).

%% Start main analyzer process. Default to 2 workers if not provided
%% and use 'florida.txt' by default
start() -> start(2).
start(WorkerCount) -> start(WorkerCount, "florida.txt").
start(WorkerCount, RegionsFile) ->
  % Get the regions from a file. Must be a valid Erlang list, where each
  % Element of the list
  {ok, [Regions]} = file:consult(RegionsFile),
  Leader = spawn(fun() -> loop([], [], [], 0) end),
  Workers = [
    spawn(analysis_worker, worker_loop, [Leader, Regions]) ||
    _ <- lists:duplicate(WorkerCount, unused)
  ],
  Leader ! {add_workers, Workers},
  Leader.

%% Stop an analysis server with Pid
stop(Pid) ->
  Pid ! {stop}.

%% Main Analysis Loop. Keep track of all the "ready" workers (not currently
%% analyzing a storm), all of the workers (to be able to stop the server at
%% any point), and the current storms that are waiting to be analyzed. If there
%% is a ready worker and a storm at any point, send the storm to the worker
loop([Worker | ReadyWorkers], AllWorkers, [Storm | Storms], StormCount) ->
  Worker ! {storm, Storm},
  loop(ReadyWorkers, AllWorkers, Storms, StormCount);

%% Main recieve loop: given there are no more workers to assign storms,
%% wait for incoming messages
loop(ReadyWorkers, AllWorkers, Storms, StormCount) ->
  receive

    % Add workers. Necessary to initialize the leader before spawning
    % workers as 2-way communication is necessary (each needs other's Pid)
    {add_workers, Workers} ->
      loop(Workers ++ ReadyWorkers, Workers ++ AllWorkers, Storms, StormCount);

    % Worker alerting the server that it's ready to receive another storm
    {worker_ready, Worker} ->
      loop([Worker | ReadyWorkers], AllWorkers, Storms, StormCount);

    % Receive a storm from the parser. Add it to the end of the list
    % of storms
    {storm, Storm} ->
      loop(ReadyWorkers, AllWorkers, Storms ++ [Storm], StormCount);

    % Print the results of a storm analysis. Do this in the main
    % process in order to limit printing to one at a time
    {print, Results} ->
      {Name, {Year, Month, Day, Hours, Minutes}, MaxWindSpeed} = Results,
      io:format("~p~n", [Name]),
      io:format("Landfall: ~p/~p/~p ~p:~2..0B UTC~n", [Month, Day, Year, Hours, Minutes]),
      io:format("Max Wind Speed: ~p knots~n~n", [MaxWindSpeed]),
      io:format("Storm Count: ~p~n", [StormCount + 1]),
      loop(ReadyWorkers, AllWorkers, Storms, StormCount + 1);

    % Stop the server. Alert all workers to stop, then don't loop.
    {stop} ->
      [Worker ! {stop} || Worker <- AllWorkers],
      ok
  end.
