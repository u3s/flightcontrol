-module(flightcontrol).
-compile(export_all).

%% SIMULATION PROCESSES
tower_loop(Map0) ->
    io:format("~p [TC] approach area map: ~p~n", [self(), Map0]),
    receive
        {get_next_lane, 0, 0, PlanePid} ->
            PlanePid ! landed,
            tower_loop(lists:keydelete(PlanePid, 2, Map0));
        {get_next_lane, 0, _, PlanePid} ->
            PlanePid ! {crash, "while landing"},
            tower_loop(lists:keydelete(PlanePid, 2, Map0));
        {get_next_lane, Distance, Lane, PlanePid} ->
            io:format("~p [TC] plane~p on lane: ~p distance: ~p~n",
                      [self(), PlanePid, Lane, Distance]),
            NextLane = reduce_lane(Lane),
            case lists:keyfind({Distance, NextLane}, 1, Map0) of
                false ->
                    PlanePid ! {path_clear, NextLane},
                    tower_loop(lists:keystore(PlanePid, 2, Map0,
                                              {{Distance, NextLane}, PlanePid}));
                {_, FoundPlanePid} ->
                    PlanePid ! {crash, "in air"},
                    FoundPlanePid ! {crash, "in air"},
                    Map = lists:keydelete(FoundPlanePid, 2, Map0),
                    io:format("~p [TC] Collision between ~p an ~p~n",
                              [self(), PlanePid, FoundPlanePid]),
                    tower_loop(Map)
            end
    end.

reduce_lane(0) -> 0;
reduce_lane(L) -> L-1.

plane_loop(Speed, Distance, Lane, TowerPid) ->
    {message_queue_len,Length} = process_info(self(), message_queue_len),
    io:format("~p [Plane] Length=~p ~n", [self(), Length]),
    case Length of
        0 ->
            io:format("~p [Plane] sends get_next_lane ~n", [self()]),
            TowerPid ! {get_next_lane, Distance, Lane, self()};
        _ ->
            ok
    end,
    receive
        {crash, Info} ->
            io:format("~p [Plane] <<CRASHED>> ~p~n", [self(), Info]);
        landed ->
            io:format("~p [Plane] <<LANDED>> ~n", [self()]);
        {path_clear, NextLane} ->
            timer:sleep(Speed),
            plane_loop(Speed, Distance-1, NextLane, TowerPid)
    end.

%% TEST FUNCTIONS
simulate() ->
    TowerPid = spawn(flightcontrol, tower_loop, [[]]),
    spawn(flightcontrol, plane_loop,
          [_Speed = 100, _Distance = 5, _Lane = 5, TowerPid]),
    spawn(flightcontrol, plane_loop, [50, 4, 4, TowerPid]),
    ok.

% FIXME why one of the planes tries flying after crash?
simulate_crash_in_air() ->
    TowerPid = spawn(flightcontrol, tower_loop, [[]]),
    spawn(flightcontrol, plane_loop,
          [_Speed = 100, _Distance = 5, _Lane = 5, TowerPid]),
    spawn(flightcontrol, plane_loop, [100, 5, 5, TowerPid]),
    ok.

simulate_crash_while_landing() ->
    TowerPid = spawn(flightcontrol, tower_loop, [[]]),
    spawn(flightcontrol, plane_loop,
          [_Speed = 100, _Distance = 3, _Lane = 5, TowerPid]),
    ok.
