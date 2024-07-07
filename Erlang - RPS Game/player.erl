% Mohammed Huzaifa
% 40242080
% COMP 6411: CSPL : RPS PROJECT
% 24th June 2024

-module(player).
-export([start/4, handle_messages/3]).

start(Name, IsDisqualified, Players, Master) ->
    initiate_game(Name, IsDisqualified, Players, Master).

initiate_game(Name, IsDisqualified, Players, Master) ->
    Delay = rand:uniform(91) + 9, % Generate a random delay between 10 and 100 milliseconds
    timer:sleep(Delay), % Sleep for the randomly generated delay
    
    % Get the list of other players
    RegisteredNames = [Player || {Player, _} <- Players],
    OtherPlayers = lists:delete(Name, RegisteredNames),

    case OtherPlayers of
        [] ->
            % No other players available
            handle_messages(Name, IsDisqualified, Master);
        _ ->
            RandomIndex = rand:uniform(length(OtherPlayers)),
            RandomPlayer = lists:nth(RandomIndex, OtherPlayers),
            whereis(RandomPlayer) ! {invitation, Name},
            handle_messages(Name, IsDisqualified, Master)
    end.

handle_messages(Name, IsDisqualified, Master) ->
    receive
        {initiate_game, Players} when not IsDisqualified ->
            initiate_game(Name, IsDisqualified, Players, Master);
        {invitation, FromName} when not IsDisqualified, Name =/= FromName ->
            whereis(FromName) ! {confirmation, Name},
            handle_messages(Name, IsDisqualified, Master);
        {invitation, FromName} ->
            whereis(FromName) ! {rejection, Name},
            handle_messages(Name, IsDisqualified, Master);
        {confirmation, FromName} when not IsDisqualified ->
            Master ! {request_game, Name, FromName},
            handle_messages(Name, IsDisqualified, Master);
        {confirmation, _FromName} ->
            handle_messages(Name, IsDisqualified, Master);
        {rejection, _FromName} ->
            handle_messages(Name, IsDisqualified, Master);
        {game_id, GameId, Opponent} when not IsDisqualified, Name =/= Opponent ->
            Move = random_move(),
            Master ! {move, GameId, Name, Move},
            % io:format("~n Move ~p From ~p~n", [Move, Name]),
            handle_messages(Name, IsDisqualified, Master);
        {game_id, _GameId, _Opponent} ->
            handle_messages(Name, IsDisqualified, Master);
        {replay, GameId} when not IsDisqualified ->
            Move = random_move(),
            Master ! {move, GameId, Name, Move},
            handle_messages(Name, IsDisqualified, Master);
        {replay, _GameId} ->
            handle_messages(Name, IsDisqualified, Master);
        {disqualified} ->
            handle_messages(Name, true, Master);
        {tournament_over} ->
            stop;
        stop ->
            io:format("~p is stopped.~n", [Name]);
        _ ->
            handle_messages(Name, IsDisqualified, Master)
    end.

random_move() ->
    Moves = [rock, paper, scissors],
    RandomIndex = rand:uniform(3),
    lists:nth(RandomIndex, Moves).
