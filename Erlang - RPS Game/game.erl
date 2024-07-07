% Mohammed Huzaifa
% 40242080
% COMP 6411: CSPL : RPS PROJECT
% 24th June 2024

-module(game).
-export([start/1, handle_messages/4]).

start(Args) ->
    % Print welcome message
    io:format("~n** Rock, Paper, Scissors World Championship **"),

    % Read player info from file
    io:format("~n~nPlayer Credits:"),
    PlayerFile = lists:nth(1, Args),
    {ok, Players} = file:consult(PlayerFile),

    % Print players info
    print_players_info(Players),

    io:format("~n~nStarting game log...~n~n"),

    % Spawn players and register them
    spawn_and_register_players(Players),

    InitialPlayersState = lists:foldl(fun({Name, Credits}, Acc) -> maps:put(Name, {Credits, Credits, false}, Acc) end, #{}, Players),

    % Initialize the master process state with player losses and game ID counter
    handle_messages(Players, #{}, InitialPlayersState, 0),
    io:format("~n").

print_players_info(Players) ->
    % Iterate over each player record and print it
    lists:foreach(fun({Name, Credits}) ->
        io:format("~n* ~p: ~p", [Name, Credits])
    end, Players).

spawn_and_register_players(Players) ->
    % Iterate over each player record and spawn them
    lists:foreach(fun({Name, _Credits}) ->
        Pid = spawn(player, start, [Name, false, Players, self()]),
        register(Name, Pid)
    end, Players).

handle_messages(Players, Games, PlayersState, GameIdCounter) ->
    receive
        {request_game, ReqPlayer1, ReqPlayer2} when ReqPlayer1 =/= ReqPlayer2 ->
            % Check active status (i.e. not disqualified status) of both players
            IsPlayer1Disqualified = get_disqualification_status(ReqPlayer1, PlayersState),
            IsPlayer2Disqualified = get_disqualification_status(ReqPlayer2, PlayersState),
            
            if not IsPlayer1Disqualified andalso not IsPlayer2Disqualified ->
                % update counters
                NewGameIdCounter = GameIdCounter + 1,
                
                io:format("+ [~p] new game for ~p and ~p~n", [NewGameIdCounter, ReqPlayer1, ReqPlayer2]),
                
                % Send game ID to both players
                whereis(ReqPlayer1) ! {game_id, NewGameIdCounter, ReqPlayer2},
                whereis(ReqPlayer2) ! {game_id, NewGameIdCounter, ReqPlayer1},
                
                handle_messages(Players, Games, PlayersState, NewGameIdCounter);
            true ->
                % If any player is disqualified, skip game scheduling
                handle_messages(Players, Games, PlayersState, GameIdCounter)
            end;
        {request_game, _ReqPlayer1, _ReqPlayer2} -> 
            handle_messages(Players, Games, PlayersState, GameIdCounter);
        {move, GameId, Player, Move} ->
            % Update the game with the new move
            case maps:get(GameId, Games, undefined) of
                undefined ->
                    % io:format("~n <~p> First move: ~p ~nGames: ~p~n", [GameId, Player, Games]),
                    % First move for this game
                    NewGame = #{player_name => Player, player_moves => [Move], opponent_name => undefined, opponent_moves => []},
                    NewGames = maps:put(GameId, NewGame, Games),
                    handle_messages(Players, NewGames, PlayersState, GameIdCounter);
                Game ->
                    PlayerName = maps:get(player_name, Game),
                    OpponentName = maps:get(opponent_name, Game, undefined),
                    % io:format("~n<~p>  ~p vs ~p~n", [GameId, PlayerName, OpponentName]),
                    % io:format("~n <~p> Player: ~p Opponent: ~p~n", [GameId, PlayerName, OpponentName]),
                    if OpponentName =:= undefined ->
                        % First move for the opponent
                        UpdatedGame = maps:put(opponent_name, Player, maps:put(opponent_moves, [Move], Game)),
                        NewGames = maps:put(GameId, UpdatedGame, Games),
                        
                        PlayerMoves = maps:get(player_moves, UpdatedGame),
                        OpponentMoves = maps:get(opponent_moves, UpdatedGame),

                        % Check if both players have made the same number of moves
                        if length(PlayerMoves) =:= length(OpponentMoves) ->
                            % Determine the winner for the last moves
                            LastPlayerMove = lists:last(PlayerMoves),
                            LastOpponentMove = lists:last(OpponentMoves),
                            Winner = determine_winner({PlayerName, LastPlayerMove}, {Player, LastOpponentMove}),
                            case Winner of
                                tie ->
                                    % Replay the game
                                    whereis(PlayerName) ! {replay, GameId},
                                    whereis(Player) ! {replay, GameId},
                                    handle_messages(Players, NewGames, PlayersState, GameIdCounter);
                                _ ->
                                    NewState = update_player_state_and_print_game_result(Winner, PlayerName, Player, PlayerMoves, OpponentMoves, PlayersState, GameId),
                                    notify_players_if_tournament_over(NewState),
                                    whereis(PlayerName) ! {initiate_game, Players},
                                    whereis(Player) ! {initiate_game, Players},
                                    handle_messages(Players, NewGames, NewState, GameIdCounter)
                            end;
                        true ->
                            handle_messages(Players, NewGames, PlayersState, GameIdCounter)
                        end;

                       PlayerName =:= Player ->
                        % Add move to player's moves
                        UpdatedPlayerMoves = maps:get(player_moves, Game) ++ [Move],
                        UpdatedGame = maps:put(player_moves, UpdatedPlayerMoves, Game),
                        NewGames = maps:put(GameId, UpdatedGame, Games),
                        handle_messages(Players, NewGames, PlayersState, GameIdCounter);

                       OpponentName =:= Player ->
                        % Add move to opponent's moves
                        UpdatedOpponentMoves = maps:get(opponent_moves, Game) ++ [Move],
                        UpdatedGame = maps:put(opponent_moves, UpdatedOpponentMoves, Game),
                        NewGames = maps:put(GameId, UpdatedGame, Games),
                        PlayerMoves = maps:get(player_moves, UpdatedGame),
                        OpponentMoves = maps:get(opponent_moves, UpdatedGame),
                        % Check if both players have made the same number of moves
                        if length(PlayerMoves) =:= length(OpponentMoves) ->
                            % Determine the winner for the last moves
                            LastPlayerMove = lists:last(PlayerMoves),
                            LastOpponentMove = lists:last(OpponentMoves),
                            Winner = determine_winner({PlayerName, LastPlayerMove}, {OpponentName, LastOpponentMove}),
                            case Winner of
                                tie ->
                                    % Replay the game
                                    whereis(PlayerName) ! {replay, GameId},
                                    whereis(OpponentName) ! {replay, GameId},
                                    handle_messages(Players, NewGames, PlayersState, GameIdCounter);
                                _ ->
                                    NewState = update_player_state_and_print_game_result(Winner, PlayerName, OpponentName, PlayerMoves, OpponentMoves, PlayersState, GameId),
                                    notify_players_if_tournament_over(NewState),
                                    whereis(PlayerName) ! {initiate_game, Players},
                                    whereis(OpponentName) ! {initiate_game, Players},
                                    handle_messages(Players, NewGames, NewState, GameIdCounter)
                            end;
                        true ->
                            handle_messages(Players, NewGames, PlayersState, GameIdCounter)
                        end;
                    true ->
                        handle_messages(Players, Games, PlayersState, GameIdCounter)
                    end
            end;
        _ ->
            handle_messages(Players, Games, PlayersState, GameIdCounter)
    end.

% Function to get player disqualification status
get_disqualification_status(Player, PlayersState) ->
    {_, _, DisqualificationStatus} = maps:get(Player, PlayersState),
    DisqualificationStatus.

determine_winner({Player1, Move1}, {Player2, Move2}) ->
    case {Move1, Move2} of
        {rock, scissors} -> Player1;
        {scissors, paper} -> Player1;
        {paper, rock} -> Player1;
        {scissors, rock} -> Player2;
        {paper, scissors} -> Player2;
        {rock, paper} -> Player2;
        _ -> tie
    end.

update_player_state_and_print_game_result(Winner, Player, Opponent, PlayerMoves, OpponentMoves, PlayersState, GameId) ->
    MovesString = format_moves(Player, Opponent, PlayerMoves, OpponentMoves),
    case Winner of
        Player ->
            % Opponent loses
            {NewCredits, NewDisqualificationStatus, NewState} = update_player_credits_and_disqualification_status(Opponent, PlayersState),
            % Check if opponent is disqualified
            if NewDisqualificationStatus == true ->
                whereis(Opponent) ! {disqualified},
                io:format("- (~p) ~s = ~p loses [~p credits left]~n", [GameId, MovesString, Opponent, NewCredits]);
            true ->
                io:format("$ (~p) ~s = ~p loses [~p credits left]~n", [GameId, MovesString, Opponent, NewCredits])
            end,
            NewState;
        Opponent ->
            % Player loses
            {NewCredits, NewDisqualificationStatus, NewState} = update_player_credits_and_disqualification_status(Player, PlayersState),
            % Check if player is disqualified
            if NewDisqualificationStatus == true ->
                whereis(Player) ! {disqualified},
                io:format("- (~p) ~s = ~p loses [~p credits left]~n", [GameId, MovesString, Player, NewCredits]);
            true ->
                io:format("$ (~p) ~s = ~p loses [~p credits left]~n", [GameId, MovesString, Player, NewCredits])
            end,
            NewState
    end.

format_moves(Player, Opponent, PlayerMoves, OpponentMoves) ->
    io_lib:format("~p:[~s] -> ~p:[~s]", [
        Player,
        lists:join(", ", [atom_to_list(Move) || Move <- PlayerMoves]),
        Opponent,
        lists:join(", ", [atom_to_list(Move) || Move <- OpponentMoves])
    ]).

update_player_credits_and_disqualification_status(Player, PlayersState) ->
    {Credits, RemainingCredits, DisqualificationStatus} = maps:get(Player, PlayersState),
    NewCredits = RemainingCredits - 1,
    NewDisqualificationStatus = case NewCredits =< 0 of
        true -> true;
        false -> DisqualificationStatus
    end,
    NewState = maps:put(Player, {Credits, NewCredits, NewDisqualificationStatus}, PlayersState),
    {NewCredits, NewDisqualificationStatus, NewState}.


% Function to notify players if the tournament is over
notify_players_if_tournament_over(PlayersState) ->
    ActivePlayers = get_active_players(PlayersState),

    case ActivePlayers of
        [Winner] ->
            io:format("~nWe have a winner...~n~n"),
            print_tournament_report(PlayersState),
            io:format("~nWinner: ~p~n", [Winner]),
            io:format("\nSee you next year...~n~n"),
            lists:foreach(fun(Player) -> whereis(Player) ! {tournament_over} end, maps:keys(PlayersState)),
            lists:foreach(fun(Player) -> whereis(Player) ! stop end, maps:keys(PlayersState)),
            halt(); % Terminate the program after the tournament is over
        _ ->
            ok
    end.

% Function to get the list of active players
get_active_players(PlayersState) ->
    maps:fold(fun(Player, {_, _, Disqualified}, Acc) ->
        if Disqualified == false -> [Player | Acc]; true -> Acc end
    end, [], PlayersState).

% Function to print the tournament report
print_tournament_report(PlayersState) ->
    io:format("** Tournament Report **~n~nPlayers:"),
    TotalGames = maps:fold(fun(Player, {Credits, RemainingCredits, _}, Acc) ->
        io:format("~n ~p: credits used: ~p, credits remaining: ~p", [Player, Credits - RemainingCredits, RemainingCredits]),
        Acc + (Credits - RemainingCredits)
    end, 0, PlayersState),
    io:format("~n ----"),
    io:format("~n Total games: ~p~n", [TotalGames]).
