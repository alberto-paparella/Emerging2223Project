% Ogni automobile è rappresentata dal seguente sotto-sistema di attori:
% - Un attore "main" che lancia gli altri attori ed è responsabile di ri-crearli nel caso di fallimento di uno di loro
% - Un attore "friendship" che si preoccupa di mantenere 5 attori nella lista di attori, integrandone di nuovi nel caso in cui il numero scenda.
% - Un attore "state" che si preoccupa di mantenere il modello interno dell'ambiente e le coordinate del posteggio obiettivo.
%   In pratica l'attore registra per ogni cella l'ultima informazione giunta in suo possesso (posteggio libero/occupato/nessuna informazione)
%   e propaga le nuove informazione ottenute agli amici (protocollo di gossiping). Inoltre cambia il posteggio obiettivo quando necessario
%   (es. quando scopre che il posteggio è ora occupato).
% - Un attore "detect" che si occupa di muovere l'automobile sulla scacchiera, interagendo con l'attore "ambient" per fare sensing dello stato
%   di occupazione dei posteggi.

-module(car).
-export([main/4]).
-compile(export_all).

main(X, Y, GridWidth, GridHeight) ->
    % TODO: this message should be sent by detect actor, not main
    render ! {position, self(), X, Y},
    Grid = maps:from_list([{{N,M}, none} || N <- lists:seq(1, GridWidth), M <- lists:seq(1, GridHeight)]),
    StatePid = spawn_link(?MODULE, state, [Grid, X, Y]),
    FriendshipPid = spawn_link(?MODULE, friendship, [[], StatePid]),
    DetectPid = spawn_link(?MODULE, detect, [StatePid, GridWidth, GridHeight]).

% if car hasn't any friends, ask wellknown for friends
friendship(FriendsList, StatePid) when FriendsList =:= [] -> 
    Ref = make_ref(),
    wellknown ! {getFriends, self(), StatePid, Ref},
    receive 
        {myFriends, PidList, Ref} ->
            % TODO: choose 5 Pids from PidList that are different from self()
            io:format("PidList: ~p~n", [PidList]),
            friendship(PidList, StatePid)
    end;
% if friends are > 0 and < 5 then ask for new friends to friends
friendship(FriendsList, StatePid) when length(FriendsList) < 5 -> io:format("").
% do nothing
friendship(FriendsList, StatePid, Grid) -> io:format("").

state(Grid, X, Y) ->
    NewGrid = maps:update({X,Y}, {self(), true}, Grid),
    state(NewGrid, {X, Y}).

state(Grid, {X, Y}) -> 
    receive
        {isGoalFree, PID, NewGoalX, NewGoalY, Ref} -> case maps:get({NewGoalX, NewGoalY}, Grid) of
                none -> PID ! {goalFree, true, Ref};
                {_, Boolean} -> PID ! {goalFree, Boolean, Ref}
            end,
            state(Grid, {X, Y});
        {myPosition, PID, Ref} -> 
            PID ! {myPos, X, Y, Ref},
            state(Grid, {X, Y});
        {updateMyPosition, NewX, NewY, _} -> 
            NewGrid = maps:update({X,Y}, none, Grid),
            NewGrid1 = maps:update({NewX,NewY}, {self(), true}, NewGrid),
            state(NewGrid1, {X, Y})
    end.

detect(StatePid, GridWidth, GridHeight) -> 
    NewGoalX = rand:uniform(GridWidth),
    NewGoalY = rand:uniform(GridHeight),
    Ref = make_ref(),
    StatePid ! {isGoalFree, self(), NewGoalX, NewGoalY, Ref},
    receive
        {goalFree, Boolean, Ref} -> case Boolean of
            true -> move(NewGoalX, NewGoalY, StatePid, GridWidth, GridHeight);
            false -> detect(StatePid, GridWidth, GridHeight)
        end
    end,
    detect(StatePid, GridWidth, GridHeight).

move(NewGoalX, NewGoalY, StatePid, GridWidth, GridHeight) -> 
    Ref = make_ref(),
    StatePid ! {myPosition, self(), Ref},
    receive 
        {myPos, X, Y, Ref} -> 
            case X =:= NewGoalX of
                true -> 
                    NewY = (GridHeight - lists:max([Y, NewGoalY])) + lists:min([Y, NewGoalY]),
                    YPosRef = make_ref(),
                    StatePid ! {updateMyPosition, X, NewY, YPosRef};
                false -> 
                    case Y =:= NewGoalY of
                        true -> 
                            NewX = (GridWidth - lists:max([X, NewGoalX])) + lists:min([X, NewGoalX]),
                            XPosRef = make_ref(),
                            StatePid ! {updateMyPosition, NewX, Y, XPosRef};
                        false -> 
                            Axis = rand:uniform(2),
                            case Axis of 
                                1 -> 
                                    ANewX = (GridWidth - lists:max([X, NewGoalX])) + lists:min([X, NewGoalX]),
                                    PosRef = make_ref(),
                                    StatePid ! {updateMyPosition, ANewX, Y, PosRef};
                                2 ->
                                    ANewY = (GridHeight - lists:max([Y, NewGoalY])) + lists:min([Y, NewGoalY]),
                                    PosRef = make_ref(),
                                    StatePid ! {updateMyPosition, X, ANewY, PosRef}
                            end
                    end
            end
    end,
    move(NewGoalX, NewGoalY, StatePid, GridWidth, GridHeight).




