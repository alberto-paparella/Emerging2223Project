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
    Grid = maps:from_list([{{N,M}, true} || N <- lists:seq(1, GridWidth), M <- lists:seq(1, GridHeight)]),
    StatePid = spawn_link(?MODULE, state, [Grid, X, Y]),

    % TODO: this message should be sent by detect actor, not main
    render ! {position, StatePid, X, Y},

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
    NewGrid = maps:update({X,Y}, true, Grid),
    state(NewGrid, {X, Y}).

state(Grid, {X, Y}) -> 
    receive
        {isGoalFree, PID, NewGoalX, NewGoalY, Ref} -> case maps:get({NewGoalX, NewGoalY}, Grid) of
                true -> PID ! {goalFree, true, Ref};
                Boolean -> PID ! {goalFree, Boolean, Ref}
            end,
            state(Grid, {X, Y});
        {myPosition, PID, Ref} -> 
            PID ! {myPos, X, Y, Ref},
            state(Grid, {X, Y});
        {updateMyPosition, NewX, NewY, _} -> 
            NewGrid = maps:update({X,Y}, true, Grid),
            NewGrid1 = maps:update({NewX,NewY}, true, NewGrid),
            state(NewGrid1, {NewX, NewY});
        {statusUpdate, StatusX, StatusY, IsFree} -> 
            NewGrid = maps:update({StatusX, StatusY}, IsFree, Grid),
            state(NewGrid, {X, Y})
    end.

detect(StatePid, GridWidth, GridHeight) -> 
    NewGoalX = rand:uniform(GridWidth),
    NewGoalY = rand:uniform(GridHeight),
    Ref = make_ref(),
    StatePid ! {isGoalFree, self(), NewGoalX, NewGoalY, Ref},
    receive
        {goalFree, Boolean, Ref} -> case Boolean of
            true -> 
                render ! {target, StatePid, NewGoalX, NewGoalY},
                move(NewGoalX, NewGoalY, StatePid, GridWidth, GridHeight);
            false -> detect(StatePid, GridWidth, GridHeight)
        end
    end.

detect(StatePid, GoalX, GoalY, GridWidth, GridHeight) ->
    % sleep function
    sleep(2000),
    move(GoalX, GoalY, StatePid, GridWidth, GridHeight),
    Ref = make_ref(),
    StatePid ! {isGoalFree, self(), GoalX, GoalY, Ref},
    receive
        {goalFree, Boolean, Ref} -> case Boolean of
            true -> detect(StatePid, GoalX, GoalY, GridWidth, GridHeight);
            false -> detect(StatePid, GridWidth, GridHeight)
        end
    end.
    

move(NewGoalX, NewGoalY, StatePid, GridWidth, GridHeight) -> 
    Ref = make_ref(),
    StatePid ! {myPosition, self(), Ref},
    receive 
        {myPos, X, Y, Ref} -> 
            case X =:= NewGoalX of
                true -> case Y =:= NewGoalY of 
                            true -> 
                                ParkRef = make_ref(),
                                ambient ! {park, StatePid, NewGoalX, NewGoalY, ParkRef},
                                IsFreeRef = make_ref(),
                                ambient ! {isFree, StatePid, X, Y, IsFreeRef},
                                receive
                                    {status, _, IsFree} -> StatePid ! {statusUpdate, X, Y, IsFree}
                                end;
                                % TODO: handle parking
                            false -> moveY(Y, NewGoalY, X, GridHeight, StatePid)
                        end;
                false -> case Y =:= NewGoalY of 
                            true -> moveX(X, NewGoalX, Y, GridWidth, StatePid);
                            false -> 
                                Axis = rand:uniform(2),
                                case Axis of 
                                    1 -> moveX(X, NewGoalX, Y, GridWidth, StatePid);
                                    2 -> moveY(Y, NewGoalY, X, GridHeight, StatePid)
                                end
                        end
            end
    end.
    %move(NewGoalX, NewGoalY, StatePid, GridWidth, GridHeight).

moveX(X, NewGoalX, Y, GridWidth, StatePid) -> 
    case X < NewGoalX of
        true -> case abs(X - NewGoalX) < (GridWidth - lists:max([X, NewGoalX]) + lists:min([X, NewGoalX])) of
                true -> NewX = (X + 1) rem GridWidth;
                false -> NewX = (X - 1) rem GridWidth
            end;
        false -> case abs(X - NewGoalX) < (GridWidth - lists:max([X, NewGoalX]) + lists:min([X, NewGoalX])) of
                true -> NewX = (X - 1) rem GridWidth;
                false -> NewX = (X + 1) rem GridWidth
            end
    end,
    case NewX =:= 0 of
        true -> 
            UltimateX = 5,
            XPosRef = make_ref(),
            StatePid ! {updateMyPosition, UltimateX, Y, XPosRef};
        false ->
            UltimateX = NewX,
            XPosRef = make_ref(),
            StatePid ! {updateMyPosition, UltimateX, Y, XPosRef}
    end,
    render ! {position, StatePid, UltimateX, Y},
    IsFreeRef = make_ref(),
    ambient ! {isFree, StatePid, UltimateX, Y, IsFreeRef},
    receive
        {status, _, IsFree} -> StatePid ! {statusUpdate, UltimateX, Y, IsFree}
    end.

moveY(Y, NewGoalY, X, GridHeight, StatePid) ->
    case Y < NewGoalY of
        true -> case abs(Y - NewGoalY) < (GridHeight - lists:max([Y, NewGoalY]) + lists:min([Y, NewGoalY])) of
                    true -> NewY = (Y + 1) rem GridHeight;
                    false -> NewY = (Y - 1) rem GridHeight
                end;
        false -> case abs(Y - NewGoalY) < (GridHeight - lists:max([Y, NewGoalY]) + lists:min([Y, NewGoalY])) of
                    true -> NewY = (Y - 1) rem GridHeight;
                    false -> NewY = (Y + 1) rem GridHeight
                end
    end,
    case NewY =:= 0 of
        true -> 
            UltimateY = 5,
            YPosRef = make_ref(),
            StatePid ! {updateMyPosition, X, UltimateY, YPosRef};
        false ->
            UltimateY = NewY,
            YPosRef = make_ref(),
            StatePid ! {updateMyPosition, X, UltimateY, YPosRef}
    end,
    render ! {position, StatePid, X, UltimateY},
    IsFreeRef = make_ref(),
    ambient ! {isFree, StatePid, X, UltimateY, IsFreeRef},
    receive
        {status, _, IsFree} -> StatePid ! {statusUpdate, X, UltimateY, IsFree}
    end.


% sleep function
sleep(N) -> receive after N -> ok end.