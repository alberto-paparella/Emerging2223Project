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

%%% main

main(X, Y, GridWidth, GridHeight) ->
    Grid = maps:from_list([{{N,M}, true} || N <- lists:seq(1, GridWidth), M <- lists:seq(1, GridHeight)]),
    StatePid = spawn_link(?MODULE, state, [Grid, X, Y]),

    % TODO: this message should be sent by detect actor, not main
    render ! {position, StatePid, X, Y},

    % Lancio l'attore friendship e registro l'atomo `friendship` come PID dell'attore
    friendship = spawn_link(?MODULE, friendship, [statePid, []]),
    
    DetectPid = spawn_link(?MODULE, detect, [StatePid, GridWidth, GridHeight]).

%%% friendship
% Un attore "friendship" che si preoccupa di mantenere 5 attori nella lista di attori, integrandone di nuovi nel caso in cui il numero scenda.
%%% Protocollo per la Friendship
% Il protocollo permette di chiedere agli amici la lista dei loro amici per poi farne l'unione e scegliere da tale insieme i 5 attori da usare come amici.
% Viene implementato dagli attori "friendship" e, per quanto riguarda la sola risposta, dall'attore speciale wellKnown.
%  - {getFriends, PID1, PID2, Ref} inviato da un attore "friendship" (il cui PID è PID1) di un'automobile all'attore "friendship" di un'altra automobile.
%    PID2 è il PID dell'attore "state" dell'automobile mittente. Ref è una nuova reference che identifica la richiesta
%  - {myFriends, PIDSLIST, Ref} è la risposta al messaggio precedente, inviata al PID PID1 contenuto nel messaggio di richiesta. Ref è la reference ricevuta nella richiesta.
%    PIDSLIST è la lista di coppie {PIDF,PIDS} degli amici, dove PIDF è il PID dell'attore "friendship" e PIDS quello dell'attore "state".
% Per inizializzare la lista di amici o qual'ora gli amici degli amici non siano sufficienti a ripristinare l'insieme di 5 amici, la richiesta getFriends viene inviata all'attore wellKnown.

% Inizializzazione
friendship(state, FRIENDSLIST) when FRIENDSLIST =:= [] ->
    Ref = make_ref(),
    % Per inizializzare la lista di amici, la richiesta getFriends viene inviata all'attore wellKnown.
    wellknown ! {getFriends, self(), state, Ref},
    receive 
        {myFriends, PIDSLIST, Ref} ->
            newFRIENDSLIST = makeFriends(state, FRIENDSLIST, PIDSLIST),
            io:format("Car (state): ~p~n", [state]),
            io:format("FRIENDSLIST: ~p~n", [newFRIENDSLIST]),
            friendship(state, newFRIENDSLIST)            
    end;

% Ripristino
friendship(state, FRIENDSLIST) when length(FRIENDSLIST) < 5 ->
    % Chiede ad ogni attore nella FRIENDSLIST la lista dei suoi amici e ne fa l'unione, dopodichè sceglie da tale insieme i 5 attori da usare come amici.
    % Nota: aggiunge alla lista degli amici solo quelli che gli mancano per arrivare a 5, mantenendo i precedenti.
    PIDSLIST = lists:usort(),
    newFRIENDSLIST = makeFriends(state, FRIENDSLIST, PIDSLIST),
    % Qual'ora gli amici degli amici non siano sufficienti a ripristinare l'insieme di 5 amici, la richiesta getFriends viene inviata all'attore wellKnown.
    case length(newFRIENDSLIST) < 5 of
        true ->
            Ref = make_ref(),
            wellKnown ! {getFriends, self(), state, Ref},
            receive
                {myFriends, wellKnownPIDSLIST, Ref} ->
                    ultimateFRIENDSLIST = makeFriends(state, newFRIENDSLIST, wellKnownPIDSLIST),
                    friendship(state, ultimateFRIENDSLIST)
            end;
        false -> friendship(state, newFRIENDLIST)
    end;

% Default behaviour
friendship(state, FRIENDSLIST) ->
    receive
        {getFriends, PID1, PID2, Ref} ->
            PID1 ! {myFriends, FRIENDSLIST, Ref},
            % A seguito della ricezione di un messaggio getFriends, il ricevente può aggiungere alla sua lista di amici
            % il PID PID2 contenuto nel messaggio, sempre con l'obiettivo di mantenere una lista di 5 amici.
            % Nota: scegliamo di aggiungerlo dopo aver restituito myFriends in quanto verrebbe scartato dall'attore friendship mittente in ogni caso.
            case length(FRIENDSLIST) < 5 and lists:member({PID1, PID2}, FRIENDSLIST) of
                true -> friendship(state, FRIENDSLIST);
                false -> friendship(state, [{PID1, PID2} | FRIENDSLIST])
            end
    end.

%  Aggiunge casualmente un attore da PIDSLIST diverso da sè stesso non ancora contenuto nella lista degli amici.
makeFriend(state, FRIENDSLIST, PIDSLIST) ->
    case length(PIDSLIST) /= 0 of
        true ->
            newFriend = lists:nth(rand:uniform(length(PIDSLIST)), PIDSLIST),
            case newFriend =:= {self(), state} or lists:member(newFriend, FRIENDSLIST) of
                true -> makeFriend(state, FRIENDSLIST, lists:delete(newFriend,PIDSLIST));
                false -> [newFriend | FRIENDSLIST]
            end;
        false -> FRIENDSLIST
    end.

% Finchè la lista degli amici non contiene 5 amici, aggiunge casualmente un attore da PIDSLIST diverso da sè stesso non ancora contenuto nella lista degli amici.
makeFriends(state, FRIENDSLIST, PIDSLIST) ->
    % Nota: questo corrisponde a un ciclo DO-WHILE in quanto in questo punto sicuramente è necessario aggiungere almeno un attore alla lista.
    % Questo ci permette di spostare il controllo all'interno e risparmiare una chiamata a ricorsiva.
    newFRIENDSLIST = makeFriend(state, FRIENDSLIST, PIDSLIST),
    case (length(newFRIENDSLIST) < 5) and (length(FRIENDSLIST) /= length(newFRIENDSLIST))  of
        true -> makeFriends(state, newFRIENDSLIST, PIDSLIST);
        false -> newFRIENDSLIST
    end.

% Chiede ad ogni attore nella FRIENDSLIST la lista dei suoi amici e ne fa l'unione
getFriends(state, FRIENDSLIST, newFRIENDSLIST) ->
    case length(FRIENDSLIST) > 0 of
        true ->
            Ref = make_ref(),
            lists:last(FRIENDSLIST) ! {getFriends, self(), state, Ref},
            receive
                {myFriends, PIDSLIST, Ref} ->
                    getFriends(state, lists:droplast(FRIENDSLIST), lists:usort(newFRIENDSLIST ++ PIDSLIST))
            end;
        false -> newFRIENDSLIST
    end.

%%% state

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
                detect(StatePid, NewGoalX, NewGoalY, GridWidth, GridHeight);
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
    ambient ! {isFree, self(), UltimateX, Y, IsFreeRef},
    receive
        {status, IsFreeRef, IsFree} -> StatePid ! {statusUpdate, UltimateX, Y, IsFree}
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
    ambient ! {isFree, self(), X, UltimateY, IsFreeRef},
    receive
        {status, IsFreeRef, IsFree} -> StatePid ! {statusUpdate, X, UltimateY, IsFree}
    end.


% sleep function
sleep(N) -> receive after N -> ok end.