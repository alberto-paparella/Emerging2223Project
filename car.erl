% Ogni automobile è rappresentata dal seguente sotto-sistema di attori:
% - Un attore "main" che lancia gli altri attori ed è responsabile di ri-crearli nel caso di fallimento di uno di loro.
% - Un attore "friendship" che si preoccupa di mantenere 5 attori nella lista di attori, integrandone di nuovi nel caso in cui il numero scenda.
% - Un attore "state" che si preoccupa di mantenere il modello interno dell'ambiente e le coordinate del posteggio obiettivo.
%   In pratica l'attore registra per ogni cella l'ultima informazione giunta in suo possesso (posteggio libero/occupato/nessuna informazione)
%   e propaga le nuove informazione ottenute agli amici (protocollo di gossiping). Inoltre cambia il posteggio obiettivo quando necessario
%   (es. quando scopre che il posteggio è ora occupato).
% - Un attore "detect" che si occupa di muovere l'automobile sulla scacchiera, interagendo con l'attore "ambient" per fare sensing dello stato
%   di occupazione dei posteggi.

-module(car).
-export([main/4, friendship/2, detect/3, state/1]).

%%% main
% Un attore "main" che lancia gli altri attori ed è responsabile di ri-crearli nel caso di fallimento di uno di loro.

% Inizializzazione
main(X, Y, GridWidth, GridHeight) ->
    StatePid = spawn_link(?MODULE, state, [{X, Y}]),
    spawn_link(?MODULE, friendship, [StatePid, []]),    
    spawn_link(?MODULE, detect, [StatePid, GridWidth, GridHeight]).
    % TODO: ri-creazione degli attori nel caso di fallimento di uno di loro (modello dei fallimenti).

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
friendship(StatePid, FRIENDSLIST) when FRIENDSLIST =:= [] ->
    Ref = make_ref(),
    % Per il futuro algoritmo di gossiping, friendship deve comunicare il suo PID a state in modo che possano comunicare.
    StatePid ! {friendshipPid, self()},
    % Per inizializzare la lista di amici, la richiesta getFriends viene inviata all'attore wellKnown.
    wellKnown ! {getFriends, self(), StatePid, Ref},
    receive 
        {myFriends, PIDSLIST, Ref} ->
            NewFRIENDSLIST = makeFriends(StatePid, FRIENDSLIST, PIDSLIST),
            friendship(StatePid, NewFRIENDSLIST)            
    end;

% Ripristino
friendship(StatePid, FRIENDSLIST) when length(FRIENDSLIST) < 5 ->
    % Chiede ad ogni attore nella FRIENDSLIST la lista dei suoi amici e ne fa l'unione, dopodichè sceglie da tale insieme i 5 attori da usare come amici.
    % Nota: aggiunge alla lista degli amici solo quelli che gli mancano per arrivare a 5, mantenendo i precedenti.
    PIDSLIST = getFriends(StatePid, FRIENDSLIST, []),
    NewFRIENDSLIST = makeFriends(StatePid, FRIENDSLIST, PIDSLIST),
    % Qual'ora gli amici degli amici non siano sufficienti a ripristinare l'insieme di 5 amici, la richiesta getFriends viene inviata all'attore wellKnown.
    case length(NewFRIENDSLIST) < 5 of
        true ->
            Ref = make_ref(),
            wellKnown ! {getFriends, self(), StatePid, Ref},
            receive
                {myFriends, WellKnownPIDSLIST, Ref} ->
                    UltimateFRIENDSLIST = makeFriends(StatePid, NewFRIENDSLIST, WellKnownPIDSLIST),
                    friendship(StatePid, UltimateFRIENDSLIST)
            end;
        false -> friendship(StatePid, NewFRIENDSLIST)
    end;

% Default behaviour
friendship(StatePid, FRIENDSLIST) ->
    %io:format("Car (StatePid): ~p~n", [StatePid]),
    %io:format("FRIENDSLIST: ~p~n", [FRIENDSLIST]),
    receive
        {getFriends, PID1, PID2, Ref} ->
            PID1 ! {myFriends, FRIENDSLIST, Ref},
            % A seguito della ricezione di un messaggio getFriends, il ricevente può aggiungere alla sua lista di amici
            % il PID PID2 contenuto nel messaggio, sempre con l'obiettivo di mantenere una lista di 5 amici.
            % Nota: scegliamo di aggiungerlo dopo aver restituito myFriends in quanto verrebbe scartato dall'attore friendship mittente in ogni caso.
            case (length(FRIENDSLIST) < 5) and (lists:member({PID1, PID2}, FRIENDSLIST)) of
                true -> friendship(StatePid, FRIENDSLIST);
                false -> friendship(StatePid, [{PID1, PID2} | FRIENDSLIST])
            end;
        % Protocollo privato fra state e friendship della stessa automobile per permettere a state di ricevere la lista dei suoi amici.
        {getFriendsList, Ref} ->
            StatePid ! {friendsList, FRIENDSLIST, Ref}
    end.

%  Aggiunge casualmente un attore da PIDSLIST diverso da sè stesso non ancora contenuto nella lista degli amici.
makeFriend(StatePid, FRIENDSLIST, PIDSLIST) ->
    case length(PIDSLIST) /= 0 of
        true ->
            NewFriend = lists:nth(rand:uniform(length(PIDSLIST)), PIDSLIST),
            case (NewFriend =:= {self(), StatePid}) or (lists:member(NewFriend, FRIENDSLIST)) of
                true -> makeFriend(StatePid, FRIENDSLIST, lists:delete(NewFriend,PIDSLIST));
                false -> [NewFriend | FRIENDSLIST]
            end;
        false -> FRIENDSLIST
    end.

% Finchè la lista degli amici non contiene 5 amici, aggiunge casualmente un attore da PIDSLIST diverso da sè stesso non ancora contenuto nella lista degli amici.
makeFriends(StatePid, FRIENDSLIST, PIDSLIST) ->
    % Nota: questo corrisponde a un ciclo DO-WHILE in quanto in questo punto sicuramente è necessario aggiungere almeno un attore alla lista.
    % Questo ci permette di spostare il controllo all'interno e risparmiare una chiamata a ricorsiva.
    NewFRIENDSLIST = makeFriend(StatePid, FRIENDSLIST, PIDSLIST),
    case (length(NewFRIENDSLIST) < 5) and (length(FRIENDSLIST) /= length(NewFRIENDSLIST))  of
        true -> makeFriends(StatePid, NewFRIENDSLIST, PIDSLIST);
        false -> NewFRIENDSLIST
    end.

% Chiede ad ogni attore nella FRIENDSLIST la lista dei suoi amici e ne fa l'unione
getFriends(StatePid, FRIENDSLIST, NewFRIENDSLIST) ->
    case length(FRIENDSLIST) > 0 of
        true ->
            Ref = make_ref(),
            lists:nth(1,tuple_to_list(lists:last(FRIENDSLIST))) ! {getFriends, self(), StatePid, Ref},
            receive
                {myFriends, PIDSLIST, Ref} ->
                    getFriends(StatePid, lists:droplast(FRIENDSLIST), lists:merge([NewFRIENDSLIST, PIDSLIST]))
            end;
        false -> NewFRIENDSLIST
    end.

%%% detect
% Un attore "detect" che si occupa di muovere l'automobile sulla scacchiera, interagendo con l'attore "ambient" per fare sensing dello stato
% di occupazione dei posteggi.
%%% Protocolli per la Detection e il Parcheggio
% L'attore "detect" di un'automobile sceglie un posteggio obiettivo libero interagendo con l'attore "state".
% Dopodichè, ogni 2s, si avvicina di una cella verso tale obiettivo. Se deve muoversi lungo entrambi gli assi (x e y),
% lo fa scegliendo randomicamente l'asse e muovendosi nella direzione che minimizza la distanza percorsa.
% Dopo ogni movimento invia la richiesta:
%  - {isFree, PID, X, Y, Ref} all'attore ambient dove PID è il PID dell'attore che ne fa richiesta e Ref una nuova reference
%  - {status, Ref, IsFree} è la risposta da parte dell'ambiente all'attore il cui PID PID era contenuto nella richiesta.
% Il booleano IsFree vale true sse il posteggio è libero.
% In seguito alla ricezione del messaggio status, il messaggio viene condiviso con l'attore "state" tramite un protocollo privato.
% Nel caso in cui sia stato raggiunto il posteggio obiettivo e questo sia libero:
%  - {park, PID, X, Y, Ref} viene invato all'attore "ambient" per dire che l'automobile sta parcheggiando. Ref è una nuova reference.
%  - {leave, PID, Ref} viene inviato dopo 1-5s (valore scelto casualmente) all'attore "ambient" per dire che l'automobile sta lasciando il posteggio.
% La reference contenuta nel messaggio deve essere identica a quella del messaggio precedente.
% Nel caso in cui due automobili arrivino contemporanemante al posteggio e inviino entrambe un messaggio park, l'ambiente assegnerà il posteggio
% a quella arrivata per prima, killando la seconda automobile.
% Durante il parcheggio, l'attore ambient monitora l'automobile parcheggiata in modo da liberare il posteggio qualora l'attore automobile venga killato.
% Tramite un protocollo privato l'attore "detect" viene informato dall'attore "state" quando il parcheggio obiettivo divienta noto essere occupato,
% al fine di cambiare posteggio obiettivo scegliendone uno ritenuto libero.

% (Re-)Inizializzazione
detect(StatePid, GridWidth, GridHeight) ->
    % L'attore "detect" di un'automobile sceglie un posteggio obiettivo libero interagendo con l'attore "state".
    GoalX = rand:uniform(GridWidth),
    GoalY = rand:uniform(GridHeight),
    NewGoalRef = make_ref(),
    StatePid ! {newGoal, self(), {GoalX, GoalY}, NewGoalRef},
    receive
        {isGoalFree, IsGoalFree, NewGoalRef} ->
            case IsGoalFree of
                true -> 
                    % Notifico render sulla scelta del nuovo posteggio obiettivo
                    render ! {target, StatePid, GoalX, GoalY},
                    detect(StatePid, GridWidth, GridHeight, GoalX, GoalY);
                false -> detect(StatePid, GridWidth, GridHeight)
            end
    end.

% Default behaviour
detect(StatePid, GridWidth, GridHeight, GoalX, GoalY) ->
    % Dopodichè, ogni 2s, si avvicina di una cella verso tale obiettivo. Se deve muoversi lungo entrambi gli assi (x e y),
    % lo fa scegliendo randomicamente l'asse e muovendosi nella direzione che minimizza la distanza percorsa.
    sleep(2000),
    move(StatePid, GridWidth, GridHeight, GoalX, GoalY), 
    % Dopo ogni movimento invia la richiesta:
    %  - {isFree, PID, X, Y, Ref} all'attore ambient dove PID è il PID dell'attore che ne fa richiesta e Ref una nuova reference
    StateRef = make_ref(),
    StatePid ! {getMyPosition, self(), StateRef},
    receive
        {myPosition, {X, Y}, StateRef} ->
            %io:format("My position is: ~p~n", [{self(), X, Y}]),    % DEBUG
            IsFreeRef = make_ref(),
            ambient ! {isFree, self(), X, Y, IsFreeRef},
            receive
                %  - {status, Ref, IsFree} è la risposta da parte dell'ambiente all'attore il cui PID PID era contenuto nella richiesta.
                % Il booleano IsFree vale true sse il posteggio è libero.
                {status, IsFreeRef, IsFree} ->
                    %io:format("IsFree: ~p~n", [{self(), IsFree}]),    % DEBUG
                    % In seguito alla ricezione del messaggio status, il messaggio viene condiviso con l'attore "state" tramite un protocollo privato.
                    StatePid ! {status, self(), {X, Y}, IsFree, StateRef},
                    % A questo punto, l'attore state condivide se la cella obiettivo è ancora disponibile, e nel caso contrario scelgo un nuovo obiettivo.
                    % Tramite un protocollo privato l'attore "detect" viene informato dall'attore "state" quando il parcheggio obiettivo diventa
                    % noto essere occupato, al fine di cambiare posteggio obiettivo scegliendone uno ritenuto libero.
                    receive
                        {isGoalFree, IsGoalFree, StateRef} ->
                            case IsGoalFree of
                                false -> detect(StatePid, GridWidth, GridHeight);
                                true -> io:format("")   % Do nothing
                            end
                    end,
                    % Aggiorno il render sulla mia nuova posizione
                    render ! {position, StatePid, X, Y},
                    % Nel caso in cui sia stato raggiunto il posteggio obiettivo e questo sia libero:
                    %  - {park, PID, X, Y, Ref} viene invato all'attore "ambient" per dire che l'automobile sta parcheggiando. Ref è una nuova reference.
                    case (X =:= GoalX) and  (Y =:= GoalY) and (IsFree =:= true) of
                        true ->
                            ParkRef = make_ref(),
                            ambient ! {park, self(), X, Y, ParkRef},
                            % Notifica status che la cella è ora occupata (in modo da poterlo condividere durante il gossiping).
                            StatePid ! {status, self(), {X, Y}, false},
                            %  - {leave, PID, Ref} viene inviato dopo 1-5s (valore scelto casualmente) all'attore "ambient" per dire che l'automobile
                            % sta lasciando il posteggio. La reference contenuta nel messaggio deve essere identica a quella del messaggio precedente.
                            sleep(rand:uniform(5)),
                            ambient ! {leave, self(), ParkRef},
                            detect(StatePid, GridWidth, GridHeight);
                        false -> detect(StatePid, GoalX, GoalY, GridWidth, GridHeight)
                    end
            end
    end.    

% Movimento di una cella verso l'obiettivo. Se deve muoversi lungo entrambi gli assi (x e y),
% lo fa scegliendo randomicamente l'asse e muovendosi nella direzione che minimizza la distanza percorsa.
move(StatePid, GridWidth, GridHeight, GoalX, GoalY) -> 
    Ref = make_ref(),
    StatePid ! {getMyPosition, self(), Ref},
    receive 
        {myPosition, {X, Y}, Ref} ->
            case X =:= GoalX of
                true ->
                    moveY(Y, GoalY, X, GridHeight, StatePid);
                false ->
                    case Y =:= GoalY of 
                        true -> moveX(X, GoalX, Y, GridWidth, StatePid);
                        false -> 
                            Axis = rand:uniform(2),
                            case Axis of 
                                1 -> moveX(X, GoalX, Y, GridWidth, StatePid);
                                2 -> moveY(Y, GoalY, X, GridHeight, StatePid)
                            end
                    end
            end
    end.

% Movimento di una cella verso l'obiettivo lungo l'asse x nella direzione che minimizza la distanza percorsa.
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
            StatePid ! {updateMyPosition, {UltimateX, Y}};
        false ->
            UltimateX = NewX,
            StatePid ! {updateMyPosition, {UltimateX, Y}}
    end,
    render ! {position, StatePid, UltimateX, Y},
    IsFreeRef = make_ref(),
    ambient ! {isFree, self(), UltimateX, Y, IsFreeRef},
    receive
        {status, IsFreeRef, IsFree} -> StatePid ! {status, self(), {UltimateX, Y}, IsFree}
    end.

% Movimento di una cella verso l'obiettivo lungo l'asse y nella direzione che minimizza la distanza percorsa.
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
            StatePid ! {updateMyPosition, {X, UltimateY}};
        false ->
            UltimateY = NewY,
            StatePid ! {updateMyPosition, {X, UltimateY}}
    end,
    render ! {position, StatePid, X, UltimateY},
    IsFreeRef = make_ref(),
    ambient ! {isFree, self(), X, UltimateY, IsFreeRef},
    receive
        {status, IsFreeRef, IsFree} -> StatePid ! {status, self(), {X, UltimateY}, IsFree}
    end.

% Funzione di sleep
sleep(N) -> receive after N -> ok end.

%%% state
% Un attore "state" che si preoccupa di mantenere il modello interno dell'ambiente e le coordinate del posteggio obiettivo.
% In pratica l'attore registra per ogni cella l'ultima informazione giunta in suo possesso (posteggio libero/occupato/nessuna informazione)
% e propaga le nuove informazione ottenute agli amici (protocollo di gossiping). Inoltre cambia il posteggio obiettivo quando necessario
% (es. quando scopre che il posteggio è ora occupato).
%%% Protocollo di Gossiping
% L'attore "state" mantiene il modello del mondo, ricevendo update sia dall'attore "detect" (tramite messaggi status),
% sia dagli altri attori "state", via messaggi notifyStatus descritti fra poco.
% Quando l'update comporta una modifica del modello interno (es. un posteggio che si riteneva essere occupato ora
% diventa libero, o viceversa), tale cambiamento viene notificato a tutti gli amici tramite messaggi notifyStatus:
%  - {notifyStatus, X, Y, IsFree}
% Un protocollo privato permette all'attore "state" di ottenere la lista di amici correnti dall'attore "friendship".
% Il protocollo può essere ottimizzato per trasferire la lista solamente al cambiamento di questa.

% Inizializzatione (nel nostro modello state mantiene informazioni anche sulla posizione attuale dell'automobile)
state({MyX, MyY}) ->
    % Posteggio libero = true, occupato = false, nessuna informazione = none (non è presente nella Map)
    Grid = #{},
    % Per il futuro algoritmo di gossiping, state deve conoscere il PID di frienship in modo che possano comunicare
    receive
        {friendshipPid, PID} -> state(Grid, {MyX, MyY}, PID)
    end.

% Si mette in attesa della ricezione di un (nuovo) posteggio obiettivo da parte di detect
state(Grid, {MyX, MyY}, FriendshipPid) ->
    receive
        {newGoal, PID, {GoalX, GoalY}, Ref} ->
            % Controllo se è libero
            case maps:get({GoalX, GoalY}, Grid, none) of
                % Se non ha nessuna informazione per il momento si comporta come se fosse libera
                none -> PID ! {isGoalFree, true, Ref};
                true -> PID ! {isGoalFree, true, Ref};
                false ->
                    % Chiedo a detect un nuovo posteggio libero e mi metto in attesa di riceverlo
                    PID ! {isGoalFree, false, Ref},
                    state(Grid, {MyX, MyY}, FriendshipPid)
            end,
            state(Grid, {MyX, MyY}, {GoalX, GoalY}, FriendshipPid)
    end.

% Default behaviour
state(Grid, {MyX, MyY}, {GoalX, GoalY}, FriendshipPid) -> 
    receive
        {getMyPosition, PID, Ref} -> 
            PID ! {myPosition, {MyX, MyY}, Ref},
            state(Grid, {MyX, MyY}, {GoalX, GoalY}, FriendshipPid);
        {updateMyPosition, {NewX, NewY}} -> 
            state(Grid, {NewX, NewY}, {GoalX, GoalY}, FriendshipPid);
        {status, PID, {X, Y}, IsFree, Ref} ->
            io:format("DEBUG 1\t"),
            % Questa informazione modifica l'ambiente interno?
            case maps:get({X, Y}, Grid, none) of
                OldValue ->
                    case OldValue =:= IsFree of
                        true -> state(Grid, {MyX, MyY}, {GoalX, GoalY}, FriendshipPid);
                        false ->
                            % Chiede a friendship la lista dei suoi amici e invia notifyStatus ad ognuno di loro (gossiping)
                            GetFriendsListRef = make_ref(),
                            FriendshipPid ! {getFriendsList, GetFriendsListRef},
                            receive
                                {friendsList, FRIENDSLIST, GetFriendsListRef} ->
                                    io:format("DEBUG 2\t"),
                                    notifyFriends(FRIENDSLIST, {X, Y}, IsFree)
                            end,
                            NewGrid = maps:put({X, Y}, IsFree, Grid),
                            % Il posteggio obiettivo è ancora libero?
                            case maps:get({GoalX, GoalY}, Grid, none) of
                                % Se non ha nessuna informazione per il momento si comporta come se fosse libera
                                none -> PID ! {isGoalFree, true, Ref};
                                true -> PID ! {isGoalFree, true, Ref};
                                false ->
                                    % Chiedo a detect un nuovo posteggio libero e mi metto in attesa di riceverlo
                                    PID ! {isGoalFree, false, Ref},
                                    state(NewGrid, {MyX, MyY}, FriendshipPid)
                            end,
                            state(NewGrid, {MyX, MyY}, {GoalX, GoalY}, FriendshipPid)
                    end
            end;
        {notifyStatus, {X, Y}, IsFree} ->
            io:format("DEBUG 3\t"),
            % Questa informazione modifica l'ambiente interno?
            case maps:get({X, Y}, Grid, none) of
                OldValue ->
                    case OldValue =:= IsFree of
                        true -> state(Grid, {MyX, MyY}, {GoalX, GoalY}, FriendshipPid);
                        false ->
                            % Chiede a friendship la lista dei suoi amici e invia notifyStatus ad ognuno di loro (gossiping)
                            GetFriendsListRef = make_ref(),
                            FriendshipPid ! {getFriendsList, GetFriendsListRef},
                            receive
                                {friendsList, FRIENDSLIST, GetFriendsListRef} ->
                                io:format("DEBUG 4\t"),
                                    notifyFriends(FRIENDSLIST, {X, Y}, IsFree)
                            end,
                            NewGrid = maps:put({X, Y}, IsFree, Grid),
                            % TODO: in questo punto non posso notificare a detect di cambiare il goal (mi manca il PID)
                            state(NewGrid, {MyX, MyY}, {GoalX, GoalY}, FriendshipPid)
                    end
            end
    end.

% Notifica gli amici nella lista degli amici
notifyFriends(FRIENDSLIST, {X, Y}, IsFree) ->
    case length(FRIENDSLIST) > 0 of
        true ->
            lists:last(tuple_to_list(lists:last(FRIENDSLIST))) ! {notifyStatus, {X, Y}, IsFree},
            notifyFriends(lists:droplast(FRIENDSLIST), {X, Y}, IsFree);
        false -> io:format("")  % Do nothing
    end.