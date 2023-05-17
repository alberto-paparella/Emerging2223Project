% Un attore "render" che permette il debugging raccogliendo informazioni dagli altri attori per poterle visualizzare.
% Per permettere il debugging, dovrete implementare un ultimo attore "render", il cui PID viene registrato associandolo
% all'atomo render e che si occupa di rappresentare lo stato del sistema di attori.
% In particolare il "render" visualizza la griglia, indicando la posizione di ogni automobile, il suo stato (parcheggiata o meno),
% quale sia il posteggio obiettivo di ogni automobile e la lista di amici di ogni automobile.
% Queste informazioni vanno comunicate all'attore render dagli altri attori tramite i seguenti messaggi:
%  - {position, PID, X, Y} la posizione dell'automobile, inviato dall'attore "detect"
%  - {target, PID, X, Y} la posizione del posteggio obiettivo dell'automobile, inviato dall'attore "detect"
%  - {parked, PID, X, Y, IsParked} inviata dall'attore "ambient" quando l'auto parcheggia/riparte
%  - {friends, PID, PIDLIST} inviata dall'attore "friendship" quando cambia la lista di amici
%  - In tutti i messaggi qui sopra si deve utilizzare lo stesso PID per identificare le automobili (es. il PID dell'attore "state" o quello dell'attore "main").
% La resa delle informazioni è liberamente implementabile a vostro gusto.
% Per esempio, potrebbe essere un'"immagine" in ASCII art seguita da una leggenda, rigenerata e mostrata a intervalli regolari.

-module(render).
-export([render/2, render_loop/0]).

%%% renderLoop/0
render_loop() ->
    sleep(1000),
    render ! {draw},
    render_loop().

%%% render/2
render(W, H) ->
    % Si registra usando l'atomo render
    register(render, self()),
    io:format("# Render actor created and registered to 'render' atom with pid ~p\n", [self()]),
    spawn_link(?MODULE, render_loop, []),
    render(#{}, W, H).

%%% render/3
render(CarsPositionsMap, W, H) ->
    % A ogni automobile corrisponde un elemento #{Pid -> {{X, Y}, IsParked}} dove {X, Y} è la sua
    % posizione sulla scacchiera, e IsParked vale true se l'automobile è parcheggiata, false altrimenti
    receive
        % {position, PID, X, Y} la posizione dell'automobile, inviato dall'attore "detect"
        {position, PID, X, Y} ->
            monitor(process, PID),
            NewCarsPositionsMap = maps:put(PID, {{X, Y}, false}, CarsPositionsMap),
            render(NewCarsPositionsMap, W, H);
        % {target, PID, X, Y} la posizione del posteggio obiettivo dell'automobile, inviato dall'attore "detect"
        {target, PID, X, Y} ->
            io:format("Car ~p wants to park in {~p, ~p}\n", [PID, X, Y]),
            render(CarsPositionsMap, W, H);
        % {parked, PID, X, Y, IsParked} inviata dall'attore "ambient" quando l'auto parcheggia/riparte
        {parked, PID, X, Y, IsParked} -> 
            monitor(process, PID),
            NewCarsPositionsMap = maps:put(PID, {{X, Y}, IsParked}, CarsPositionsMap),
            render(NewCarsPositionsMap, W, H);
        % {friends, PID, PIDLIST} inviata dall'attore "friendship" quando cambia la lista di amici
        {friends, PID, PIDLIST} ->
            io:format("Car ~p new friend list:\n~p\n", [PID,PIDLIST]),
            render(CarsPositionsMap, W, H);
        % Notifica che un'automobile è morta
        {'DOWN', Ref, process, Pid, _} ->
            demonitor(Ref),
            NewCarsPositionsMap = maps:remove(Pid, CarsPositionsMap),
            render(NewCarsPositionsMap, W, H);
        % Richiesta da parte di render_loop di stampare la situazione attuale della scacchiera
        {draw} -> 
            BGrid = maps:from_list([{{X,Y}, none} || X <- lists:seq(1, W), Y <- lists:seq(1, H)]),
            CarsPList = maps:to_list(CarsPositionsMap),
            UpdateGridFun = fun(K,_) -> 
                    PidList = [ {P, IsParked} || {P, {Coord, IsParked}} <- CarsPList, Coord =:= K],
                    case length(PidList) =:= 0 of
                        true ->  none;
                        false -> case length(PidList) > 1 of
                            true -> two_or_more;
                            false -> lists:nth(1, PidList)
                        end
                    end
                end,
            NewGrid = maps:map(UpdateGridFun, BGrid),
            % Ordiniamo la lista sulle chiavi in modo da stampare {1,1}, {2,1}, {3,1},... invece di {1,1}, {1,2}, {1,3},...
            % (altrimenti per come funziona lists:sort, le righe e le colonne sarebbero invertite) 
            List = lists:sort(fun({{X1, Y1},V1}, {{X2, Y2},V2}) -> {{Y1, X1},V1} =< {{Y2, X2},V2} end, maps:to_list(NewGrid)),
            draw(List, 1, W),
            render(CarsPositionsMap, W, H)
    end.

%%% draw/3
% Stampa lo stato attuale della scacchiera e relativa legenda
draw([], _, _) -> io:format("~nLegend:~n    * '*': empty spaces~n    * 'X': car parked~n    * 'O': moving car\n    * 'U': more than one car in the cell~n~n");
draw([{_, Value}|TL], Now, W) when Now rem W =:= 0 ->
    Symbol = case Value of
        none -> '*';
        two_or_more -> 'U';
        {_, true} -> 'X';
        {_, false} -> 'O' end,
    io:format("~s ~n", [Symbol]),
    draw(TL, Now+1, W);
draw([{_, Value}|TL], Now, W) ->
    Symbol = case Value of
        none -> '*';
        two_or_more -> 'U';
        {_, true} -> 'X';
        {_, false} -> 'O' end,
    io:format("~s ", [Symbol]),
    draw(TL, Now+1, W).

%%% sleep/1
% Sospende l'esecuzione per N ms
sleep(N) -> receive after N -> ok end.