% Un attore "render" che permette il debugging raccogliendo informazioni dagli altri attori per poterle visualizzare.
% Per permettere il debugging, dovrete implementare un ultimo attore "render", il cui PID viene registrato associandolo
% all'atomo rendere che si occupa di rappresentare lo stato del sistema di attori.
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
-export([render/3]).

% each cell is either {Pid, isParked} or none (if none the cell is empty)
render(CarsPositionsMap, W, H) ->
    receive 
        {position, Pid, X, Y} ->
            NewCarsPositionsMap = maps:put(Pid, {{X, Y}, false}, CarsPositionsMap),
            render(NewCarsPositionsMap, W, H);
        {parked, Pid, X, Y, IsParked} -> 
            NewCarsPositionsMap = maps:put(Pid, {{X, Y}, IsParked}, CarsPositionsMap),
            render(NewCarsPositionsMap, W, H);
        {target, Pid, X, Y} ->
            io:format("Car ~p wants to park in {~p, ~p}\n", [Pid, X, Y]),
            render(CarsPositionsMap, W, H);
        {draw} -> 
            BGrid = maps:from_list([{{X,Y}, none} || X <- lists:seq(1, W), Y <- lists:seq(1, H)]),
            CarsPList = maps:to_list(CarsPositionsMap),
            UpdateGridFun = fun(K,_) -> 
                    PidList = [ {P, IsParked} || {P, {Coord, IsParked}} <- CarsPList, Coord =:= K],
                    case length(PidList) =:= 0 of
                        true ->  none;
                        false -> lists:nth(1, PidList)
                    end
                end,
            NewGrid = maps:map(UpdateGridFun, BGrid),
            % maps:to_List returns a list in arbitrary order, so we order by the key
                % sort so that it prints {1,1}, {2,1}, {3,1},... instead of {1,1}, {1,2}, {1,3},...  
            List = lists:sort(fun({{X1, Y1},V1}, {{X2, Y2},V2}) -> {{Y1, X1},V1} =< {{Y2, X2},V2} end, maps:to_list(NewGrid)),
            draw(List, 1, W),
            render(CarsPositionsMap, W, H)
    end.

draw([], _, _) -> io:format("~nLegend:~n    * '*': empty spaces~n    * 'X': car parked~n    * 'O': moving car~n~n");
draw([{_, Value}|TL], Now, W) when Now rem W =:= 0 ->
    Symbol = case Value of
        none -> '*';
        {_, true} -> 'X';
        {_, false} -> 'O' end,
    io:format("~s ~n", [Symbol]),
    draw(TL, Now+1, W);
draw([{_, Value}|TL], Now, W) ->
    Symbol = case Value of
        none -> '*';
        {_, true} -> 'X';
        {_, false} -> 'O' end,
    io:format("~s ", [Symbol]),
    draw(TL, Now+1, W).