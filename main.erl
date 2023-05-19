-module(main).
-export([main/0]).

%%% main/0
% funzione pubblica la quale, una volta invocata:
% - crea l'attore "ambient" che si registra usando l'atomo ambient
% - crea l'attore "wellknown" che si registra usando l'atomo wellknown
% - crea l'attore "render" che si registra usando l'atomo render
% - crea un numero congruo di automobili assegnando a ognuna una posizione iniziale
%   casuale e ricordandosi l'insieme dei PID dei loro attori "main"
% - a intervalli regolari sceglie casualmente delle automobili e le killa,
%   rimpiazzandole con nuove automobili create in posizione casuale
main() -> 
    io:format("# Main started\n"),
    Width = 30, Height = 10,    % dimensioni della scacchiera
    NCarsToCreate = 15,          % numero di macchine presenti nella scacchiera
    NCarsToKill = 3,            % numero di macchine da killare e spawnare a intervalli regolari
    SleepTime = 10000,          % intervallo di ms fra una kill/spawn e la successiva
    spawn(ambient, ambient, []),
    spawn(wellknown, wellknown, []),
    spawn(render, render, [Width, Height]),
    create_cars(NCarsToCreate, [], Width, Height, SleepTime, NCarsToKill).

%%% create_cars/6
% crea "NCarsToCreate" automobili in maniera ricorsiva assegnando a ognuna una
% posizione iniziale casuale e tenendo traccia dei PID dei loro attori "main"
create_cars(NCarsToCreate, CarsPids, Width, Height, SleepTime, NCarsToKill) when NCarsToCreate > 0 ->
    NewX = rand:uniform(Width),
    NewY = rand:uniform(Height),
    CarPid = spawn(car, main, [NewX, NewY, Width, Height]),
    NewCarsPids = [CarPid | CarsPids],
    create_cars(NCarsToCreate-1, NewCarsPids, Width, Height, SleepTime, NCarsToKill);
% dopo aver creato "NCarsToCreate" automobili si sopsende per
% "SleepTime" ms, dopodiché cambia behaviour in "kill_cars"
create_cars(NCarsToCreate, CarsPids, Width, Height, SleepTime, NCarsToKill) when NCarsToCreate =:= 0 -> 
    io:format("Cars: ~p\n", [CarsPids]),
    sleep(SleepTime),
    kill_cars(NCarsToKill, NCarsToKill, CarsPids, SleepTime, Width, Height).

%%% kill_cars/6
% killa "NCarsToKill" automobili, dopodiché cambia behaviour in create_cars per crearne altrettante
kill_cars(NCarsToKill, NCarsToCreate, CarsPids, SleepTime, Width, Height) when length(CarsPids) > 1 ->
    CarIndexToKill = rand:uniform(length(CarsPids) - 1),
    {Left, [PidToKill|Right]} = lists:split(CarIndexToKill, CarsPids),
    io:format("Main killing ~p\n", [PidToKill]),
    exit(PidToKill, kill),
    case NCarsToKill > 1 of
        true -> kill_cars(NCarsToKill - 1, NCarsToCreate, Left ++ Right, SleepTime, Width, Height);
        false -> create_cars(NCarsToCreate, Left ++ Right, Width, Height, SleepTime, NCarsToCreate)
    end;
kill_cars(_, NCarsToCreate, CarsPids, SleepTime, Width, Height) when length(CarsPids) =< 1 ->
    create_cars(NCarsToCreate, CarsPids, Width, Height, SleepTime, NCarsToCreate).

%%% sleep/1
% Sospende l'esecuzione per N ms
sleep(N) -> receive after N -> ok end.