% Il progetto esporterà una funzione pubblica main/0 la quale, una volta invocata:
% - crea l'attore "ambient" che si registra usando l'atomo ambient
% - crea l'attore "wellKnown" che si registra usando l'atomo wellKnown
% - crea l'attore "render" che si registra usando l'atomo render
% - crea un numero congruo di automobili assegnando a ognuna una posizione iniziale casuale e ricordandosi l'insieme dei PID dei loro attori "main"
% - a intervalli regolari sceglie casualmente delle automobili e le killa, rimpiazzandole con nuove automobili create in posizione casuale

-module(main).
-export([main/0]).


main() -> 
    io:format("# Main started\n"),
    
    % initialize grid
    Width = 10,
    Height = 10,

    % set SleepTime
    SleepTime = 10000, %ms

    % spawning and registering ambient actor
    AmbientPid = spawn(ambient, ambient, []),
    register(ambient, AmbientPid),
    io:format("# Ambient actor created and registered to 'ambient' atom with pid ~p\n", [AmbientPid]),

    % spawning and registering wellKnown actor
    WellknownPid = spawn(wellKnown, wellKnown, [[]]),
    register(wellKnown, WellknownPid),
    io:format("# Wellknown actor created and registered to 'wellKnown' atom with pid ~p\n", [WellknownPid]),
    
    % spawning and registering render actor
    RenderPid = spawn(render, render, [Width, Height]),
    register(render, RenderPid),
    io:format("# Render actor created and registered to 'render' atom with pid ~p\n", [RenderPid]),
    
    % spawning default number of cars
    NCarsToCreate = 6,
    createCars(NCarsToCreate, [], Width, Height, SleepTime).

% create cars recursively and keep trace of their Pids
createCars(NCarsToCreate, CarsPids, Width, Height, SleepTime) when NCarsToCreate > 0 ->
    % spawn a car
    NewX = rand:uniform(Width),
    NewY = rand:uniform(Height),
    CarPid = spawn(car, main, [NewX, NewY, Width, Height]),
    NewCarsPids = [CarPid | CarsPids],
    createCars(NCarsToCreate-1, NewCarsPids, Width, Height, SleepTime);
% once finished call main loop
createCars(NCarsToCreate, CarsPids, Width, Height, SleepTime) when NCarsToCreate =:= 0 -> 
    loop(SleepTime, CarsPids, Width, Height).

% main loop that spawns and kills cars each N seconds
loop(SleepTime, CarsPids, Width, Height) -> 
    io:format("Cars: ~p\n", [CarsPids]),
    sleep(SleepTime),
    % kill N cars (and, afterwards, spawn M cars)
    killCars(3, 3, CarsPids, SleepTime, Width, Height).

% function that kills the selected car and calls the loop back
killCars(NCarsToKill, NCarsToCreate, CarsPids, SleepTime, Width, Height) when length(CarsPids) > 1 ->
    % choose the index of one car to kill 
    CarIndexToKill = rand:uniform(length(CarsPids) - 1),
    {Left, [PidToKill|Right]} = lists:split(CarIndexToKill, CarsPids),
    io:format("Main killing ~p\n", [PidToKill]),
    exit(PidToKill, kill),
    case NCarsToKill > 1 of
        true -> killCars(NCarsToKill - 1, NCarsToCreate, Left ++ Right, SleepTime, Width, Height);
        false ->
            % spawn M cars
            createCars(NCarsToCreate, CarsPids, Width, Height, SleepTime)
    end;
killCars(_, NCarsToCreate, CarsPids, SleepTime, Width, Height) when length(CarsPids) =< 1 ->
    % spawn M cars
    createCars(NCarsToCreate, CarsPids, Width, Height, SleepTime).

% sleep function
sleep(N) -> receive after N -> ok end.
