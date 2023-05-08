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
    NumberOfCars = 6,
    createCars(NumberOfCars, [], Width, Height, 20000).

% create cars recursively and keep trace of their Pids
createCars(NumberOfCars, CarsPids, Width, Height, SleepTime) when NumberOfCars > 0 ->
    % spawn a car
    NewX = rand:uniform(Width),
    NewY = rand:uniform(Height),
    CarPid = spawn(car, main, [NewX, NewY, Width, Height]),
    NewCarsPids = [CarPid | CarsPids],
    createCars(NumberOfCars-1, NewCarsPids, Width, Height, SleepTime);
% once finished call main loop
createCars(NumberOfCars, CarsPids, Width, Height, SleepTime) when NumberOfCars =:= 0 -> 
    loop(SleepTime, CarsPids, Width, Height).

% main loop that spawns and kills cars each N seconds
loop(N, CarsPids, Width, Height) -> 
    io:format("Cars: ~p\n", [CarsPids]),
    sleep(N),
    % spawn a car
    NewX = rand:uniform(Width),
    NewY = rand:uniform(Height),
    CarPid = spawn(car, main, [NewX, NewY, Width, Height]),
    NewCarsPids = [CarPid | CarsPids],
    % kill one car
    killCars(NewCarsPids, N, Width, Height).

% function that kills the selected car and calls the loop back
killCars(CarsPids, N, Width, Height) when length(CarsPids) > 1 ->
    % choose the index of one car to kill 
    CarIndexToKill = rand:uniform(length(CarsPids) - 1),
    {Left, [PidToKill|Right]} = lists:split(CarIndexToKill, CarsPids),
    io:format("Main killing ~p\n", [PidToKill]),
    exit(PidToKill, kill),
    loop(N, Left ++ Right, Width, Height);
killCars(CarsPids, N, Width, Height) when length(CarsPids) =< 1 ->
    loop(N, CarsPids, Width, Height).

% sleep function
sleep(N) -> receive after N -> ok end.
