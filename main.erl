%Il progetto esporterà una funzione pubblica main/0 la quale, una volta invocata:

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
    Width = 5,
    Height = 5,
    Grid = maps:from_list([{{X,Y}, none} || X <- lists:seq(1, Width), Y <- lists:seq(1, Height)]),

    % spawning and registering ambient actor
    AmbientPid = spawn(ambient, ambient, [Grid, Width, Height]),
    register(ambient, AmbientPid),
    io:format("# Ambient actor created and registered to 'ambient' atom with pid ~p\n", [AmbientPid]),

    % spawning and registering wellknown actor
    WellknownPid = spawn(wellknown, wellknown, [[]]),
    register(wellknown, WellknownPid),
    io:format("# Wellknown actor created and registered to 'wellknown' atom with pid ~p\n", [WellknownPid]),
    
    % spawning and registering render actor
    RenderPid = spawn(render, render, [Grid, Width, Height]),
    register(render, RenderPid),
    io:format("# Render actor created and registered to 'render' atom with pid ~p\n", [RenderPid]),
    
    % spawning default number of cars
    NumberOfCars = 5,
    createCars(NumberOfCars, [], Width, Height, 3000).

% create cars recursively and keep trace of their Pids
createCars(NumberOfCars, CarsPids, Width, Height, SleepTime) when NumberOfCars > 0 ->
    % spawn a car
    % TODO: what if position is already occupied?
    NewX = rand:uniform(Width),
    NewY = rand:uniform(Height),
    CarPid = spawn(car, main, [NewX, NewY]),
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
    % TODO: what if position is already occupied?
    NewX = rand:uniform(Width),
    NewY = rand:uniform(Height),
    CarPid = spawn(car, main, [NewX, NewY]),
    NewCarsPids = [CarPid | CarsPids],
    sleep(N),
    % choose the index of one car to kill 
    CarIndexToKill = rand:uniform(length(NewCarsPids) - 1),
    % kill it
    killCars(CarIndexToKill, NewCarsPids, N, Width, Height).

% function that kills the selected car and calls the loop back
killCars(CarIndexToKill, CarsPids, N, Width, Height) ->
    {Left, [PidToKill|Right]} = lists:split(CarIndexToKill, CarsPids),
    % TODO: Kill PidToKill
    NewCarsPids = Left ++ Right,
    loop(N, NewCarsPids, Width, Height).

% sleep function
sleep(N) -> receive after N -> ok end.
