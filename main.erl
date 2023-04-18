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
    AmbientPid = spawn(ambient, ambient, [Grid, 5, 5]),
    register(ambient, AmbientPid),
    io:format("# Ambient actor created and registered to 'ambient' atom with pid ~p\n", [AmbientPid]),

    % spawning and registering wellknown actor
    WellknownPid = spawn(wellknown, wellknown, []),
    register(wellknown, WellknownPid),
    io:format("# Wellknown actor created and registered to 'wellknown' atom with pid ~p\n", [WellknownPid]),
    
    % spawning and registering render actor
    RenderPid = spawn(render, render, [Grid, Width, Height]),
    register(render, RenderPid),
    io:format("# Render actor created and registered to 'render' atom with pid ~p\n", [RenderPid]),
    
    % main loop
    loop(3000, Width, Height).

% loop that spawns and kills cars each N seconds
loop(N, Width, Height) -> 
    sleep(N),
    % spawn a car
    % TODO: what if position is already occupied?
    NewX = rand:uniform(Width),
    NewY = rand:uniform(Height),
    CarPid = spawn(car, main, [NewX, NewY]),
    sleep(N),
    % kill a car
    loop(N, Width, Height).

% sleep function
sleep(N) -> receive after N -> ok end.
