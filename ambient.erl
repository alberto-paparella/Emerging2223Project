% Un attore ambiente omniscente che rappresenta lo stato reale del mondo.
% In particolare l'ambiente conosce per ogni cella/posteggio il suo stato (libero o occupato).
% L'atomo ambient è registrato come PID dell'attore.

-module(ambient).
-export([ambient/0]).

ambient() ->
    % Si registra usando l'atomo ambient
    register(ambient, self()),
    io:format("# Ambient actor created and registered to 'ambient' atom with pid ~p\n", [self()]),

    % Grid è una mappa #{Pid -> {X, Y, MonitorRef}} per ogni macchina parcheggiata
    % Se non vi è un elemento di valore {X, Y, Ref}, allora quella cella è libera
    Grid = #{},
    process_flag(trap_exit, true),
    ambient(Grid).

ambient(Grid) ->
    receive         
        % Richiesta da parte di detect se il posteggio è libero
        {isFree, PID, X, Y, Ref} ->
            case length([{A,B,C} || {A,B,C} <- maps:values(Grid), A =:= X, B =:=Y]) of
                % Il booleano restituito vale true sse il posteggio è libero
                0 -> PID ! {status, Ref, true};
                1 -> PID ! {status, Ref, false}
            end,
            ambient(Grid);
        % Notifica da parte di detect che l'automobile sta parcheggiando
        {park, PID, X, Y, _} -> %{park, PID, X, Y, Ref} ->
            % Nel caso in cui due (o più) automobili arrivino contemporanemante al posteggio
            % e inviino entrambe un messaggio park, l'ambiente assegnerà il posteggio a quella
            % arrivata per prima, killando la seconda automobile (le altre automobili)
            case length([{A,B,C} || {A,B,C} <- maps:values(Grid), A =:= X, B =:=Y]) of
                0 -> 
                    MonitorRef = monitor(process, PID),
                    UpdatedGrid = maps:put(PID, {X, Y, MonitorRef}, Grid),
                    render ! {parked, PID, X, Y, true},
                    ambient(UpdatedGrid);
                1 -> 
                    io:format("Ambient killing car ~p that wants to park in {~p, ~p}: another car is already parked.\n", [PID, X, Y]),
                    exit(PID, kill),
                    ambient(Grid)
            end;
        % Notifica da parte di detect che l'automobile sta lasciando il posteggio
        {leave, PID, _} -> %{leave, PID, Ref} ->
            UpdatedGrid = maps:remove(PID, Grid),
            {X,Y, MonitorRef} = maps:get(PID, Grid),
            demonitor(MonitorRef),
            render ! {parked, PID, X, Y, false},
            ambient(UpdatedGrid);
        % Notifica che un'automobile parcheggiata è morta
        {'DOWN', Ref, process, Pid, Reason} ->
            io:format("Ambient demonitoring parked car ~p. Reason: ~p\n", [Pid, Reason]),
            demonitor(Ref),
            UpdatedGrid = maps:remove(Pid, Grid),
            ambient(UpdatedGrid);
        % Protezione dalla morte delle macchine
        {'EXIT', From, Reason} -> 
            io:format("Ambient received exit message from ~p with reason ~p\n", [From, Reason]),
            UpdatedGrid = maps:remove(From, Grid),
            ambient(UpdatedGrid)
    end.