% Un attore ambiente omniscente che rappresenta lo stato reale del mondo.
% In particolare l'ambiente conosce per ogni cella/posteggio il suo stato (libero o occupato).
% L'atomo ambient è registrato come PID dell'attore.

-module(ambient).
-export([ambient/3]).

ambient(Grid, W, H) ->    
    receive 
        % Richiesta da parte di detect se il posteggio è libero.
        {isFree, PID, X, Y, Ref} -> 
            case maps:get({X, Y}, Grid) of
                % Il booleano IsFree vale true sse il posteggio è libero.
                none -> PID ! {status, Ref, true};
                {_, Boolean} -> PID ! {status, Ref, Boolean}
            end,
            ambient(Grid, W, H);
        %  Notifica da parte di detect che l'automobile sta parcheggiando.
        {park, PID, X, Y, Ref} ->
            % TODO: nel caso in cui due automobili arrivino contemporanemante al posteggio e inviino entrambe un messaggio park,
            % l'ambiente assegnerà il posteggio a quella arrivata per prima, killando la seconda automobile.
            case maps:get({X, Y}, Grid) of
                none -> io:format("");  % Do nothing
                true -> io:format("");  % Do nothing
                {_, false} ->
                    % TODO: kill car.
                    ambient(Grid, W, H)
            end,
            UpdatedGrid = maps:update({X, Y}, {PID, false}, Grid),
            render ! {parked, PID, X, Y, true},
            % ambient(UpdatedGrid, W, H),
            % TODO: durante il parcheggio, l'attore ambient monitora l'automobile parcheggiata in modo da liberare
            % il posteggio qualora l'attore automobile venga killato.
            receive
                %  Notifica da parte di detect che l'automobile sta lasciando il posteggio.
                {leave, PID, Ref} ->
                    NUpdatedGrid = maps:update({X, Y}, {PID, true}, UpdatedGrid),
                    render ! {parked, PID, X, Y, false},
                    ambient(NUpdatedGrid, W, H)
            end
            % ambient(UpdatedGrid, W, H)
    end.