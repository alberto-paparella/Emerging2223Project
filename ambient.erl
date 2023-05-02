% Un attore ambiente omniscente che rappresenta lo stato reale del mondo.
% In particolare l'ambiente conosce per ogni cella/posteggio il suo stato (libero o occupato).
% L'atomo ambient è registrato come PID dell'attore.

-module(ambient).
-export([ambient/0]).

ambient() ->
    % Grid è una mappa che contiene un elemento {PID, {X, Y}} per ogni macchina parcheggiata
    % Se non vi è un elemento di valore {X, Y}, allora quella cella è libera
    Grid = #{},
    ambient(Grid).

ambient(Grid) ->
    receive         
        % Richiesta da parte di detect se il posteggio è libero.
        {isFree, PID, X, Y, Ref} ->
            case length([{A,B}||{A,B}<-maps:values(Grid), A =:= X, B =:=Y]) of
                % Il booleano IsFree vale true sse il posteggio è libero.
                0 -> PID ! {status, Ref, true};
                1 -> PID ! {status, Ref, false}
            end,
            ambient(Grid);
        %  Notifica da parte di detect che l'automobile sta parcheggiando.
        {park, PID, X, Y, Ref} ->
            % TODO: nel caso in cui due automobili arrivino contemporanemante al posteggio e inviino entrambe un messaggio park,
            % l'ambiente assegnerà il posteggio a quella arrivata per prima, killando la seconda automobile.
            % case maps:get({X, Y}, Grid, none) of
            %     none -> io:format("");  % Do nothing
            %     true -> io:format("");  % Do nothing
            %     {_, false} ->
            %         % TODO: kill car.
            %         ambient(Grid)
            % end,
            UpdatedGrid = maps:put(PID, {X, Y}, Grid),
            render ! {parked, PID, X, Y, true},
            ambient(UpdatedGrid);
            % TODO: durante il parcheggio, l'attore ambient monitora l'automobile parcheggiata in modo da liberare
            % il posteggio qualora l'attore automobile venga killato.
        %  Notifica da parte di detect che l'automobile sta lasciando il posteggio.
        {leave, PID, Ref} ->
            UpdatedGrid = maps:remove(PID, Grid),
            {X,Y} = maps:get(PID, Grid),
            render ! {parked, PID, X, Y, false},
            ambient(UpdatedGrid)
    end.