% Un attore ambiente omniscente che rappresenta lo stato reale del mondo.
% In particolare l'ambiente conosce per ogni cella/posteggio il suo stato (libero o occupato).
% L'atomo ambient è registrato come PID dell'attore.

-module(ambient).
-export([ambient/3]).

ambient(Grid, W, H) ->
    receive 
        {isFree, PID, X, Y, Ref} -> 
            case maps:get({X, Y}, Grid) of
                none -> PID ! {status, Ref, true};
                {_, Boolean} -> PID ! {status, Ref, Boolean}
            end,
            ambient(Grid, W, H);
        {park, PID, X, Y, Ref} -> 
            % TODO: gestire parcheggi contemporanei killando PID
            UpdatedGrid = maps:update({X, Y}, {PID, false}, Grid),
            ambient(UpdatedGrid, W, H)
          % TODO: implementare {leave, PID, Ref}
    end.