% Un attore ambiente omniscente che rappresenta lo stato reale del mondo.
% In particolare l'ambiente conosce per ogni cella/posteggio il suo stato (libero o occupato).
% L'atomo ambient è registrato come PID dell'attore.

-module(ambient).
-export([ambient/2, listen/1, park/3]).

ambient(W,H) ->
    Grid = maps:from_list([{{X,Y}, 0} || X <- lists:seq(0, W), Y <- lists:seq(0, H)]),
    io:format("~p",[Grid]),
    listen(Grid).

listen(Grid) ->
    self() ! {2,1},
    receive {X,Y} -> %park(X,Y,Grid)
        Grid2 = maps:update({X,Y}, 1, Grid),
        io:format("~p",[Grid2])
        %listen(Grid2)
    end.

park(X,Y,Grid) ->
    Grid = maps:update({X,Y}, 1, Grid),
    io:format("~p",[Grid]),
    listen(Grid).