% Un attore "render" che permette il debugging raccogliendo informazioni dagli altri attori per poterle visualizzare.

-module(render).
-export([render/3, draw/3]).

% each cell is either {Pid, isFree} or none (if none the cell is empty)
render(Grid, W, H) ->
    %Grid2 = maps:update({3,1}, {123, false}, Grid),
    %Grid3 = maps:update({5,5}, {456, true}, Grid2),
    % maps:to_List returns a list in arbitrary order, so we order by the key
        % sort so that it prints {1,1}, {2,1}, {3,1},... instead of {1,1}, {1,2}, {1,3},...  
    receive {position, Pid, X, Y} ->
        NewGrid = maps:update({X,Y}, {Pid, true}, Grid),
        List = lists:sort(fun({{X1, Y1},V1}, {{X2, Y2},V2}) -> {{Y1, X1},V1} =< {{Y2, X2},V2} end, maps:to_list(NewGrid)),
        draw(List, 1, W),
        render(NewGrid, W, H)
    end.

draw([], _, _) -> io:format("~nLegend:~n    * '*': empty spaces~n    * 'X': car parked~n    * 'O': moving car~n~n");
draw([{_, Value}|TL], Now, W) when Now rem W =:= 0 ->
    Symbol = case Value of
        none -> '*';
        {_, true} -> 'O';
        {_, false} -> 'X' end,
    io:format("~s ~n", [Symbol]),
    draw(TL, Now+1, W);
draw([{_, Value}|TL], Now, W) ->
    Symbol = case Value of
        none -> '*';
        {_, true} -> 'O';
        {_, false} -> 'X' end,
    io:format("~s ", [Symbol]),
    draw(TL, Now+1, W).