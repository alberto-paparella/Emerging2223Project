% Un attore "render" che permette il debugging raccogliendo informazioni dagli altri attori per poterle visualizzare.

-module(render).
-export([render/3]).

% each cell is either {Pid, isFree} or none (if none the cell is empty)
render(CarsPositionsMap, W, H) ->
    receive 
        {position, Pid, X, Y} ->
            NewCarsPositionsMap = maps:put(Pid, {X, Y}, CarsPositionsMap),
            render(NewCarsPositionsMap, W, H);
        {draw} -> 
            BGrid = maps:from_list([{{X,Y}, none} || X <- lists:seq(1, W), Y <- lists:seq(1, H)]),
            CarsPList = maps:to_list(CarsPositionsMap),
            UpdateGridFun = fun(K,V) -> maps:update(K, {V, true}, BGrid) end,
            NewGrid = maps:map(UpdateGridFun, BGrid),
            % CarsP     PID1 -> {0,0}  
            % BGrid     {0,0} -> none

            % maps:to_List returns a list in arbitrary order, so we order by the key
            io:format("New Grid:\n~p\n\n\n", [NewGrid]),
                % sort so that it prints {1,1}, {2,1}, {3,1},... instead of {1,1}, {1,2}, {1,3},...  
            List = lists:sort(fun({{X1, Y1},V1}, {{X2, Y2},V2}) -> {{Y1, X1},V1} =< {{Y2, X2},V2} end, maps:to_list(NewGrid)),
            draw(List, 1, W),
            render(CarsPositionsMap, W, H)
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