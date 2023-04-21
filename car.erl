% Ogni automobile è rappresentata dal seguente sotto-sistema di attori:
% - Un attore "main" che lancia gli altri attori ed è responsabile di ri-crearli nel caso di fallimento di uno di loro
% - Un attore "friendship" che si preoccupa di mantenere 5 attori nella lista di attori, integrandone di nuovi nel caso in cui il numero scenda.
% - Un attore "state" che si preoccupa di mantenere il modello interno dell'ambiente e le coordinate del posteggio obiettivo.
%   In pratica l'attore registra per ogni cella l'ultima informazione giunta in suo possesso (posteggio libero/occupato/nessuna informazione)
%   e propaga le nuove informazione ottenute agli amici (protocollo di gossiping). Inoltre cambia il posteggio obiettivo quando necessario
%   (es. quando scopre che il posteggio è ora occupato).
% - Un attore "detect" che si occupa di muovere l'automobile sulla scacchiera, interagendo con l'attore "ambient" per fare sensing dello stato
%   di occupazione dei posteggi.

-module(car).
-export([main/2]).

main(X, Y) ->
    % TODO: this message should be sent by detect actor, not main
    render ! {position, self(), X, Y},
    
    StatePid = spawn_link(?MODULE, state, [0,0,[]]),
    FriendshipPid = spawn_link(?MODULE, friendship, [[], StatePid]),
    DetectPid = spawn_link(?MODULE, state, []).

% if car hasn't any friends, ask wellknown for friends
friendship(FriendsList, StatePid) when FriendsList =:= [] -> 
    Ref = make_ref(),
    wellknown ! {getFriends, self(), StatePid, Ref},
    receive 
        {myFriends, PidList, Ref} ->
            % TODO: choose 5 Pids from PidList that are different from self()
            io:format("PidList: ~p~n", [PidList]),
            friendship(PidList, StatePid)
    end;
% if friends are > 0 and < 5 then ask for new friends to friends
friendship(FriendsList, StatePid) when length(FriendsList) < 5 -> io:format("");
% do nothing
friendship(FriendsList, StatePId) -> io:format("").

state(GoalX, GoalY, Grid) -> io:format("").

detect() -> io:format("").
