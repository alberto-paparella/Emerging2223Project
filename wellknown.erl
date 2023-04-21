% Un attore "wellknown" che viene contattato dalle automobili che entrano nel sistema di attori
% per ottenere i PID di altre automobili amiche da contattare nell'implementazione dell'algoritmo di gossiping.
% L'atomo wellkown è registrato come PID dell'attore.

-module(wellknown).
-export([wellknown/1]).

wellknown(PidList) when PidList /= [] -> 
    receive
        {getFriends, PID1, PID2, Ref} ->
            % send to PID1 PidList
            PID1 ! {myFriends, PidList, Ref},
            % check if PID2 is in PidList (add new members)
            case lists:member(PID2, PidList) of
                % if true, call wellknown on the same PidList
                true -> wellknown(PidList);
                % if fase, call wellknown with [PID2 | PidList]
                false -> wellknown([PID2 | PidList])
            end
    end.

% TODO: delete car if it is killed