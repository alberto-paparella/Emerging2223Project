% Un attore "wellknown" che viene contattato dalle automobili che entrano nel sistema di attori per ottenere
% i PID di altre automobili amiche da contattare nell'implementazione dell'algoritmo di gossiping.
% L'atomo "wellknown" è registrato come PID dell'attore.

-module(wellknown).
-export([wellknown/0]).

wellknown() ->
    % Si registra usando l'atomo wellknown
    register(wellknown, self()),
    io:format("# Wellknown actor created and registered to 'wellknown' atom with pid ~p\n", [self()]),
    wellknown([]).

wellknown(PIDSLIST) ->
    receive
        % {getFriends, PID1, PID2, Ref} inviato da un attore "friendship" (il cui PID è PID1)
        % di un'automobile all'attore "friendship" di un'altra automobile
        % - PID2 è il PID dell'attore "state" dell'automobile mittente
        % - Ref è una nuova reference che identifica la richiesta
        {getFriends, PID1, PID2, Ref} ->
            % {myFriends, PIDSLIST, Ref} è la risposta al messaggio precedente,
            % inviata al PID PID1 contenuto nel messaggio di richiesta
            % Ref è la reference ricevuta nella richiesta
            % PIDSLIST è la lista di coppie {PIDF,PIDS} degli amici, dove PIDF è
            % il PID dell'attore "friendship" e PIDS quello dell'attore "state"
            PID1 ! {myFriends, PIDSLIST, Ref},
            % Se {PID1, PID2} non è contenuto nella lista PIDSLIST lo aggiungo
            % Nota: scegliamo di aggiungerlo dopo aver restituito myFriends in quanto
            % verrebbe scartato dall'attore friendship mittente in ogni caso
            case lists:member({PID1, PID2}, PIDSLIST) of
                true -> wellknown(PIDSLIST);
                false -> 
                    monitor(process, PID1),
                    io:format("Wellknown adding new car with friendship ~p\n", [PID1]),
                    wellknown([{PID1, PID2} | PIDSLIST])
            end;
        % Notifica che un'automobile presente in PIDSLIST è morta
        {'DOWN', Ref, process, Pid, _} ->
            demonitor(Ref),
            io:format("Wellknown deleting from its pids list car with friendship ~p\n", [Pid]),
            NewPidList = [{PID1, PID2} || {PID1, PID2} <- PIDSLIST, PID1 /= Pid],
            wellknown(NewPidList)
    end.