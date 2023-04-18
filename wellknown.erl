% Un attore "wellknown" che viene contattato dalle automobili che entrano nel sistema di attori
% per ottenere i PID di altre automobili amiche da contattare nell'implementazione dell'algoritmo di gossiping.
% L'atomo wellkown è registrato come PID dell'attore.

-module(wellknown).
-export([wellknown/0]).

wellknown() -> io:format("## Wellknown\n").