% Autor:JOSE EDUARDO ACOSTA SOSA
% Fecha: 25/05/2022
%regresa una lista con las longitudes de las sublistas
sumar([],ACUMULAR,ACUMULAR).
sumar([P|R],ACUMULAR,TOTAL):- X is ACUMULAR + P,
                              sumar(R,X,TOTAL).
longitud_lista([],0).
longitud_lista([_|Xs], LISTA):-longitud_lista(Xs, L2),
                               LISTA is L2 + 1.
suma_lista([],0).
suma_lista([_|Xs],LISTA,):-suma_lista(Xs, L2),
                           LISTA is L2 + 1,
                           sumar([_|Xs],0,LISTA).
