% Autor:
% Fecha: 23/11/2022

crearMochila(0, []).
crearMochila(C, Y):-
    C > 0,
    C1 is C-1,
    random(1, 3, U),
    V is U - 1,
    Y = [V|T],
    crearMochila(C1, T).

partir(X,I,Left,Right) :-
    length(Left,I),
    append(Left,Right,X).

reproducir(X,Y,Z1,Z2) :-
    length(X,Len),
    random_between(0,Len,Cut),
    partir(X,Cut,X1,X2),
    partir(Y,Cut,Y1,Y2),
    append(X1,Y2,Z1),
    append(Y1,X2,Z2).
    
