% Autor: Magdiel Guillermo Valdez
% Fecha: 09/11/2022

% Implementar el algoritmo genetico en prolog para resolver el problema de la mochila
%[comida(pan,4,1000),comida(refresco,8,5000),comida(galletas,4,2000),comida(manzana,5,1000),comida(chocolate,1,2000),comida(fritos,2,2000)]

%despensa(posicion,nombre,peso,calorias) 
despensa(1,pan,4,1000).
despensa(2,refresco,8,5000).
despensa(3,galletas,4,2000).
despensa(4,manzana,5,1000).
despensa(5,chocolate,1,2000).
despensa(6,fritos,2,2000).

%tamaño de la poblacion,tamaño de la mochila, generaciones a simular 
simulacion(1,100,6,500).

%crear mochila aleatorea
crearMochila(0, []).
crearMochila(C, Y):-
    C > 0,
    C1 is C-1,
    random(1, 3, U),
    V is U - 1,
    Y = [V|T],
    crearMochila(C1, T).
%crear una poblacion
crearPoblacion(0, C, []).
crearPoblacion(T, C, P):-
    T > 0,
    T1 is T-1,
    crearMochila(C,M),
    P = [M|R],
    crearPoblacion(T1, C, R).

%forzar a que haya un elemneto en la mochila en caso de que este vacia
hayUnos(X, [X|_]).
hayUnos(X, [_|Ys]):-
          hayUnos(X, Ys).
agragarUno([_|R],[1|R]).
mejoraMochila(Mochila,MMejorada) :-
    not(hayUnos(1,Mochila)),
    agragarUno(Mochila,MMejorada).
mejoraPoblacion([],PMejorada).
mejoraPoblacion([M|R],PMejorada):-
    not(mejoraMochila(M,MMejorada)),
    append(Resto,[M],PMejorada),
    %PMejorada = [M|Resto],
    mejoraPoblacion(R,Resto).
mejoraPoblacion([M|R],PMejorada):-
    mejoraMochila(M,MMejorada),
    append(Resto,[MMejorada],PMejorada),
    %PMejorada = [MMejorada|Resto],
    mejoraPoblacion(R,Resto).

%crear poblacion inicial
crearPoblacionInicial(PM):-
    simulacion(1,TP,TM,G),
    crearPoblacion(TP,TM,P),
    mejoraPoblacion(P,PM).

%reproducir dos mochilas
mutar([P1|R1],[P2|[S|R2]],Izquierda,Derecha):-
    P1 = 0,
    P3 is 1,
    S = 0,
    S2 is 1,
    Izquierda = [P3|R1],
    Derecha = [P2|[S2|R2]].
mutar([P1|R1],[P2|[S|R2]],Izquierda,Derecha):-
    P1 = 1,
    P3 is 0,
    S = 1,
    S2 is 0,
    Izquierda = [P3|R1],
    Derecha = [P2|[S2|R2]].
partir(X,I,Izquierda,Derecha) :-
    mutar(Iz,De,Izquierda,Derecha),
    length(Iz,I),
    append(Iz,De,X).
partir(X,I,Izquierda,Derecha) :-
    length(Izquierda,I),
    append(Izquierda,Derecha,X).
reproducir(X,Y,Z1,Z2) :-
    length(X,Len),
    random_between(0,Len,Corte),
    partir(X,Corte,X1,X2),
    partir(Y,Corte,Y1,Y2),
    append(X1,Y2,Z1),
    append(Y1,X2,Z2).

reproducirPoblacion([],_).
reproducirPoblacion([P|[S|R]],Hijos):-
    reproducir(P,S,H1,H2),
    nl,write("P  = "),write(P),nl,write("M  = "),write(S),nl,write("H1 = "),write(H1),nl,write("H2 = "),write(H2),nl,
    reproducirPoblacion(R,Otros),
    append(Otros,[H1|[H2]],Hijos).

repro:-
    crearPoblacionInicial(P),
    reproducirPoblacion(P,H).

%  calcularPesoYEnergia([1,0,0,0,0,0],P,C,1).
calcularPesoYEnergia([],0,0,7).
calcularPesoYEnergia([Producto | Resto], PesoTotal, CaloriasTotales, N) :-
    despensa(N,_,P,C),
    N1 is N + 1, 
    calcularPesoYEnergia(Resto,RestP,RestC, N1),
    PesoTotal is (P*Producto) + RestP,
    CaloriasTotales is (C*Producto) + RestC.

%fitness([1,0,1,0,1,1],F).
fitness(X,F) :-
    calcularPesoYEnergia(X,P,C,1),
    P > 0,
    F is C/P.

%obtenerFitnesMayor:-
%    crearPoblacionInicial(P),
%    fitnessMasAlto(P,0,PM,1),
%    nl,
%    fitness(PM,FM),
%    write(FM),
%    write(PM).
%fitnessPoblacion([]).
%fitnessPoblacion([X|R]) :-
%    fitness(X,F),
%    write(" "),
%    write(F),
%    write(" "),
%    fitnessPoblacion(R).

% mochilaConFitness([[1,0,0,0,0,0],[1,0,0,0,0,0],[1,0,0,0,0,0]],Nuev).
mochilaConFitness([],[]).
mochilaConFitness([M|R], NewLista):-
    fitnessUnaMochila(M,F),
    append([M],F,MYF),
    append(New,[MYF],NewLista),
    mochilaConFitness(R,New).

mostrarPoblacion([]).
mostrarPoblacion([[M|F]|R]):-
    write("mochila: "),
    write(M),
    write(" # "),
    write("fitness: "),
    write(F),
    nl,
    mostrarPoblacion(R).

evaluarPoblacionInicial:-
    crearPoblacionInicial(P),
    mochilaConFitness(P,MYF),
    mostrarPoblacion(MYF).



    











fitnessMasAlto(_,_,_,101).
fitnessMasAlto([P|R],FitnessMayor, PMejor, N):-
    fitness(P,F),
    F > FitnessMayor,
    nl,
    nl,
    write(N),
    write(" caso 1: f > fMayor "),
    nl,
    write("fMayor "),
    write(FitnessMayor),
    nl,
    write("f "),
    write(F),
    nl,
    write("p "),
    write(P),
    nl,
    N1 is N + 1,
    fitnessMasAlto(R, F, P, N1).

fitnessMasAlto([P|R],FitnessMayor, PMejor, N):-
    fitness(P,F),
    not(F > FitnessMayor),
    nl,
    nl,
    write(N),
    write(" caso 2: f < fMayor "),
    nl,
    write("fMayor "),
    write(FitnessMayor),
    nl,
    write("f "),
    write(F),
    nl,
    write("p "),
    write(PMejor),
    N1 is N + 1,
    fitnessMasAlto(R,FitnessMayor,PMejor, N1).