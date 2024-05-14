% Autor:
% Fecha: 23/11/2022

%genetic algorithm robots
%(c) 2020 Ryan Eisele

makegene(Codes,Genes):- findall(G, (between(1,Codes,_),G is random(2^8)), Genes).
makerobot(NumGenes,NumCodes,Gs):- findall(G,(between(1,NumGenes,_),makegene(NumCodes,G)),Gs).
makefirstgeneration(NumRobots,NumGenes,NumCodes,Rs):-findall(R,(between(1,NumRobots,_),makerobot(NumGenes,NumCodes,R)),Rs).
mutate([],[]).
mutate([Ha|Ta],[Hb|Tb]):-
  mutate(Ta,Tb),
  R is random(500),
  once(((R=0,Hb is random(2^8));Ha=Hb)).
mate2([],[],[]).
mate2([M|Ma],[P|Pa],[B|Ba]):-
  R is random(2),
  once(((R=0,B=M);B=P)),
  mate(Ma,Pa,Ba).
mate(Ma,Pa,Ba):-
  mate2(Ma,Pa,Ba2),
  mutate(Ba2,Ba).
makenextgeneration2([],[]).
makenextgeneration2([X],[X]).
makenextgeneration2([A,B|Rs],[A2,B2|Rs2]):-
  mate(A,B,A2),
  mate(B,A,B2),
  makenextgeneration2(Rs,Rs2).
makenextgeneration(Rs,Rs2):-
  length(Rs,Le), NumWinners is floor(Le/2),
  length(Winners,NumWinners),
  append(Winners,_,Rs),
  makenextgeneration2(Winners,Babies),
  append(Winners,Babies,Rs2).
makemaze(0,MW,ML,[width(MW),length(ML)]):-!.
makemaze(N,MW,ML,[M|Mz]):-
  X is random(MW),
  Y is random(ML),
  M = battery((X,Y)),
  N2 is N-1,
  makemaze(N2,MW,ML,Mz).
getcode(Maze,(X,Y),wall):-member(width(MW),Maze),member(length(ML),Maze),(\+between(1,MW,X);\+between(1,ML,Y)),!.
getcode(Mz,(X,Y),battery):-memberchk(battery((X,Y)),Mz), !.
getcode(_,_,nothing).
addpoints((X,Y),(A,B),(X2,Y2)):-X2 is X+A,Y2 is Y+B.
codematches(0,nothing).
codematches(1,wall).
codematches(2,battery).
codematches(3,_).
getview(Mz,(X,Y),Vs):-
  findall(V,(member(A,[(0,-1),(-1,0),(1,0),(0,1)]),addpoints((X,Y),A,A2),
             getcode(Mz,A2,V)),Vs).
activategene([],[]).
activategene([V|View],[G|Gene]):-
  G2 is G mod 4,
  codematches(G2,V),
  activategene(View,Gene).

getmove(Mz,Rb,(X,Y),(X2,Y2)):-
  member([Move|G],Rb),
  getview(Mz,(X,Y),View),
  activategene(View,G),
  Dir is Move mod 5,
  (repeat,Rx is random(2)-1,Ry is random(2)-1,Rx*Ry=:=0,!),
  nth0(Dir,[(0,-1),(-1,0),(1,0),(0,1),(Rx,Ry)],Dif),
  addpoints((X,Y),Dif,(X2,Y2)),!.
getmove(_,[[Move|_]|_],(X,Y),(X2,Y2)):-
  Dir is Move mod 4,
  nth0(Dir,[(0,-1),(-1,0),(1,0),(0,1)],Dif),
  addpoints((X,Y),Dif,(X2,Y2)),!.

runmaze(E,_,Mz,(Xr,Yr),_):-
  best,
  writeln(E),
  member(width(MW),Mz), member(length(ML),Mz),
  (   between(1,ML,Y),nl,
  (between(1,MW,X),once(((Xr=X,Yr=Y,write('*|'));(member(battery((X,Y)),Mz),write('b|');write('_|')))),fail);nl),nl, sleep(0.1), fail.

runmaze(E,_Rb,_Mz,_Coords,0):-  E=<0,  !.
runmaze(E,Rb,Mz,(X,Y),Moves):-
  select(battery((X,Y)),Mz,Mz2),
  getmove(Mz,Rb,(X,Y),(X2,Y2)),
  runmaze(E,Rb,Mz2,(X2,Y2),Moves2),
  Moves is Moves2+1,!.
runmaze(E,Rb,Mz,(X,Y),Moves):-
  getmove(Mz,Rb,(X,Y),(X2,Y2)),
  E2 is E-1,
  runmaze(E2,Rb,Mz,(X2,Y2),Moves2),
  Moves is Moves2+1.
runntrials(0,_E,_Rb,_MW,_ML,_Perc,0):- !.
runntrials(N,E,Rb,MW,ML,Perc,Moves):-
  makemaze(Perc,MW,ML,Mz),
  X is random(MW)+1,
  Y is random(ML)+1,
  runmaze(E,Rb,Mz,(X,Y),M),
  N2 is N-1,
  runntrials(N2,E,Rb,MW,ML,Perc,Moves2),
  Moves is Moves2+M.

rungeneration(Rs,N,E,MW,ML,Perc,Results,Best,Tot):-
  Counter=count(0),
  findall(Moves-Rb, (
    nth1(Z,Rs,Rb), once(((Z=1,assert(best));retractall(best))),
    runntrials(N,E,Rb,MW,ML,Perc,Moves2),
    Moves is -Moves2,
    arg(1,Counter,C), C2 is C+Moves2, nb_setarg(1,Counter,C2)
  ), Results2),
  keysort(Results2,Results3),
  Results3=[Best2-_|_], Best is -Best2,
  findall(Res,member(_-Res,Results3),Results),
  arg(1,Counter,C),
  Tot is C.

evolve:- makefirstgeneration(200,16,5,Rs), retractall(robots(_)), assert(robots(Rs)), writeln(0), evolve2.
evolve2:- Count=counter(0),
          repeat,robots(Rs),rungeneration(Rs,5,5,10,10,40,Results,Best,Tot), Avg is Tot/200, writeln(Best/Avg),
          makenextgeneration(Results,NextGen),
          assert(robots(NextGen)),retract(robots(Rs)),arg(1,Count,N2),N3 is N2+1, nb_setarg(1,Count,N3), writeln('Generation':N3),sleep(1),fail.
