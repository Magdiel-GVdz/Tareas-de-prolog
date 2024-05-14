% Autor: Magdiel GV
% Fecha: 28/09/2022

:- dynamic sobre/2.

sobre(a,piso).
sobre(b,a).
sobre(c,b).
sobre(nada,c).
sobre(e,piso).
sobre(nada,e).
sobre(f,piso).
sobre(nada,f).
sobre(g,piso).
sobre(nada,g).

objeto(a,cubo).
objeto(b,cubo).
objeto(c,cubo).
objeto(e,piramide).
objeto(f,cilindro).
objeto(g,esfera).

% predicados
quitar(Y,X):- % precondiciones
              sobre(Y,X),
              sobre(nada,Y),
              % acciones
              retract(sobre(Y,X)),
              assert(sobre(Y,piso)),
              assert(sobre(nada,X)),
              % mensajes
              write('Se quito el bloque '),
              write(Y),
              write(' del bloque '),
              writeln(X).

quitar(Y,X):- % precondiciones
              sobre(Y,X),
              sobre(Z,Y),
              % acciones
              quitar(Z,Y),
              quitar(Y,X).
              
% escenario 1
colocar(Y,X):- % precondiciones
               sobre(nada,X),
               sobre(nada,Y),
               objeto(X,TIPO),
               not(member(TIPO,[esfera,piramide])),
               % acciones
               assert(sobre(Y,X)),
               retract(sobre(nada,X)),
               retract(sobre(Y,piso)),
               % mensajes
               write('Se coloco el bloque '),
               write(Y),
               write(' sobre el bloque '),
               writeln(X).
               
% escenario 2
colocar(Y,X):- % precondiciones
               sobre(nada,X),
               sobre(nada,Y),
               sobre(Y,Z),
               objeto(X,TIPO),
               not(member(TIPO,[esfera,piramide])),
               % acciones
               assert(sobre(Y,X)),
               retract(sobre(nada,X)),
               retract(sobre(Y,Z)),
               assert(sobre(nada,Z)),
               % mensajes
               write('Se coloco el bloque '),
               write(Y),
               write(' sobre el bloque '),
               writeln(X).
               
% escenario 3
colocar(Y,X):- % precondiciones
               sobre(Z,Y),
               sobre(Y,piso),
               sobre(nada,X),
               objeto(X,TIPO),
               not(member(TIPO,[esfera,piramide])),
               % acciones
               quitar(Z,Y),
               colocar(Y,X).

% escenario 4
colocar(Y,X):- % precondiciones
               sobre(nada,X),
               sobre(Z,Y),
               sobre(Y,W),
               objeto(X,TIPO),
               not(member(TIPO,[esfera,piramide])),
               % acciones
               quitar(Z,Y),
               colocar(Y,X).

% escenario 5
colocar(Y,X):- % precondiciones
               sobre(Y,piso),
               sobre(Z,X),
               objeto(X,TIPO),
               not(member(TIPO,[esfera,piramide])),
               % acciones
               quitar(Z,X),
               colocar(Y,X).

% escenario 6
colocar(Y,X):- % precondiciones
                sobre(Y,W),
                sobre(Z,X),
                objeto(X,TIPO),
                not(member(TIPO,[esfera,piramide])),
                % acciones
                quitar(Y,W),
                quitar(Z,X),
                colocar(Y,X).

% escenario 7
colocar(Y,X):- % precondiciones
                sobre(Y,piso),
                sobre(Z,Y),
                sobre(W,X),
                objeto(X,TIPO),
                not(member(TIPO,[esfera,piramide])),
                % acciones
                quitar(W,X),
                quitar(Z,Y),
                colocar(Y,X).

% escenario 8
colocar(Y,X):- % precondiciones
                sobre(Y,V),
                sobre(Z,Y),
                sobre(W,X),
                objeto(X,TIPO),
                not(member(TIPO,[esfera,piramide])),
                % acciones
                quitar(W,X),
                quitar(Z,Y),
                colocar(Y,X).