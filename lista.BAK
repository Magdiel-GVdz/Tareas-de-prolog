% Autor: Magdiel GV
% Fecha: 06/10/2022

%manejo de listas

%[a,b,c,1,2,]. incluir lista en el comando

%comando: primero([a,b,c.d],X).




longitud_de_la_lista([], 0).
longitud_de_la_lista([_|R], L):- longitud_de_la_lista_length(R, L2),
                                  L is L2 + 1.
  
primero([P|R],P).

segundo([P|[S|R]],S).

ultimo(L,U):- reverse(L,[U|R]).

recorrer([]).
recorrer([P|R]):-writeln(P),
                 recorrer(R).
                 
suma([],S,S).
suma([P|R],S,RESULTADO):- X is S + P,
                          suma(R,X,RESULTADO).
                          
/*
  3  5  4
  0  1  5
  6  4  3
*/

%diagonal([[3,5,4],[0,1,5],[6,4,3]],S).

diagonales([A,B,C],RESULTADO):- primero(A,P),
                              segundo(B,S),
                              ultimo(C,U),
                              RESULTADO is P+S+U.
                              
promedio(LISTA,PROMEDIO):- suma(LISTA, 0, TOTAL),
                           length(LISTA,N),
                           PROMEDIO is TOTAL/N.
                           
mayor(L,MENOR,MAYOR):- sort(L, LORDENADA),
                       primero(LORDENADA, MENOR),
                       ultimo(LORDENADA, MAYOR).

multiplicar([],S,S).
multiplicar([P|R],S,RESULTADO):- X is S * P,
                                 multiplicar(R,X,RESULTADO).
                                 
%[[6,3,2],[4,3,2],[1,4,5]] + [[4,3,0],[1,4,5],[6,0,2]]


suma_renglon([],[],R,R).
suma_renglon([P1|R1],[P2|R2],L,R):- X is P1 + P2,
                                    append(L,[X],NEW),
                                    suma_renglon(R1,R2,NEW,R).

sumar_matrices([],[],R,R).
sumar_matrices([P1|R1],[P2|R2],L,RESULTADO):- suma_renglon(P1,P2,[],R),
                                              append(L,[R],NEW),
                                              sumar_matrices(R1,R2,NEW,RESULTADO).
                                            

sumar([],[],R).
sumar([P1|R1],[P2|R2],R):- R is X + (P1*P2),
                           sumar(R1,R2,R).

multiplicar_matrices([],[],R,R).
multiplicar_matrices([P1|R1],[P2|R2],L,RESULTADO):- sumar(P1,P2,R),
                                                    append(L,[R],NEW),
                                                    multiplicar_matrices(R1,M3,NEW,RESULTADO).
                                                    
                                                    
                                                    
% clausula llenar la matriz nxn aleatoriamente
% obener la suma de cada fila y columna
% obtener dos listas con la diagonal

crear_lista_vacia(L,0,L).
crear_lista_vacia(L,N,LISTA):- X is random(50),
                               append(L,[X],NUEVA),
                               Y is N-1,
                               crear_lista_vacia(NUEVA,Y,LISTA).
                               

%crear_matriz_vacia(M,0,M).
%crear_matriz_vacia(M,N,MATRIZ):- crear_lista_vacia(NUEVA,Y,LISTA)
%                                 Y is N-1,
%                                 crear_matriz_vacia().

rellenar_renglon([],R,R).
rellenar_renglon([P|R],L,RESULTADO):- X is random(50),
                                      append(L,[X],NEW),
                                      rellenar_renglon(R,NEW,RESULTADO).

                               

%(X,X) (N-X+1)

%diagonal()
diagonal1(MATRIZ,DIAGONAL):- length(M,RENGLONES),
                            nth1(N, MATRIZ, X),
                            nth1(N, X, Y),
                            N + 1,
                            append(DIAGONAL,[Y],NEW),
                            diagonall(MATRIZ,NEW,X,N).
                            
principal(M,DIAGONAL):- length(M,RENGLONES),
                        writeln(RENGLONES),
                        diagonal(M,1,RENGLONES,[],DIAGONAL).
                        
diagonal(M,I,N,LITA,LISTA):- I>N.
diagonal(M,I,N,LISTA,DIAGONAL):- nth1(I,M,RENGLON),
                                  nth1(I,RENGLON,ELEMENTO),
                                  append(LISTA,[ELEMENTO],NUEVA),
                                  X is I + 1,
                                  diagonal(M,X,N,NUEVA,DIAGONAL).

                                    






















