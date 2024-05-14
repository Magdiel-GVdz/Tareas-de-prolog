% Autor: Magdiel GV
% Fecha: 26/10/2022

principal(M,DIAGONAL):- length(M,RENGLONES),
                        writeln(RENGLONES),
                        diagonal(M,1,RENGLONES,[],DIAGONAL).

diagonal(M,I,N,LISTA,LISTA):- I>N.
diagonal(M,I,N,LISTA,DIAGONAL):-  nth1(I,M,RENGLON),
                                  nth1(I,RENGLON,ELEMENTO),
                                  append(LISTA,[ELEMENTO],NUEVA),
                                  X is I + 1,
                                  diagonal(M,X,N,NUEVA,DIAGONAL).