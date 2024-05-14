% Autor: Magdiel Guillermo Valdez
% Fecha: 31/08/2022

%predicados
matrimonio(juan,maria).
matrimonio(mario,martha).
matrimonio(jose,teresa).
matrimonio(valentin,josefa).
matrimonio(andres,dolores).

hijo(juan,andres).
hijo(rogelio,andres).
hijo(noe,andres).

hijo(luis,valentin).
hijo(ismael,valentin).
hijo(maria,valentin).

hijo(mario,juan).
hijo(jose,juan).

hijo(ricardo,mario).

hijo(roberto,jose).

%clausulas
hermano(PERSONA,HERMANO):- hijo(PERSONA,PADRE), % se encuentra al padre de la persoina
                           hijo(HERMANO,PADRE), % se encuentra a todos los hijos
                           PERSONA \= HERMANO. % excluyes a la persona entre los hermanos
%madre
madre(PERSONA,MADRE):- hijo(PERSONA,PADRE),
                       matrimonio(PADRE,MADRE).

%tios maternos
tio(PERSONA,TIO):- madre(PERSONA,MADRE),
                   hermano(MADRE,TIO).
tio(PERSONA,TIO):- madre(PERSONA,MADRE),
                   hermano(MADRE,HERMANO),
                   matrimonio(HERMANO,TIO).
                   
%tio paterno
tio(PERSONA,TIO):- hijo(PERSONA,PADRE),
                   hermano(PADRE,TIO).
tio(PERSONA,TIO):- hijo(PERSONA,PADRE),
                   hermano(PADRE,HERMANO),
                   matrimonio(HERMANO,TIO).

%abuelos
%paternos
abuelo(PERSONA,ABUELO):- hijo(PERSONA,PADRE),  % se encuentra al padre de la persona
                         hijo(PADRE,ABUELO).   % se encuentra al padre del padre
abuelo(PERSONA,ABUELA):- hijo(PERSONA,PADRE),  % se encuentra al padre de la persona
                         madre(PADRE,ABUELA).  % se encuentra a la madre del padre
%maternos
abuelo(PERSONA,ABUELO):- madre(PERSONA,MADRE), % se encuentra a la madre de la persona
                         hijo(MADRE,ABUELO).   % se encuentra al padre de la madre
abuelo(PERSONA,ABUELA):- madre(PERSONA,MADRE), % se encuentra a la madre de la persoa
                         madre(MADRE,ABUELA).  % se encuentra a la madre de la madre

%nieto
nieto(PERSONA,NIETO):- hijo(HIJO,PERSONA),
                       hijo(NIETO,HIJO).
nieto(PERSONA,NIETO):- madre(HIJO,PERSONA),
                       hijo(NIETO,HIJO).

%primo
primo(PERSONA,PRIMO):- tio(PERSONA,TIO),
                       hijo(PRIMO,TIO).

%sobrino
sobrino(PERSONA,SOBRINO):- tio(SOBRINO,PERSONA).

%cuñado
cuñado(PERSONA,CUÑADO):- matrimonio(PERSONA,ESPOSA),
                         hermano(ESPOSA,CUÑADO).
cuñado(PERSONA,CUÑADO):- hermano(PERSONA,HERMANO),
                         matrimonio(HERMANO,CUÑADO).
cuñado(PERSONA,CUÑADO):- matrimonio(ESPOSO,PERSONA),
                         hermano(ESPOSO,CUÑADO).
cuñado(PERSONA,CUÑADO):- hermano(PERSONA,HERMANA),
                         matrimonio(CUÑADO,HERMANA).

%concuño
concuño(PERSONA,CONCUÑO):- cuñado(PERSONA,CUÑADO),
                           matrimonio(CUÑADO,CONCUÑO).
concuño(PERSONA,CONCUÑO):- cuñado(PERSONA,CUÑADA),
                           matrimonio(CONCUÑO,CUÑADA).

%suegra
suegra(PERSONA,SUEGRA):- matrimonio(PERSONA,ESPOSA),
                         madre(ESPOSA,SUEGRA).
suegra(PERSONA,SUEGRA):- matrimonio(ESPOSO,PERSONA),
                         madre(ESPOSO,SUEGRA).

%suegro
suegro(PERSONA,SUEGRO):- matrimonio(PERSONA,ESPOSA),
                         hijo(ESPOSA,SUEGRO).
suegro(PERSONA,SUEGRO):- matrimonio(ESPOSO,PERSONA),
                         hijo(ESPOSO,SUEGRO).

%bisabuelo
bisabuelo(PERSONA,BISABUELO):- abuelo(PERSONA,ABUELO),
                               hijo(ABUELO,BISABUELO).
bisabuelo(PERSONA,BISABUELO):- abuelo(PERSONA,ABUELO),
                               madre(ABUELO,BISABUELO).

%bisnieto
bisnieto(PERSONA,BISNIETO):- bisabuelo(BISNIETO,PERSONA).