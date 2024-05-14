% Autor: Magdiel Guillermo Valdez
% Fecha: 31/08/2022

%predicados
matrimonio(juan,maria).
matrimonio(mario,martha).
matrimonio(jose,teresa).

hijo(mario,juan).
hijo(jose,juan).

hijo(ricardo,mario)
hijo(roberto,jose)

%clausulas
hermano(PERSONA,HERMANO):- hijo(PERSONA,PADRE), % se encuentra al padre de la persoina
                           hijos(HERMANO,PADRE), % se encuentra a todos los hijos
                           PERSONA =\ HERMANO. % excluyes a la persona entre los hermanos
