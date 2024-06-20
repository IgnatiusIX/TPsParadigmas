%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.
tablero(0, _, []).
tablero(X, Y, [T|Ts]):- X > 0, length(T, Y), X1 is X-1, tablero(X1, Y, Ts).

%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
ocupar(pos(X, Y), Ts) :- nonvar(Ts), iesimo(X, Ts, Fila), iesimo(Y,Fila, ocupada).

ocupar(pos(X, Y), Ts) :- var(Ts), X1 is X+1, Y1 is Y+1, tablero(X1, Y1, Ts), ocupar(pos(X, Y), Ts).

%iesimo(+N, ?L, ?X)
iesimo(0,[T|_], T).
iesimo(N, [_|Ts], Fila) :- N > 0, N1 is N-1, iesimo(N1, Ts, Fila).

%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.
vecino(pos(X,Y),[T|Ts], pos(X1, Y1)) :- member(dir(N, M),[dir(1, 0),dir(-1, 0),dir(0, 1), dir(0,-1)]), 
    X1 is X+N, Y1 is Y+M, length([T | Ts], F), length(T, C), F1 is F-1, C1 is C-1,
    between(0, F1, X1), between(0, C1, Y1).

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
vecinoLibre(P, T, V) :- vecino(P, T, V), noOcupada(V, T).
noOcupada(pos(X,Y), Ts) :- nonvar(Ts), iesimo(X, Ts, Fila), iesimo(Y,Fila, PosBuscada), var(PosBuscada).


%%%%%%%%%%%%%%%%%%%%%%%%
%% Definicion de caminos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% camino(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea una lista
%% [pos(F1,C1), pos(F2,C2),..., pos(Fn,Cn)] que denoten un camino desde Inicio
%% hasta Fin pasando solo por celdas transitables.
%% Además se espera que Camino no contenga ciclos.
%% Notar que la cantidad de caminos es finita y por ende se tiene que poder recorrer
%% todas las alternativas eventualmente.
%% Consejo: Utilizar una lista auxiliar con las posiciones visitadas
 

camino(Inicio, Fin, T, [Inicio | Camino]) :- caminoAux(Inicio, Fin, T, Camino, [Inicio]).

caminoAux(pos(X, Y), pos(X, Y), _, [], _).
caminoAux(Inicio, Fin, T, [Vecino|Camino], Visitados) :- Inicio \= Fin , vecinoLibre(Inicio, T, Vecino), not(member(Vecino, Visitados)),
                    caminoAux(Vecino, Fin, T, Camino, [Vecino|Visitados]).
 
%% 5.1. Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace



%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.
camino2(Inicio, Fin, [T | Ts], Camino) :- length([T | Ts], Fila), length(T, Columna),
    Longmaximo is  Fila * Columna,
    between(0, Longmaximo, Len), camino(Inicio, Fin, [T | Ts], Camino),
    length(Camino, Len).

%% 6.1. Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.


%% Ejercicio 7
%% caminoOptimo(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea un
%% camino óptimo sobre Tablero entre Inicio y Fin. Notar que puede no ser único.
caminoOptimo(Inicio, Fin, T, C) :- camino(Inicio, Fin, T, C), length(C, X), not(otroCamino(Inicio, Fin, T, X)).

otroCamino(Inicio, Fin, T, X) :- camino(Inicio, Fin, T, C1), length(C1, X1), X1 < X.

%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 8
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
caminoDual(Inicio, Fin, T1, T2, C) :- camino(Inicio, Fin, T1, C), camino(Inicio, Fin, T2, C).

%%%%%%%%
%% TESTS
%%%%%%%%

tablero(tests,T) :- tablero(3,3,T), ocupar(pos(0,1),T), ocupar(pos(1,1),T).

cantidadTestsTablero(8). % Actualizar con la cantidad de tests que entreguen
testTablero(0) :- tablero(0,0,[]).
testTablero(1) :- tablero(3,2,[[_,_],[_,_],[_,_]]).
testTablero(2) :- tablero(2,3,[[_,_,_],[_,_,_]]).
testTablero(3) :- not(tablero(2,2,[[_,_,_],[_,_]])).
testTablero(4) :- not(tablero(2,2,[[_,_],[_,_],[_,_]])).
testOcupar(0) :- ocupar(pos(0,0), [[ocupada]]).
testOcupar(1) :- tablero(tests,T), ocupar(pos(0,1), T).
testOcupar(2) :- tablero(tests,T), not(ocupar(pos(0,0), T)).
% Agregar más tests

cantidadTestsVecino(9). % Actualizar con la cantidad de tests que entreguen
testVecino(0) :- tablero(2,2,T), vecino(pos(0,0), T, pos(0,1)), vecino(pos(0,0), T, pos(1,0)).
testVecino(1) :- tablero(3,3,T), vecino(pos(1,1), T, pos(1,0)), vecino(pos(1,1), T, pos(0,1)),
	vecino(pos(1,1), T, pos(2,1)), vecino(pos(1,1), T, pos(1,0)). % es posible irse en todas las direcciones
testVecino(2) :- not(vecino(pos(0,0), [[_]], pos(0,1))), not(vecino(pos(0,0), [[_]], pos(1,0))). % no se va del tablero
testVecino(3) :- not(vecino(pos(0,0), [[_,_],[_,_]], pos(1,1))). % no se mueve en diagonal
testVecino(4) :- not(vecino(pos(1,1), [[_,_],[_,_]], pos(0,0))). % no se mueve en diagonal II
testVecino(5) :- not(vecino(pos(0,0), [[_,_],[_,_]], pos(0,0))). % la posición actual no es vecino
testVecino(6) :- vecino(pos(0,0), [[_,_],[ocupada,_]], pos(1,0)). % las posiciones ocupadas son vecinos
testVecinoLibre(0) :- tablero(2,2,T), vecinoLibre(pos(0,0), T, pos(1,0)), vecinoLibre(pos(0,0), T, pos(0,1)).
testVecinoLibre(1) :- tablero(2,2,T), ocupar(pos(0,1),T), not(vecinoLibre(pos(0,0), T, pos(0,1))). % los vecinos ocupados no son libres
testVecinoLibre(2) :- tablero(2,2,T), ocupar(pos(0,1),T), ocupar(pos(1,0), T), not(vecinoLibre(pos(1,0),T,_)). % los hay vecinos libres
% Agregar más tests

cantidadTestsCamino(1). % Actualizar con la cantidad de tests que entreguen
testCamino(0) :- tablero(2,2,T), camino(pos(0,0),pos(0,1),T,[pos(0,0),pos(0,1)]). % funciona básico
testCamino(1) :- tablero(2,2,T), camino(pos(0,0),pos(0,1),T,[pos(0,0),pos(1,0),pos(1,1),pos(0,1)]). % funciona tomando desvíos
testCamino(2) :- tablero(2,2,T), not(camino(pos(0,0), pos(1,1), T, [pos(0,0), pos(1,1)])). % no considera pasos ilegales
testCamino(3) :- tablero(2,2,T), not(camino(pos(0,0), pos(1,1), T, [pos(0,0), pos(1,0)])). % no considera caminos que no llegan a destino
testCamino(4) :- tablero(2,2,T), not(camino(pos(0,0), pos(0,1), T, [pos(0,0), pos(1,0), pos(1,1), pos(0,1),pos(0,0),pos(0,1)])). % no valen los ciclos
testCamino(5) :- tablero(tests,T), not(camino(pos(0,0), pos(0,1), T, [pos(0,0), pos(0,1)])). % no valen caminos a celdas bloqueadas
testCamino(6) :- tablero(tests,T), not(camino(pos(0,0), pos(0,2), T, [pos(0,0), pos(0,1), pos(0,2)])). % no valen caminos que atraviesen bloqueos
testCamino(7) :- tablero(tests,T), camino(pos(0,0), pos(0,2), T, [pos(0,0), pos(1,0), pos(2,0), pos(2,1), pos(2,2), pos(1,2), pos(0,2)]). % rodea bloqueos
testCamino2(0) :- tablero(2,2,T), camino2(pos(0,0), pos(0,1), T, C), length(C, 2), camino(pos(0,0),pos(0,1),T,C1), length(C1,X), 2 =< X.
testCamino2(1) :- tablero(2,2,T), camino2(pos(0,0), pos(0,1), T, C), length(C,2), camino2(pos(0,0),pos(0,1),T,C1), C1 \= C, length(C1, 4).
testCamino2(2) :- tablero(2,2,T), camino2(pos(0,0), pos(0,1), T, C), camino2(pos(0,0),pos(0,1),T,C1), C1 \= C, not((camino2(pos(0,0),pos(0,1),T,C2),C \= C2, C1 \= C2)).
% Agregar más tests

cantidadTestsCaminoOptimo(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

cantidadTestsCaminoDual(0). % Actualizar con la cantidad de tests que entreguen
% Agregar más tests

%tests(tablero) :- cantidadTestsTablero(M), forall(between(1,M,N), testTablero(N)).
%tests(vecino) :- cantidadTestsVecino(M), forall(between(1,M,N), testVecino(N)).
%tests(camino) :- cantidadTestsCamino(M), forall(between(1,M,N), testCamino(N)).
%tests(caminoOptimo) :- cantidadTestsCaminoOptimo(M), forall(between(1,M,N), testCaminoOptimo(N)).
%tests(caminoDual) :- cantidadTestsCaminoDual(M), forall(between(1,M,N), testCaminoDual(N)).
%
%tests(todos) :-
%  tests(tablero),
%  tests(vecino),
%  tests(camino),
%  tests(caminoOptimo),
%  tests(caminoDual).
%
%tests :- tests(todos).
