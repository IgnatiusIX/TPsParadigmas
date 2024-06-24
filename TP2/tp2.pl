%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%
%% Consideraciones
%%%%%%%%%%%%%%%%%%%%%%%%
%
% - Una celda no está ocupada si es de la forma _ (es decir, es una variable libre).
%

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.

tablero(0, _, []).
tablero(X, Y, [T | Ts]):- X > 0, length(T, Y), X1 is X-1, tablero(X1, Y, Ts).

%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.

ocupar(pos(X, Y), Ts) :- nonvar(Ts), iesimo(X, Ts, Fila), iesimo(Y, Fila, ocupada).
ocupar(pos(X, Y), Ts) :- var(Ts), X1 is X + 1, Y1 is Y + 1, tablero(X1, Y1, Ts), ocupar(pos(X, Y), Ts).

%% iesimo(+N, ?L, ?X) vale cuando X está en la posición 'I' de la lista L.

iesimo(0, [T | _], T).
iesimo(I, [_ | Ts], Fila) :- I > 0, I1 is I - 1, iesimo(I1, Ts, Fila).

%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.

vecino(pos(X,Y),[T | Ts], pos(X1, Y1)) :- member(dir(N, M),[dir(1, 0), dir(-1, 0), dir(0, 1), dir(0,-1)]), 
    X1 is X + N, Y1 is Y + M, length([T | Ts], F), length(T, C), F1 is F - 1, C1 is C - 1,
    between(0, F1, X1), between(0, C1, Y1).

% ! está bien esto?
%% Acá se utiliza la técnica generate and test, donde se generan todos los posibles vecinos (norte, sur, este
%% u oeste) y se verifica que estén dentro del tablero.

%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero.

vecinoLibre(P, T, V) :- vecino(P, T, V), noOcupada(V, T).

%% noOcupada(+Pos, +Tablero) es verdad si y solo sí Pos es una posición libre en Tablero.

noOcupada(pos(X, Y), Ts) :- nonvar(Ts), iesimo(X, Ts, Fila), iesimo(Y, Fila, PosBuscada), var(PosBuscada).



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

%% caminoAux(+Inicio, +Fin, +Tablero, -Camino, +Visitados) es verdadero siempre que Camino sea una lista
%% de posiciones desde algún vecino libre de Inicio hasta Fin. Toda posición en Camino debe estar libre,
%% ser vecina de la posición anterior (si es que existe), y no aparecen más de una única vez en Camino.
%% A su vez, una posición está en Camino si y solo si está en Visitados.

caminoAux(pos(X, Y), pos(X, Y), _, [], _).
caminoAux(Inicio, Fin, T, [Vecino | Camino], Visitados) :- Inicio \= Fin, vecinoLibre(Inicio, T, Vecino), 
	not(member(Vecino, Visitados)), caminoAux(Vecino, Fin, T, Camino, [Vecino | Visitados]).
 
%% 5.1. Analizar la reversibilidad de los parámetros Fin y Camino justificando adecuadamente en cada
%% caso por qué el predicado se comporta como lo hace

% La reversibilidad sobre camino es posible sin ningún problema, ya que si no se instancia buscara un camino que logre cumplir
% con lo pedido, ahora si se instancia Camino verificara entre todas sus posibles caminos si alguno unifica con el que le pasamos
% como dato de entrada.

% En cambio la reversibilidad sobre fin no esta garantizada ya que cuando tomamos el predicado caminoAux 
% estamos comparando el valor Inicio que si esta instanciado con Fin que no esta instanciado y eso generaria inconvenientes.

%% Ejercicio 6
%% camino2(+Inicio, +Fin, +Tablero, -Camino) idem camino/4 pero que las soluciones
%% se instancien en orden creciente de longitud.

camino2(Inicio, Fin, [T | Ts], Camino) :- length([T | Ts], Fila), length(T, Columna),
	Longmaximo is  Fila * Columna, between(0, Longmaximo, Len),
	camino(Inicio, Fin, [T | Ts], Camino), length(Camino, Len).

%% 6.1. Analizar la reversibilidad de los parámetros Inicio y Camino justificando adecuadamente en
%% cada caso por qué el predicado se comporta como lo hace.



%% Ejercicio 7
%% caminoOptimo(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea un
%% camino óptimo sobre Tablero entre Inicio y Fin. Notar que puede no ser único.

caminoOptimo(Inicio, Fin, T, C) :- camino(Inicio, Fin, T, C), length(C, X),
	not(otroCamino(Inicio, Fin, T, X)).

%% otroCamino(+Inicio, +Fin, +Tablero, -LongitudÓptima) sólo es verdad cuando existe algún camino
%% cuya longitud sea menor  que la del camino óptimo.

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

cantidadTestsTablero(8).
testTablero(1) :- tablero(0, 0, []).
testTablero(2) :- tablero(3, 2, [[_, _], [_, _], [_, _]]).
testTablero(3) :- tablero(2, 3, [[_, _, _], [_, _, _]]).
testTablero(4) :- not(tablero(2, 2, [[_, _, _],[_, _]])).
testTablero(5) :- not(tablero(2, 2, [[_, _], [_, _],[_, _]])).
testTablero(6) :- ocupar(pos(0, 0), [[ocupada]]).
testTablero(7) :- tablero(tests,T), not(noOcupada(pos(0,1), T)). % vale checkear si está ocupado un casillero.
testTablero(8) :- tablero(tests,T), noOcupada(pos(0,0), T). % vale checkear si no está ocupado.

cantidadTestsVecino(10).
testVecino(1) :- tablero(2,2,T), vecino(pos(0,0), T, pos(0,1)), vecino(pos(0,0), T, pos(1,0)).
testVecino(2) :- tablero(3,3,T), vecino(pos(1,1), T, pos(1,0)), vecino(pos(1,1), T, pos(0,1)),
	vecino(pos(1,1), T, pos(2,1)), vecino(pos(1,1), T, pos(1,0)). % es posible irse en todas las direcciones
testVecino(3) :- not(vecino(pos(0,0), [[_]], _)). % no se va del tablero
testVecino(4) :- not(vecino(pos(0,0), [[_, _],[_, _]], pos(1,1))). % no se mueve en diagonal
testVecino(5) :- not(vecino(pos(1,1), [[_, _],[_, _]], pos(0,0))). % no se mueve en diagonal II
testVecino(6) :- not(vecino(pos(0,0), [[_, _],[_, _]], pos(0,0))). % la posición actual no es vecino
testVecino(7) :- vecino(pos(0,0), [[_, _],[ocupada, _]], pos(1,0)). % las posiciones ocupadas son vecinos
testVecino(8) :- tablero(2,2,T), vecinoLibre(pos(0,0), T, pos(1,0)), vecinoLibre(pos(0,0), T, pos(0,1)).
testVecino(9) :- tablero(2,2,T), ocupar(pos(0,1),T), not(vecinoLibre(pos(0,0), T, pos(0,1))). % los vecinos ocupados no son libres
testVecino(10) :- tablero(2,2,T), ocupar(pos(0,1),T), ocupar(pos(1,0), T), not(vecinoLibre(pos(0,0),T, _)). % no hay vecinos libres si las celdas contiguas están ocupadas

cantidadTestsCamino(13).
testCamino(1) :- tablero(2,2,T), camino(pos(0,0),pos(0,1),T,[pos(0,0),pos(0,1)]). % funciona básico
testCamino(2) :- tablero(1,1,T), camino(pos(0,0),pos(0,0),T,[pos(0,0)]). % funciona para caminos de un solo vértice.
testCamino(3) :- tablero(3,3,T), camino(pos(2,2),pos(2,2),T,[pos(2,2)]). % idem 2
testCamino(4) :- tablero(2,2,T), camino(pos(0,0),pos(0,1),T,[pos(0,0),pos(1,0),pos(1,1),pos(0,1)]). % funciona tomando desvíos
testCamino(5) :- tablero(2,2,T), not(camino(pos(0,0), pos(1,1), T, [pos(0,0), pos(1,1)])). % no considera pasos ilegales
testCamino(6) :- tablero(2,2,T), not(camino(pos(0,0), pos(1,1), T, [pos(0,0), pos(1,0)])). % no considera caminos que no llegan a destino
testCamino(7) :- tablero(2,2,T), not(camino(pos(0,0), pos(0,1), T, [pos(0,0), pos(1,0), pos(1,1), pos(0,1),pos(0,0),pos(0,1)])). % no valen los ciclos
testCamino(8) :- tablero(tests,T), not(camino(pos(0,0), pos(0,1), T, [pos(0,0), pos(0,1)])). % no valen caminos a celdas bloqueadas
testCamino(9) :- tablero(tests,T), not(camino(pos(0,0), pos(0,2), T, [pos(0,0), pos(0,1), pos(0,2)])). % no valen caminos que atraviesen bloqueos
testCamino(10) :- tablero(tests,T), camino(pos(0,0), pos(0,2), T, [pos(0,0), pos(1,0), pos(2,0), pos(2,1), pos(2,2), pos(1,2), pos(0,2)]). % rodea bloqueos
testCamino(11) :- tablero(2,2,T), camino2(pos(0,0), pos(0,1), T, C), length(C, X1), 
	not((camino(pos(0,0),pos(0,1),T,C1), length(C1, X2), X1 > X2)). % todos los caminos encontrados tienen longitud mayor o igual al camino mínimo de camino2
testCamino(12) :- tablero(2,2,T), camino2(pos(0,0), pos(0,1), T, C),
	length(C,2), camino2(pos(0,0),pos(0,1),T,C1), C1 \= C, length(C1, 4).
testCamino(13) :- tablero(2,2,T), camino2(pos(0,0), pos(0,1), T, C), camino2(pos(0,0),pos(0,1),T,C1), C1 \= C,
	not((camino2(pos(0,0),pos(0,1),T,C2), C \= C2, C1 \= C2)). % para un tablero de 2 x 2, camino2 encuentra sólo 2 caminos válidos y ninguno más.

cantidadTestsCaminoOptimo(4).
testCaminoOptimo(1) :- tablero(2,2,T), caminoOptimo(pos(0,0),pos(0,1),T,[pos(0,0),pos(0,1)]),
	not(caminoOptimo(pos(0,0),pos(0,1),T,[pos(0,0),pos(1,0),pos(1,1),pos(0,1)])). % encuentra el camino óptimo y sólo el óptimo.
testCaminoOptimo(2) :- tablero(2,2,T), caminoOptimo(pos(0,0), pos(1,1), T, [pos(0,0), pos(1,0), pos(1,1)]),
	caminoOptimo(pos(0,0), pos(1,1), T, [pos(0,0), pos(0,1), pos(1,1)]). % encuentra todos los caminos óptimos.
testCaminoOptimo(3).
testCaminoOptimo(4).

cantidadTestsCaminoDual(5). % Actualizar con la cantidad de tests que entreguen
testCaminoDual(1) :- tablero(tests,T1), tablero(3,3,T2), caminoDual(pos(0,0),pos(0,2), T1, T2, [pos(0,0), pos(1,0), pos(2,0), pos(2,1), pos(2,2), pos(1,2), pos(0,2)]),
	not(caminoDual(pos(0,0),pos(0,2), T1, T2, [pos(0,0), pos(0,1), pos(0,2)])). % encuentra el camino válido y no considera caminos que valen para uno de los tableros.
testCaminoDual(2) :- tablero(tests,T1), ocupar(pos(2,1),T1), tablero(3,3,T2), not(caminoDual(pos(0,0),pos(0,2),T1,T2,_)). % no devuelve algún camino si no existe caminos para alguno de los tableros.
testCaminoDual(3) :- tablero(tests,T1), tablero(3,3,T2), ocupar(pos(2,1),T2), not(caminoDual(pos(0,0),pos(0,2),T1,T2,_)). % no devuelve algún camino si no hay camino común entre ambos tableros.
testCaminoDual(4). :- tablero(3,3,T1), ocupar(pos(1,1),T1), tablero(3,3,T2), caminoDual(pos(0,0),pos(0,2),T1,T2,[pos(0,0),pos(0,1),pos(0,2)]),
	caminoDual(pos(0,0),pos(0,2),T1,T2,[pos(0,0),pos(1,0),pos(2,0),pos(2,1),pos(2,2),pos(1,2),pos(0,2)]),
	not(caminoDual(pos(0,0),pos(0,2),T1,T2,[pos(0,0),pos(1,0),pos(1,1),pos(1,2),pos(0,2)])).
testCaminoDual(5).
% Agregar más tests

tests(tablero) :- cantidadTestsTablero(M), forall(between(1,M,N), testTablero(N)).
tests(vecino) :- cantidadTestsVecino(M), forall(between(1,M,N), testVecino(N)).
tests(camino) :- cantidadTestsCamino(M), forall(between(1,M,N), testCamino(N)).
tests(caminoOptimo) :- cantidadTestsCaminoOptimo(M), forall(between(1,M,N), testCaminoOptimo(N)).
tests(caminoDual) :- cantidadTestsCaminoDual(M), forall(between(1,M,N), testCaminoDual(N)).

tests(todos) :-
  tests(tablero),
  tests(vecino),
  tests(camino),
  tests(caminoOptimo),
  tests(caminoDual).

tests :- tests(todos).
