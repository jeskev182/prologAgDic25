Funciones Básicas de Listas en PrologLas listas son una estructura de datos secuencial clave en Prolog, representada internamente como una secuencia de tuplas .(Head, Tail).
El Operador Principal: [Cabeza | Cola]
El operador fundamental para manipular listas es la notación [Head | Tail].
Head (Cabeza): Es el primer elemento de la lista.
Tail (Cola): Es una lista que contiene el resto de los elementos (todo excepto la cabeza).
Ejemplo del Operador:% Unificación simple:
?- [1, 2, 3] = [Cabeza | Cola].
% Resultado:
Cabeza = 1,
Cola = [2, 3].

% Lista vacía:
?- [] = [Cabeza | Cola].
% Resultado:
% false. (La lista vacía no tiene cabeza ni cola)

% Unificación de más elementos:
?- [a, b, c, d] = [H1, H2 | Cola].
% Resultado:
H1 = a,
H2 = b,
Cola = [c, d].



## 1. Cabeza y cola de una lista
Explicación: Este concepto se maneja directamente con la unificación del operador [Cabeza | Cola]. 
No se necesita un predicado específico, sino usar la unificación en las reglas.
Implementación (Implícita en la Unificación):
% Obtener la Cabeza y la Cola de L
obtener_componentes([Cabeza | Cola], Cabeza, Cola).

/*
Ejemplo:
?- obtener_componentes([manzana, pera, uva], C, L).
C = manzana,
L = [pera, uva].
*/



## 2. Verificar si un elemento pertenece a una lista
Explicación: Este predicado, a menudo llamado member/2 en Prolog estándar, utiliza la recursividad para verificar si un elemento es la cabeza de la lista o si pertenece a la cola.
Predicado: pertenece(+Elemento, +Lista)
% Caso base: El elemento pertenece si es la Cabeza de la lista.
pertenece(X, [X | _]).

% Caso recursivo: El elemento pertenece si está en la Cola.
pertenece(X, [_ | Cola]) :-
    pertenece(X, Cola).

/*
Ejemplo:
?- pertenece(gato, [perro, gato, pez]).
true.

?- pertenece(leon, [perro, gato, pez]).
false.
*/



## 3. Calcular la longitud de una lista
Explicación: El predicado calcula la longitud contando 1 por cada elemento y recurriendo a la cola hasta llegar a la lista vacía (que tiene longitud 0).Predicado: longitud(+Lista, -N)
% Caso base: La longitud de una lista vacía es 0.
longitud([], 0).

% Caso recursivo: La longitud de [Cabeza | Cola] es 1 + la longitud de la Cola.
longitud([_ | Cola], N) :-
    longitud(Cola, N1), % Calcula la longitud de la Cola (N1)
    N is N1 + 1.        % N es 1 más N1

/*
Ejemplo:
?- longitud([a, b, c, d], N).
N = 4.

?- longitud([], N).
N = 0.
*/



## 4. Concatenar dos listas
Explicación: Este predicado, llamado append/3 en Prolog estándar, toma los elementos de la primera lista y los agrega al inicio de la segunda lista, formando una tercera lista.
Predicado: concatenar(+Lista1, +Lista2, -Resultado)
% Caso base: Concatenar la lista vacía con L2 da como resultado L2.
concatenar([], L2, L2).

% Caso recursivo:
% Si L1 tiene Cabeza y Cola, la Cabeza va al Resultado,
% y recursivamente concatenamos la Cola con L2 para obtener la Cola_Resultado.
concatenar([Cabeza | Cola], L2, [Cabeza | Cola_Resultado]) :-
    concatenar(Cola, L2, Cola_Resultado).

/*
Ejemplo:
?- concatenar([1, 2], [3, 4, 5], R).
R = [1, 2, 3, 4, 5].
*/



## 5. Invertir una lista
Explicación: La manera más eficiente y idiomática de invertir una lista es usar un acumulador (una tercera variable que almacena el resultado parcial) en una función auxiliar.
Predicado: invertir(+Lista, -Resultado)
% Predicado principal que llama a la función auxiliar con el acumulador inicial [].
invertir(Lista, Invertida) :-
    invertir_acumulador(Lista, [], Invertida).

% Caso base: Cuando la lista de entrada está vacía, el resultado final es el acumulador.
invertir_acumulador([], Acumulador, Acumulador).

% Caso recursivo:
% Mueve la Cabeza de la lista de entrada al frente del Acumulador.
invertir_acumulador([Cabeza | Cola], Acumulador, Invertida) :-
    invertir_acumulador(Cola, [Cabeza | Acumulador], Invertida).

/*
Ejemplo:
?- invertir([a, b, c, d], R).
R = [d, c, b, a].
*/



## 6. Obtener el último elemento
Explicación: Se define un predicado que es verdadero si el elemento es la cabeza de una lista cuya cola es la lista vacía. Si no, se recurre a la cola.
Predicado: ultimo_elemento(+Lista, -Ultimo)
% Caso base: Si la Cola es vacía, la Cabeza es el último elemento.
ultimo_elemento([Ultimo], Ultimo).

% Caso recursivo: Si la lista tiene más de un elemento, ignoramos la Cabeza y
% buscamos el último en la Cola.
ultimo_elemento([_ | Cola], Ultimo) :-
    ultimo_elemento(Cola, Ultimo).

/*
Ejemplo:
?- ultimo_elemento([toro, vaca, cerdo], U).
U = cerdo.
*/
## 7. Sumar los elementos de una lista numérica
Explicación: Suma el valor de la cabeza al resultado de la suma de la cola, utilizando un acumulador (similar a invertir) para optimizar o, más simplemente, recursión directa. Usaremos la recursión directa.
Predicado: suma_elementos(+Lista, -Suma)
% Caso base: La suma de una lista vacía es 0.
suma_elementos([], 0).

% Caso recursivo: Suma es la Cabeza + la suma de la Cola.
suma_elementos([Cabeza | Cola], Suma) :-
    suma_elementos(Cola, Suma_Cola), % Calcula la suma de la Cola
    Suma is Cabeza + Suma_Cola.      % Suma el valor de la Cabeza

/*
Ejemplo:
?- suma_elementos([10, 5, 20], S).
S = 35.
*/
## 8. Eliminar un elemento de una lista
Explicación: Define cómo se forma la lista resultante, que es una copia de la original, pero omitiendo una instancia del elemento a eliminar.
Predicado: eliminar(+Elemento, +Original, -Resultado)
% Caso 1: El elemento a eliminar (E) es la Cabeza. La lista Resultado es simplemente la Cola.
eliminar(E, [E | Cola], Cola).

% Caso 2: El elemento no es la Cabeza. Mantenemos la Cabeza y recurrimos
% en la Cola para ver si el elemento está ahí.
eliminar(E, [Cabeza | Cola], [Cabeza | Cola_Resultado]) :-
    E \= Cabeza, % Asegura que la Cabeza no es el elemento a eliminar
    eliminar(E, Cola, Cola_Resultado).

/*
Ejemplo:
?- eliminar(c, [a, b, c, d, c], R).
% (Nota: Solo elimina la primera ocurrencia por cómo está definida la regla 1)
R = [a, b, d, c].
*/
## 9. Duplicar los elementos de una lista
Explicación: Recorre la lista de entrada y, por cada elemento X, produce una sublista [X, X] en la lista resultante.
Predicado: duplicar(+Original, -Duplicada)
% Caso base: Duplicar una lista vacía resulta en una lista vacía.
duplicar([], []).

% Caso recursivo: Por cada Cabeza, la lista Duplicada tiene [Cabeza, Cabeza]
% seguido de la duplicación de la Cola.
duplicar([Cabeza | Cola], [Cabeza, Cabeza | Cola_Duplicada]) :-
    duplicar(Cola, Cola_Duplicada).

/*
Ejemplo:
?- duplicar([1, 2, 3], R).
R = [1, 1, 2, 2, 3, 3].
*/
## 10. Intercalar dos listas
Explicación: Combina elementos de dos listas alternadamente. La recursión se detiene cuando una de las listas se vacía, y el resto de la lista no vacía se añade al final.
Predicado: intercalar(+L1, +L2, -Resultado)
% Caso base 1: Si L1 está vacía, el Resultado es L2.
intercalar([], L2, L2).

% Caso base 2: Si L2 está vacía, el Resultado es L1.
intercalar(L1, [], L1).

% Caso recursivo: Toma la Cabeza de L1, luego la Cabeza de L2, y recurre con las colas.
intercalar([C1 | R1], [C2 | R2], [C1, C2 | R_intercalado]) :-
    intercalar(R1, R2, R_intercalado).

/*
Ejemplo:
?- intercalar([a, b, c], [1, 2, 3, 4], R).
R = [a, 1, b, 2, c, 3, 4].
*/
