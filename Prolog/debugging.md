% --- Análisis de Trazabilidad (Debugging) en Prolog ---

% 1. size/2: Cálculo de la Longitud de una Lista
% ----------------------------------------------------
% Consulta: size([1,2,3,4,5], N).
% Reglas:
%   size([], 0).
%   size([X|Y], N) :- size(Y, N1), N is N1 + 1.

% FASE DE DESCENSO (Llamadas Recursivas):
% El motor desciende en la lista, quitando un elemento en cada paso, hasta llegar a la lista vacía [].
% size([1,2,3,4,5], N) llama a size([2,3,4,5], N1)
% ...
% size([5], N4) llama a size([], N5) -> ACTIVA CASO BASE.

% FASE DE SUBIDA (Cálculo y Retorno):
% El motor regresa de la recursión, sumando 1 en cada paso.
% size([], 0) -> N5 = 0
% N4 es 0 + 1 = 1
% N3 es 1 + 1 = 2
% N2 es 2 + 1 = 3
% N1 es 3 + 1 = 4
% N es 4 + 1 = 5
% Resultado: N = 5.
% Conclusion: La longitud se construye acumulando +1 en la fase de retorno por cada elemento de la lista.

Prolog inicia un proceso recursivo para determinar la longitud de la lista.
Descenso Recursivo (Descomposición)Puesto que la lista inicial no está vacía, Prolog aplica la regla recursiva, llamándose a sí mismo con el resto de la lista (la "cola") en cada paso, ignorando el primer elemento (la "cabeza").
Este proceso de descomposición se repite hasta que la lista se reduce a la lista vacía ([]).
Al alcanzar [], se activa el caso base que establece que el tamaño de una lista vacía es cero (0).2. Ascenso Recursivo (Acumulación)
Una vez que se resuelve el caso base, el programa regresa (asciende) a través de las llamadas recursivas.En cada nivel de retorno, se suma 1 al resultado parcial obtenido del paso anterior.
El proceso comienza con $0$ (de []), y se va incrementando: $0+1=1$, $1+1=2$, $2+1=3$, y así sucesivamente.
Al finalizar el ascenso hasta el nivel de la consulta original, el resultado acumulado es 5.
Prolog unifica este valor con la variable $N$, concluyendo que la lista tiene cinco elementos: $N = 5$.

% ----------------------------------------------------

% 2. rotar/3 (Primer Enfoque: Recursivo por Paso)
% ----------------------------------------------------
% Consulta: rotar([1,2,3,4], L, 2).
% Reglas:
%   rotar(X,X,0).
%   rotar([X|Y], L, N):-N1 is N-1, append(Y,[X],Y1), rotar(Y1, L, N1).

% Proceso de Rotación:
% - Inicial: Lista [1,2,3,4], N=2
% - Paso 1: N1 = 1. Mueve el '1' al final. Nueva Lista: [2, 3, 4, 1]
% - Paso 2: N1 = 0. Mueve el '2' al final. Nueva Lista: [3, 4, 1, 2]
% - Paso 3: N=0. ACTIVA CASO BASE -> L se unifica con [3, 4, 1, 2].
% Resultado: L = [3, 4, 1, 2].
% Conclusion: Simulación paso a paso de la rotación mediante recursión y append/3.

El análisis de la consulta $\text{rotar}([1,2,3,4], L, 2).$ revela cómo el predicado ejecuta la rotación izquierda paso a paso, utilizando la recursión y el predicado append/3.1. Primera Rotación (N=2)
El programa recibe la lista original [1,2,3,4] y la instrucción de realizar dos rotaciones ($N=2$).
Paso 1: Decremento: Prolog reduce el contador de rotaciones: $2 - 1 = 1$.Paso 2: Movimiento: Usa la regla rotar([X|Y], L, N):- ... para tomar la cabeza de la lista ($X=1$) y la coloca al final de la cola ($Y=[2,3,4]$) usando append/3.
Resultado Parcial: La lista se transforma en $\mathbf{[2, 3, 4, 1]}$.
Llamada Siguiente: Llama recursivamente a rotar con la nueva lista $\mathbf{[2, 3, 4, 1]}$ y el contador actualizado a 1.2. Segunda Rotación (N=1)La nueva llamada intenta realizar la rotación restante:Paso 1: Decremento: El contador se reduce nuevamente: $1 - 1 = \mathbf{0}$.
Paso 2: Movimiento: Toma la cabeza actual ($X=2$) y la mueve al final de la cola ($Y=[3,4,1]$) con append/3.
Resultado Final: La lista se transforma en $\mathbf{[3, 4, 1, 2]}$.3. Caso Base (N=0)Dado que el contador de rotaciones ha llegado a cero (0), se cumple el caso base ($\text{rotar}(X, X, 0)$).
El proceso recursivo se detiene, y la lista final $\mathbf{[3, 4, 1, 2]}$ se unifica con la variable resultado $L$.

% ----------------------------------------------------

% 3. rotar/3 (Segundo Enfoque: Declarativo por División)
% ----------------------------------------------------
% Consulta: rotar([1,2,3,4], R, 2).
% Reglas:
%   rotar(L,R, N):-append(X, Y, L), size(X, N), append(Y, X, R).

% BÚSQUEDA Y FILTRADO (Backtracking):
% append(X, Y, [1,2,3,4]) genera todas las particiones. size(X, 2) las filtra:
% - Intento X=[], Y=[1,2,3,4] -> size(X, 2) falla (0 != 2).
% - Intento X=[1], Y=[2,3,4] -> size(X, 2) falla (1 != 2).
% - Intento X=[1,2], Y=[3,4] -> size(X, 2) tiene éxito (2 = 2). -> SELECCIONADO.

% CONSTRUCCIÓN DEL RESULTADO:
% Con X=[1,2] y Y=[3,4], se ejecuta:
% append(Y, X, R) -> append([3, 4], [1, 2], R)
% Resultado: R = [3, 4, 1, 2].
% Conclusion: Se divide la lista en el prefijo (X) de tamaño N=2 y el resto (Y). Luego se reordenan: Y seguido de X.

El análisis de la consulta $\text{rotar}([1,2,3,4], R, 2).$ con el enfoque declarativo (que utiliza la división de la lista) se centra en la búsqueda y filtrado de las sublistas correctas.
1. Búsqueda de la Partición CorrectaEl objetivo del programa es dividir la lista [1,2,3,4] en dos sublistas, $X$ y $Y$, donde la longitud de $X$ coincida exactamente con el número de rotaciones deseado, que es 
2.El predicado $\text{append}(X, Y, [1,2,3,4])$ genera posibles divisiones, y $\text{size}(X, 2)$ las valida:
Intento 1: Si $X=[]$ (Tamaño 0), la condición de $\text{size}([], 2)$ falla (0 $\ne$ 2).
Intento 2: Si $X=[1]$ (Tamaño 1), la condición de $\text{size}([1], 2)$ falla (1 $\ne$ 2).
Intento 3: Finalmente, Prolog encuentra la división donde $\mathbf{X=[1,2]}$ y $\mathbf{Y=[3,4]}$. Aquí, $\text{size}([1,2], 2)$ es exitosa (2 = 2).
El motor de Prolog realiza un backtracking (prueba y error) hasta que la división de la lista cumple con el requisito de longitud.
2. Construcción del Resultado
Una vez que las sublistas $\mathbf{X}$ (el prefijo a rotar) y $\mathbf{Y}$ (el resto de la lista) han sido identificadas correctamente, se realiza la rotación:El predicado $\text{append}(Y, X, R)$ se encarga de reordenar las partes.
Esto une la segunda parte ($Y=[3,4]$) con la primera parte ($X=[1,2]$), formando $\mathbf{R=[3, 4, 1, 2]}$.