## 1. Secuencia fibonnacci
% Caso base 1: El número de Fibonacci en la posición 0 es 0.
fibonacci(0, 0).

% Caso base 2: El número de Fibonacci en la posición 1 es 1.
fibonacci(1, 1).

% Caso recursivo: F(N) = F(N-1) + F(N-2).
% Asume que N es mayor que 1.
fibonacci(N, F) :-
    N > 1,
    N1 is N - 1,   % Calcula N-1
    N2 is N - 2,   % Calcula N-2
    fibonacci(N1, F1), % Llama recursivamente para F(N-1)
    fibonacci(N2, F2), % Llama recursivamente para F(N-2)
    F is F1 + F2.  % F es la suma de los dos anteriores

/*
Consultas de Ejemplo:
?- fibonacci(7, F).
F = 13.

?- fibonacci(0, F).
F = 0.
*/


## 2. División de Enteros con Restas Sucesivas
Este predicado calcula el cociente y el resto de una división utilizando únicamente la resta.
Predicado: division_restas(+Dividendo, +Divisor, -Cociente, -Resto)
Calcula el Cociente y el Resto de Dividendo / Divisor.% Caso base: Si el Dividendo es menor que el Divisor, el Cociente es 0 y el Resto es el Dividendo.
division_restas(Dividendo, Divisor, 0, Dividendo) :-
    Dividendo < Divisor,
    !. % El corte (!) es para asegurar que solo se evalúe este caso base cuando Dividendo < Divisor.

% Caso recursivo: Restamos el Divisor al Dividendo y aumentamos el Cociente en 1.
division_restas(Dividendo, Divisor, Cociente, Resto) :-
    NuevoDividendo is Dividendo - Divisor, % Realiza la resta
    division_restas(NuevoDividendo, Divisor, NuevoCociente, Resto), % Llamada recursiva
    Cociente is NuevoCociente + 1. % Incrementa el cociente

/*
Consultas de Ejemplo:
?- division_restas(17, 4, C, R).
C = 4,
R = 1.

?- division_restas(10, 3, C, R).
C = 3,
R = 1.

?- division_restas(5, 10, C, R).
C = 0,
R = 5.
*/
## 3. Potencia con Sumas (Multiplicación con Sumas, luego Potencia)
Para implementar la potencia (A^B) usando solo sumas, primero necesitamos un predicado de multiplicación que use solo sumas, y luego un predicado de potencia que use esa multiplicación.
### 3a. Multiplicación con Sumas Sucesivas
Predicado: multiplicacion_sumas(+A, +B, -P)
Calcula el producto P de A * B usando solo sumas.% 
Caso base: Multiplicar por 0 da 0.
multiplicacion_sumas(_, 0, 0).

% Caso recursivo: A * B = A + (A * (B-1)).
multiplicacion_sumas(A, B, P) :-
    B > 0,
    B1 is B - 1, % Decrementa el multiplicador
    multiplicacion_sumas(A, B1, P1), % Llamada recursiva para A * (B-1)
    P is A + P1. % Suma A al resultado anterior

/*
Consultas de Ejemplo:
?- multiplicacion_sumas(5, 3, P).
P = 15.
*/
### 3b. Potencia con Multiplicación (que usa Sumas)Predicado: potencia_sumas(+Base, +Exponente, -Resultado)Calcula la Resultado de Base ^ Exponente usando la multiplicación definida anteriormente.% Caso base: Cualquier número elevado a la 0 es 1.
potencia_sumas(_, 0, 1).

% Caso recursivo: Base^Exponente = Base * (Base^(Exponente-1)).
potencia_sumas(Base, Exponente, Resultado) :-
    Exponente > 0,
    E1 is Exponente - 1, % Decrementa el exponente
    potencia_sumas(Base, E1, R1), % Llama recursivamente para Base^(Exponente-1)
    multiplicacion_sumas(Base, R1, Resultado). % Multiplica la base por el resultado anterior

/*
Consultas de Ejemplo:
?- potencia_sumas(2, 4, R). % 2 * 2 * 2 * 2 = 16
R = 16.

?- potencia_sumas(5, 3, R). % 5 * 5 * 5 = 125
R = 125.
*/
