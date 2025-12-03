## Tema: Biblioteca y Libros

- **Actividad:** Formalizar los siguientes enunciados y generar los posibles hechos y reglas

1. La **Biblioteca Nacional** tiene el libro **"Don Quijote de la Mancha"**.
2. Todos los libros en la **Biblioteca Nacional** están catalogados.
3. Existen libros que están en más de una biblioteca.
4. Si un libro es **raro**, entonces no se puede prestar.
5. La **Biblioteca Central** tiene más de **10,000** libros.
6. Todos los autores tienen al menos un libro en una biblioteca.
7. Existe un autor que tiene más de **5** libros publicados.
8. No todos los libros de la biblioteca están en buen estado.
9. Si un libro está en buen estado, puede ser prestado.
10. Todos los usuarios registrados pueden tomar prestado un libro.
11. Existen libros que solo se pueden consultar en la biblioteca.
12. Todo libro prestado debe ser devuelto en 15 días.
13. Hay un libro que nadie ha pedido en préstamo.
14. Si un usuario tiene una multa, no puede pedir un libro prestado.
15. Todos los libros escritos por un mismo autor están en la misma
    sección.
16. Existe un libro que tiene más de un ejemplar en la biblioteca.
17. Todo usuario con más de tres préstamos debe devolver uno para pedir
    otro.
18. Hay una sección de la biblioteca donde todos los libros son de
    ciencias.
19. No todos los libros en la biblioteca tienen más de 100 páginas.
20. Existe un usuario que ha tomado prestados todos los libros de la
    sección infantil.


% =================================================================
% | HECHOS (Aserciones sobre el mundo)
% =================================================================

% Hecho 1: Libros que tiene cada biblioteca.
tiene_libro(biblioteca_nacional, 'Don Quijote de la Mancha').
tiene_libro(biblioteca_nacional, 'Cien años de soledad').
tiene_libro(biblioteca_nacional, 'El coronel no tiene quien le escriba').
tiene_libro(biblioteca_nacional, 'Crónica de una muerte anunciada').
tiene_libro(biblioteca_nacional, 'El Libro Troll').
tiene_libro(biblioteca_nacional, 'Análisis Léxico y Sintáctico con ANTLR').
tiene_libro(biblioteca_nacional, 'Programación Lógica y funcional').
tiene_libro(biblioteca_nacional, 'El Principito').
tiene_libro(biblioteca_nacional, 'Termodinámica').
tiene_libro(biblioteca_nacional, 'Cálculo I').
tiene_libro(biblioteca_nacional, 'Relatividad').
tiene_libro(biblioteca_nacional, 'El patito feo'). % Para la sección infantil
tiene_libro(biblioteca_nacional, 'Caperucita Roja'). % Para la sección infantil

% Hecho 3: Libros que están en más de una biblioteca.
tiene_libro(biblioteca_central, 'Termodinámica de Fluidos').
tiene_libro(biblioteca_nacional, 'Termodinámica de Fluidos').

% Hecho 4: Libros que son raros (no prestables).
es_raro('Termodinámica de Fluidos').
es_raro('Manuscritos Antiguos de Prolog').

% Hecho 5: Número de libros en una biblioteca (para la condición > 10000).
num_libros(biblioteca_central, 12001).

% Hecho 7: Autor y sus libros (para la condición > 5 libros).
autor_de('Cien años de soledad', gabriel_garcia_marquez).
autor_de('El coronel no tiene quien le escriba', gabriel_garcia_marquez).
autor_de('Crónica de una muerte anunciada', gabriel_garcia_marquez).
autor_de('El Libro Troll', gabriel_garcia_marquez).
autor_de('Análisis Léxico y Sintáctico con ANTLR', gabriel_garcia_marquez).
autor_de('Programación Lógica y funcional', gabriel_garcia_marquez). % 6 libros
autor_de('Don Quijote de la Mancha', miguel_de_cervantes).
autor_de('El Principito', antoine_de_saint_exupery).
autor_de('El patito feo', hans_christian_andersen).
autor_de('Caperucita Roja', charles_perrault).

% Hecho 8, 9: Estado de los libros.
estado('Cien años de soledad', malo). % Condición para la regla 8
estado('Don Quijote de la Mancha', bueno).
estado('El Libro Troll', bueno).
estado('Cálculo I', bueno).
estado('El Principito', bueno).
estado('El patito feo', bueno).
estado('Caperucita Roja', bueno).
estado('Termodinámica de Fluidos', bueno). % A pesar del estado, sigue siendo raro.

% Hecho 10: Usuarios registrados.
usuario_registrado(usuario_a).
usuario_registrado(usuario_b).
usuario_registrado(usuario_c). % El que pide la sección infantil

% Hecho 11: Libros que solo se pueden consultar en la biblioteca (no prestables).
solo_consulta('Manuscritos Antiguos de Prolog').
solo_consulta('Don Quijote de la Mancha').

% Hecho 14: Usuarios con multas.
tiene_multa(jorge).
tiene_multa(kevin).

% Hecho 15, 18: Ubicación de los libros en secciones y materia de la sección.
ubicado_en('Termodinámica', seccion_ciencias).
ubicado_en('Cálculo I', seccion_ciencias).
ubicado_en('Relatividad', seccion_ciencias).
ubicado_en('El patito feo', seccion_infantil).
ubicado_en('Caperucita Roja', seccion_infantil).
ubicado_en('Cien años de soledad', seccion_literatura).
ubicado_en('El coronel no tiene quien le escriba', seccion_literatura).
ubicado_en('Crónica de una muerte anunciada', seccion_literatura).
es_materia(seccion_ciencias, ciencias).

% Hecho 16: Número de ejemplares (ejemplares > 1).
num_ejemplares('El Libro Troll', 70).
num_ejemplares('Don Quijote de la Mancha', 1).

% Hecho 19: Páginas de los libros (condición para la regla 19).
paginas('El Principito', 96). % Libro con menos de 100 páginas.
paginas('Cien años de soledad', 410).

% Hecho 20: Libros actualmente prestados.
prestado('El patito feo', usuario_c).
prestado('Caperucita Roja', usuario_c).
prestado('El Libro Troll', usuario_a).
prestado('Cálculo I', usuario_a).
prestado('Relatividad', usuario_a).
prestado('Termodinámica', usuario_a). % Usuario A tiene 4 préstamos.


% =================================================================
% | REGLAS (Lógica y condiciones)
% =================================================================

% Regla 2: Todos los libros en la Biblioteca Nacional están catalogados.
esta_catalogado(X) :- tiene_libro(biblioteca_nacional, X).

% Regla 4: Si un libro es raro, entonces no se puede prestar.
% Regla 11: Libros que solo se pueden consultar (tampoco son prestables).
no_prestable(X) :- es_raro(X).
no_prestable(X) :- solo_consulta(X).

% Regla 5: La Biblioteca Central tiene más de 10,000 libros. (Regla de verificación)
tiene_mas_de_diez_mil_libros(B) :- num_libros(B, N), N > 10000.

% Regla 7: Existe un autor que tiene más de 5 libros publicados.
autor_con_mas_de_cinco_libros(A) :-
    setof(L, autor_de(L, A), ListaLibros), % Encuentra todos los libros (L) de un autor (A)
    length(ListaLibros, N),               % Cuenta cuántos libros hay (N)
    N > 5.                                % Verifica si el conteo es mayor a 5

% Regla 9: Si un libro está en buen estado, puede ser prestado. (Incorpora las reglas 4 y 11)
prestable(X) :-
    estado(X, bueno),
    \+ no_prestable(X),
    \+ es_raro(X).

% Regla 10: Todos los usuarios registrados pueden tomar prestado un libro.
% Incorpora las reglas 14 y 17.
puede_prestar(U, L) :-
    usuario_registrado(U),
    prestable(L),
    \+ tiene_multa(U),         % Regla 14
    \+ necesita_devolver(U).   % Regla 17

% Regla 12: Todo libro prestado debe ser devuelto en 15 días. (Regla de declaración de política)
debe_devolver_en_dias(Libro, 15) :- prestado(Libro, _).

% Regla 13: Hay un libro que nadie ha pedido en préstamo. (Libro que existe, pero no aparece en los hechos 'prestado/2')
nunca_prestado(L) :-
    tiene_libro(_, L),
    \+ prestado(L, _).

% Regla 15: Todos los libros escritos por un mismo autor están en la misma sección. (Regla de verificación/restricción)
% Esta regla falla (es falsa) si encuentra dos libros del mismo autor en secciones diferentes.
cumple_uniformidad_seccion(A) :-
    autor_de(L1, A),
    autor_de(L2, A),
    ubicado_en(L1, S1),
    ubicado_en(L2, S2),
    S1 \= S2,
    !, fail. % Falla si encuentra un contraejemplo.
cumple_uniformidad_seccion(_). % Si el fail no se activa, la regla es verdadera.

% Regla 17: Todo usuario con más de tres préstamos debe devolver uno para pedir otro.
num_prestamos(U, N) :-
    findall(L, prestado(L, U), ListaPrestamos),
    length(ListaPrestamos, N).

necesita_devolver(U) :-
    num_prestamos(U, N),
    N > 3.

% Regla 18: Hay una sección de la biblioteca donde todos los libros son de ciencias. (Regla de verificación)
es_seccion_tematica_pura(S, Materia) :-
    es_materia(S, Materia),
    % Verifica que NO exista un libro ubicado en S que NO sea de la Materia esperada.
    ubicado_en(L, S),
    L \= 'Termodinámica', % Ejemplo de un libro que no es de ciencias (debería fallar si no todos fueran)
    \+ es_de_materia(L, Materia),
    !, fail.
es_seccion_tematica_pura(S, Materia) :- es_materia(S, Materia). % Si el fail no se activa, es una sección pura.

% Regla auxiliar para es_seccion_tematica_pura
es_de_materia(L, ciencias) :-
    ubicado_en(L, S),
    es_materia(S, ciencias).

% Regla 19: No todos los libros en la biblioteca tienen más de 100 páginas. (Regla de verificación)
existe_libro_menos_de_cien_paginas :-
    paginas(L, N),
    N =< 100.

% Regla 20: Existe un usuario que ha tomado prestados todos los libros de la sección infantil. (Regla de verificación)
ha_prestado_toda_seccion(U, S) :-
    % 1. Encontrar todos los libros (L) en la sección (S).
    findall(L, ubicado_en(L, S), LibrosSeccion),
    % 2. Verificar que el usuario (U) haya prestado CADA UNO de esos libros.
    \+ ( member(LibroFaltante, LibrosSeccion), \+ prestado(LibroFaltante, U) ).

usuario_presto_toda_seccion_infantil(U) :-
    ha_prestado_toda_seccion(U, seccion_infantil).

% CONSULTAS DE EJEMPLO:
% ?- prestable('Don Quijote de la Mancha'). % Falso por la regla 11 (solo_consulta)
% ?- prestable('El Libro Troll'). % Verdadero (bueno, no_raro, no_solo_consulta)
% ?- puede_prestar(usuario_b, 'El Libro Troll'). % Verdadero (registrado, sin multa, libro prestable)
% ?- puede_prestar(usuario_a, 'Don Quijote de la Mancha'). % Falso (libro no prestable)
% ?- necesita_devolver(usuario_a). % Verdadero (tiene 4 préstamos)
% ?- autor_con_mas_de_cinco_libros(gabriel_garcia_marquez). % Verdadero
% ?- cumple_uniformidad_seccion(gabriel_garcia_marquez). % Verdadero (todos en literatura)
% ?- es_seccion_tematica_pura(seccion_ciencias, ciencias). % Verdadero