% --- Predicados Fundamentales de Listas en Prolog ---

% member/2: Verifica si un elemento (X) pertenece a una lista.
member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

% append/3: Concatena dos listas en una.
append([], L, L).
append([X|Xs], L, [X|R]) :- append(Xs, L, R).

% length/2: Devuelve la longitud (N) de una lista.
length([], 0).
length([_|T], N) :- length(T, N1), N is N1 + 1.

% nth0/3: Devuelve el elemento (X) en la posición N (empezando desde 0).
nth0(0, [X|_], X).
nth0(N, [_|T], X) :- N > 0, N1 is N - 1, nth0(N1, T, X).

% select/3: Elimina un elemento (X) de una lista, obteniendo el resto.
select(X, [X|T], T).
select(X, [Y|T], [Y|R]) :- select(X, T, R).

% reverse/2: Invierte una lista.
reverse([], []).
reverse([H|T], R) :- reverse(T, RT), append(RT, [H], R).

% last/2: Obtiene el último elemento (X) de una lista.
last([X], X).
last([_|T], X) :- last(T, X).

% sum_list/2: Suma todos los elementos numéricos de una lista (S).
sum_list([], 0).
sum_list([X|T], S) :- sum_list(T, S1), S is X + S1.

% max_list/2: Obtiene el máximo valor (M) de una lista.
max_list([X], X).
max_list([X|T], M) :- max_list(T, M1), M is max(X, M1).

% sublist/2: Determina si una lista (Sub) es sublista de otra (List).
sublist(Sub, List) :- append(_, rest1, List), append(Sub, _, rest1).