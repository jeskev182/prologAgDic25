## PRUEBA DE ESCRITORIO DE PREDICADOS EN PROLOG (SISTEMA MÉDICO)

------------------------------------------------------------------------
1. PREDICADO: buscar/3 (CONTAR COINCIDENCIAS DE SÍNTOMAS)
------------------------------------------------------------------------
DEFINICIÓN:
buscar([], _, 0).
buscar(X, E, 1) :- sintomade(X, E).
buscar([X|Xs], E, P) :- enfermedad(E), buscar(X, E, S1), buscar(Xs, E, S2), P is S1 + S2.

CONSULTA:
buscar([diarrea, nauseas], hepatitis, P).

TRAZADO (CREEP):
   Call: buscar([diarrea, nauseas], hepatitis, _P)
   (Regla 3: Descomposición de la lista)
   Call: enfermedad(hepatitis) -> Exit
   Call: buscar(diarrea, hepatitis, S1) -> Exit: S1 = 1
   Call: buscar([nauseas], hepatitis, S2)
      (Sub-llamada recursiva, resuelve nauseas = 1, lista vacía = 0)
   Exit: buscar([nauseas], hepatitis, 1) -> S2 = 1
   Call: P is 1 + 1
   Exit: P is 2

RESULTADO:
P = 2

EXPLICACIÓN DE LA PRUEBA DE ESCRITORIO:
La consulta busca el número de síntomas que coinciden entre la lista ([diarrea, nauseas]) y 'hepatitis'. Prolog utiliza la tercera regla de forma recursiva. Para 'diarrea', se encuentra la coincidencia (S1=1). Luego, para 'nauseas', se encuentra la coincidencia (S2=1). Finalmente, los resultados se suman: P = 1 + 1, resultando en P = 2.

------------------------------------------------------------------------
2. PREDICADO: cantSint/2 (CONTAR SÍNTOMAS TOTALES DE UNA ENFERMEDAD)
------------------------------------------------------------------------
DEFINICIÓN:
cantSint(E, C) :- findall(X, sintomade(X, E), L), length(L, C).

CONSULTA:
cantSint(gripe, C).

TRAZADO (CREEP):
   Call: cantSint(gripe, _C)
   Call: findall(_X, sintomade(_X, gripe), _L)
      (Recopila todas las soluciones para sintomade(X, gripe))
   Exit: findall(_X, sintomade(_X, gripe), [tos, cansancio, fiebre, dolorcabeza]) <-- L
   Call: length([tos, cansancio, fiebre, dolorcabeza], _C)
   Exit: length([tos, cansancio, fiebre, dolorcabeza], 4)

RESULTADO:
C = 4

EXPLICACIÓN DE LA PRUEBA DE ESCRITORIO:
El predicado 'cantSint/2' utiliza 'findall/3' para crear una lista (L) con todos los síntomas asociados a la 'gripe', que son: [tos, cansancio, fiebre, dolorcabeza]. Luego, 'length/2' cuenta los elementos de esta lista, obteniendo el resultado C = 4.

------------------------------------------------------------------------
3. PREDICADO: diagnostico/3 (CALCULAR PORCENTAJE DE COINCIDENCIA)
------------------------------------------------------------------------
DEFINICIÓN:
diagnostico(Sintomas, E, K) :- buscar(Sintomas, E, P), cantSint(E, T), K is P * 100 / T.

CONSULTA:
diagnostico([cansancio, apatia], anemia, P).

TRAZADO (CREEP):
   Call: diagnostico([cansancio, apatia], anemia, _P)
   Call: buscar([cansancio, apatia], anemia, P_coinc)
      (Resuelve que 'cansancio' y 'apatia' coinciden)
   Exit: buscar([cansancio, apatia], anemia, 2) <-- P = 2
   Call: cantSint(anemia, T_total)
      (Resuelve que anemia tiene 3 síntomas: cansancio, apatia, nauseas)
   Exit: cantSint(anemia, 3)              <-- T = 3
   Call: _P is 2*100/3
   Exit: _P is 66.66666666666667

RESULTADO:
P = 66.66666666666667

EXPLICACIÓN DE LA PRUEBA DE ESCRITORIO:
1. Coincidencias (P): 'buscar/3' determina que 2 síntomas del usuario coinciden con 'anemia'.
2. Total (T): 'cantSint/2' determina que 'anemia' tiene un total de 3 síntomas definidos.
3. Cálculo (K): Se aplica la fórmula: K = (2 * 100) / 3.
El resultado es P = 66.67% de coincidencia.

========================================================================
ANEXO: EXPLICACIÓN DE FUNCIONES CLAVE
========================================================================

FUNCION findall/3

PROPÓSITO: Recopilar todas las soluciones posibles de una consulta en una lista. Permite agrupar resultados para poder procesarlos colectivamente.

FORMATO:
findall(Elemento, Condición, Lista)

Elemento: La variable o término que se desea recoger de cada solución.
Condición: El predicado o meta que debe cumplirse.
Lista: Variable que recibe la lista de todos los elementos que cumplieron la condición.

FUNCION length/2

PROPÓSITO: Obtener o verificar la cantidad de elementos de una lista en Prolog.

FORMATO:
length(Lista, Longitud)

Lista: La lista que se desea analizar.
Longitud: Variable o número que representa la cantidad de elementos de la lista.