# Funciones Prolog
funciones predefinidas más comunes en el lenguaje de Prolog, junto con una breve descripción de su propósito y un ejemplo práctico.

## Entrada/Salida y Formato
Función	Descripción	Ejemplo
write/1	Muestra el término (texto o valor) especificado en el argumento en la consola.	
?- write('¡Hola desde Prolog!').


Salida: ¡Hola desde Prolog!

writeln/1	Similar a write/1, pero automáticamente añade un salto de línea al final del texto impreso.	
?- writeln('Primer línea.').


Salida: Primer línea. (y luego un salto de línea)

nl/0	Inserta un nuevo salto de línea en la salida de la consola en la posición actual.	
?- write('Parte Uno'), nl, write('Parte Dos').


Salida: Parte Uno


Parte Dos

tab/1	Imprime un número específico de espacios en blanco, dado por el argumento numérico.	
?- write('Inicio'), tab(5), write('Final').


Salida: Inicio Final

read/1	Lee un término (valor) ingresado por el usuario desde el teclado, esperando que termine con un punto (.).	
?- read(EntradaUsuario).


Usuario escribe: dato_ejemplo.


Resultado: EntradaUsuario = dato_ejemplo.

get_char/1	Lee y unifica con la variable un solo carácter presionado en el teclado.	
?- get_char(Caracter).


Usuario presiona: z


Resultado: Caracter = 'z'.

put_char/1	Imprime un único carácter especificado en el argumento en la consola.	
?- put_char('P').


Salida: P




## Control de Flujo y Lógica
Función	Descripción	Ejemplo
true/0	Es una meta que siempre se cumple (siempre devuelve true). Útil para validaciones o bases de reglas.	
?- true.


Resultado: true.

fail/0	Es una meta que siempre falla (siempre devuelve false). Se utiliza comúnmente para forzar el backtracking.	
?- write('Intento fallido'), fail.


Resultado: false.

not/1	El predicado de negación (\+ es su operador más común). Se cumple si el argumento (la condición) falla.	
?- not(5 == 10). (5 no es igual a 10, por lo tanto not se cumple).


Resultado: true.

!/0 (cut)	El corte (cut) detiene la búsqueda de más soluciones en ese punto de la regla, fijando las decisiones tomadas hasta el momento e impidiendo el backtracking hacia atrás.	
Ejemplo de uso en una regla: clasificar(Edad, 'Adulto') :- Edad >= 18, !.


clasificar(_, 'Menor').

;/2	El OR lógico. Permite probar la alternativa de la derecha si la de la izquierda falla, o generar múltiples soluciones.	
?- Edad = 18 ; Edad = 20.


Resultado: Edad = 18 ;


Edad = 20.

->/2	El operador If-Then. Si la condición a la izquierda es verdadera, se ejecuta la acción de la derecha.	
?- (Dia == 'Sabado' -> writeln('Fin de semana')).


Resultado (si Dia es 'Sabado'): Fin de semana

->/3	El operador If-Then-Else. Si la condición (primer argumento) es verdadera, se ejecuta el segundo; si no, se ejecuta el tercero.	
?- (Temperatura > 30 -> writeln('Calor') ; writeln('Frio')).


Resultado (si Temp=25): Frio

forall/2	Se cumple si todos los valores generados por el primer argumento satisfacen el segundo argumento (la condición).	
?- forall(member(N,[2,4,6]), N mod 2 =:= 0). (Todos los números son pares).


Resultado: true.




## Operaciones Aritméticas y Comparación
Función	Descripción	Ejemplo
is/2	Evalúa la expresión matemática de la derecha y asigna el resultado a la variable de la izquierda.	
?- Resultado is 10 * (2 + 1).


Resultado: Resultado = 30.

>/2	Compara si el valor del lado izquierdo es estrictamente mayor que el derecho.	
?- 15 > 10.


Resultado: true.

</2	Compara si el valor del lado izquierdo es estrictamente menor que el derecho.	
?- 5 < 50.


Resultado: true.

>=/2	Compara si el valor izquierdo es mayor o igual que el derecho.	
?- 100 >= 100.


Resultado: true.

=</2	Compara si el valor izquierdo es menor o igual que el derecho.	
?- 9 =< 10.


Resultado: true.

=:=/2	Compara si dos expresiones son iguales después de ser evaluadas aritméticamente.	
?- 2 * 3 =:= 5 + 1.


Resultado: true.

===/2	Compara si dos expresiones son diferentes después de ser evaluadas aritméticamente.	
?- 7 =\=\= 4 + 2.


Resultado: true.




## Unificación e Igualdad
Función	Descripción	Ejemplo
=/2	El operador de unificación. Intenta hacer que los dos términos (variables, átomos o estructuras) sean iguales.	
?- Ciudad = 'Morelia'.


Resultado: Ciudad = 'Morelia'.

==/2	Comprueba si dos términos son idénticos (mismos valores, mismos tipos, mismas variables). No intenta unificar.	
?- atomo == atomo.


Resultado: true.

==/2	Comprueba si dos términos no son idénticos.	
?- 3 \== 3.0. (El tipo es distinto en algunos dialectos)


Resultado: true.




## Manipulación de la Base de Datos Dinámica
Función	Descripción	Ejemplo
assert/1	Agrega un nuevo hecho o regla al programa en una posición indeterminada (depende del dialecto de Prolog).	
?- assert(persona(alex, 25)).


Resultado: true (el hecho se añade).

assertz/1	Agrega un nuevo hecho o regla al final de la base de datos dinámica.	
?- assertz(color(azul)).


Resultado: true (color(azul) es el último hecho de color/1).

asserta/1	Agrega un nuevo hecho o regla al inicio de la base de datos dinámica.	
?- asserta(color(rojo)).


Resultado: true (color(rojo) es el primer hecho de color/1).

retract/1	Elimina el primer hecho o regla que unifique con el argumento de la base de datos.	
Si existe empleado(juan, 5000): ?- retract(empleado(juan, _)).


Resultado: true (el hecho es eliminado).

retractall/1	Elimina todos los hechos o reglas que unifiquen con el argumento de la base de datos.	
?- retractall(tarea_pendiente(_)). (Borra todas las tareas).


Resultado: true (todos los hechos son eliminados).




## Predicados de Lista
Función	Descripción	Ejemplo
member/2	Determina si un elemento (primer argumento) está contenido dentro de una lista (segundo argumento).	
?- member(c, [a, b, c, d]).


Resultado: true.

append/3	Se utiliza para concatenar dos listas (primeros dos argumentos) en una tercera, o para dividir una lista.	
?- append([l, o], [g, i, c, a], Resultado).


Resultado: Resultado = [l, o, g, i, c, a].

length/2	Obtiene la longitud (número de elementos) de una lista, o verifica si la lista tiene una longitud específica.	
?- length([p, r, o, l, o, g], L).


Resultado: L = 6.

reverse/2	Crea una nueva lista (segundo argumento) con los elementos de la lista original (primer argumento) en orden inverso.	
?- reverse([x, y, z], Invertida).


Resultado: Invertida = [z, y, x].

sort/2	Ordena la lista de entrada y elimina los elementos duplicados.	
?- sort([9, 1, 5, 9, 1], Ordenada).


Resultado: Ordenada = [1, 5, 9].

msort/2	Ordena la lista de entrada pero mantiene los elementos duplicados (multi-set sort).	
?- msort([4, 1, 4, 2], Ordenada).


Resultado: Ordenada = [1, 2, 4, 4].




## Predicados de Tipo
Función	Descripción	Ejemplo
atom/1	Se cumple si el argumento es un átomo (constante textual, como una palabra que no es una variable).	
?- atom('nombre_ejemplo').


Resultado: true.

integer/1	Se cumple si el argumento es un número entero.	
?- integer(150).


Resultado: true.

number/1	Se cumple si el argumento es cualquier tipo de número (entero o de punto flotante/decimal).	
?- number(3.14159).


Resultado: true.

var/1	Se cumple si el argumento es una variable que no ha sido unificada (no tiene valor asignado).	
?- var(MiVariableSinAsignar).


Resultado: true.

nonvar/1	Se cumple si el argumento no es una variable sin asignar (es un átomo, un número, una lista o una variable ya unificada).	
?- Valor = 10, nonvar(Valor).


Resultado: true.




## Generación de Rangos
Función	Descripción	Ejemplo
between/3	Genera o verifica números enteros dentro de un rango (desde, hasta, número generado). El rango incluye ambos límites.	
?- between(1, 5, N).


Resultado: N = 1 ; N = 2 ; N = 3 ; N = 4 ; N = 5.
