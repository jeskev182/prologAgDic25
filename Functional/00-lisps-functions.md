# Funciones Importantes en Lisp

---

1.  **`defun`**: Define una nueva función. Es la forma estándar de crear funciones reutilizables.
    * Ejemplo: `(defun suma (x y) (+ x y))`

---

2.  **`let`**: Crea y asigna variables locales. A diferencia de `setq` o `defvar`, que modifican variables existentes o globales, `let` crea un nuevo espacio de nombres temporal donde las variables solo existen mientras el bloque de código se está ejecutando. Estas variables son accesibles únicamente dentro del cuerpo del código. Una vez que el `let` finaliza, las variables y sus valores se eliminan de la memoria.
    * Ejemplo: `(let ((x 10)) (* x 2))`

---

3.  **`setq`**: Viene de "set quote". Asigna un valor a una variable. Es una de las formas más simples de asignación. `setq` es, en esencia, una forma abreviada de `setf` para el caso de variables.
    * Ejemplo: `(setq a 5)`

---

4.  **`setf`**: Una macro de asignación más general y poderosa que puede modificar casi cualquier lugar de almacenamiento, como elementos de listas, arreglos y variables.
    * Ejemplo: `(setf (car '(1 2 3)) 0)`

---

5.  **`if`**: La macro condicional básica que evalúa un bloque de código si la condición es verdadera, y otro si es falsa. Si la condición se evalúa como cualquier cosa que no sea `nil`, se considera "verdadera" en Lisp. El bloque que se ejecuta si es falsa es opcional. Para agrupar varias expresiones en un solo bloque, se utiliza la macro `progn`. Sin embargo, la mayoría de las macros de control de flujo en Lisp, como `if`, `when`, `cond`, `dolist`, etc., tienen un cuerpo implícito. Esto significa que en las secciones verdadera/falsa se puede usar una o más expresiones y que el valor tomado como "retorno" es el de la última expresión, tal cual como si usáramos `progn`.
    * Ejemplo: `(if (> 5 3) 'verdadero 'falso)`

---

6.  **`when`**: Evalúa un bloque de código solo si la condición es verdadera.
    * Ejemplo: `(when (zerop x) (format t "Cero"))`

---

7.  **`unless`**: Evalúa un bloque de código solo si la condición es falsa. Es el opuesto de `when`.
    * Ejemplo: `(unless (plusp x) (format t "No es positivo"))`

---

8.  **`cond`**: Es la abreviatura de "conditional". Permite evaluar múltiples condiciones en una secuencia. Es una alternativa a anidar múltiples `if`. Son una serie de cláusulas, donde cada cláusula es una lista que contiene una expresión de prueba y una o más expresiones de resultado. Lisp evalúa cada cláusula en orden hasta que encuentra una cuya prueba sea verdadera.
    * El proceso es el siguiente:
        * Lisp evalúa la prueba de la primera cláusula.
        * Si la prueba es verdadera (cualquier valor que no sea `nil`), Lisp ejecuta las expresiones de resultado de esa misma cláusula. El valor de la última expresión es lo que `cond` devuelve, y la ejecución termina.
        * Si la prueba es falsa (`nil`), Lisp pasa a la siguiente cláusula y repite el proceso.
        * A menudo, la última cláusula usa `t` como su prueba. Como `t` siempre es verdadero, esta cláusula actúa como un caso por defecto, asegurando que `cond` siempre devuelva un valor si ninguna de las condiciones anteriores se cumple.
    * Ejemplo: `(cond ((< x 0) 'negativo) ((> x 0) 'positivo) (t 'cero))`

---

9.  **`cons`**: Construye una nueva lista o par. Es el constructor fundamental de listas en Lisp. Es el "ladrillo" básico para construir estructuras de datos en Lisp. `cons` crea un nuevo "par cons", que es una estructura de dos partes que contiene un valor para cada argumento. Cuando se encadenan varios pares `cons`, forman una lista. La forma más común de usarlo es para añadir un nuevo elemento al principio de una lista existente.
    * Creación de una nueva lista: `(cons 'a '())` ; -> `(a)`
    * Ejemplo: `(cons 'a '(b c))`

---

10. **`car`**: Devuelve el primer elemento de una lista.
    * Ejemplo: `(car '(1 2 3))`

---

11. **`cdr`**: Devuelve todos los elementos de una lista excepto el primero.
    * Ejemplo: `(cdr '(1 2 3))`

---

12. **Combinaciones `car`/`cdr`**:
    * **`cadr`**: Equivalente a `(car (cdr lista))`. Accede al segundo elemento de la lista.
        * Ejemplo: `(cadr '(a b c))` -> `b`
    * **`cddr`**: Equivalente a `(cdr (cdr lista))`. Devuelve la lista sin los dos primeros elementos.
        * Ejemplo: `(cddr '(a b c))` -> `(c)`
    * **`caar`**: Equivalente a `(car (car lista))`. Accede al primer elemento de la primera sublista.
        * Ejemplo: `(caar '((a b) (c d)))` -> `a`
    * **`caddr`**: Equivalente a `(car (cdr (cdr lista)))`. Accede al tercer elemento de la lista.
        * Ejemplo: `(caddr '(a b c))` -> `c`
    * Lisp proporciona combinaciones hasta de cuatro letras (`a`s y `d`s). Puedes verlas como un mapa de cómo navegar una lista para llegar al valor que necesitas.
        * `cddar` -> `(cdr (cdr (car lista)))`
        * `cdar` -> `(cdr (car lista))`
        * `caaar` -> `(car (car (car lista)))`

---

13. **`list`**: Crea una nueva lista con sus argumentos como elementos.
    * Ejemplo: `(list 1 2 3)`

---

14. **`append`**: Une dos o más listas en una sola nueva lista. Es importante destacar que `append` no modifica las listas originales. En su lugar, construye una nueva lista desde cero. El último argumento de `append` puede ser cualquier objeto de Lisp, no solo una lista. Si no es una lista, se convierte en el último elemento de la nueva lista, formando un par punteado.
    * Ejemplo: `(append '(1 2 3) '(4 5 6))` ; -> `(1 2 3 4 5 6)`
    * Par punteado: `(append '(a b) 'c)` ; -> `(a b . c)`

---

15. **`length`**: Devuelve el número de elementos en una lista (o el tamaño de un vector).
    * Ejemplo: `(length '(a b c d))`

---

16. **`reverse`**: Crea una nueva lista con los elementos en orden inverso.
    * Ejemplo: `(reverse '(1 2 3 4 5))` ; -> `(5 4 3 2 1)`

---

17. **`member`**: Busca un elemento en una lista y devuelve la sublista que comienza con ese elemento.
    * Ejemplo: `(member 'b '(a b c))` ; -> `(b c)`

---

18. **`+`**: Realiza la suma de números.
    * Ejemplo: `(+ 1 2 3)` ; -> `6`

---

19. **`-`**: Realiza la resta o negación.
    * Ejemplo: `(- 10 5)` ; -> `5`

---

20. **`*`**: Realiza la multiplicación.
    * Ejemplo: `(* 2 3 4)` ; -> `24`

---

21. **`/`**: Realiza la división.
    * Ejemplo: `(/ 10 2)` ; -> `5`

---

22. **`format`**: Una poderosa función de impresión que permite un formateo detallado. A menudo comparada con `printf` en C, pero con mucha más flexibilidad. Su nombre viene de "formatear". Las directivas de formato son lo que hace que `format` sea tan versátil. Aquí están algunas de las más utilizadas:
    * `~a`: Imprime un argumento de forma legible para humanos (como `princ`).
    * `~s`: Imprime un argumento de forma legible para Lisp (como `prin1`).
    * `~%`: Imprime un carácter de nueva línea.
    * `~t`: Imprime espacios, útil para tabulación. Por ejemplo, `~10t` imprime 10 espacios.
    * `~d`: Imprime un número en formato decimal.
    * `~f`: Imprime un número de punto flotante.
    * Ejemplo: `(format t "Hola ~a" "Mundo")`
    * Ejemplo 1: Imprimir una cadena simple en la consola.
      `(format t "Hola, mundo.")` ; Imprime "Hola, mundo." en la consola
    * Ejemplo 2: Usar directivas para reemplazar valores.
      `(setq nombre "Juan")`
      `(setq edad 30)`
      `(format t "Mi nombre es ~a y tengo ~a años." nombre edad)`
      ; Imprime "Mi nombre es Juan y tengo 30 años."
    * Ejemplo 3: Obtener una cadena de texto sin imprimirla.
      `(setq resultado (format nil "El resultado es ~a." 100))`
      `(print resultado)`
      ; Imprime "El resultado es 100." (como una cadena de texto)

---

23. **`princ`**: Imprime un objeto sin comillas ni caracteres de escape.
    * Ejemplo: `(princ "cadena")`

---

24. **`print`**: Imprime un objeto, añade un salto de línea y devuelve el objeto impreso.
    * Ejemplo: `(print "hola")`

---

25. **`read`**: Lee una expresión Lisp desde la entrada estándar. La función `read` procesa un flujo de caracteres, ignorando los espacios en blanco y los comentarios, hasta que puede formar una expresión-S completa. Una vez que analiza con éxito un átomo (como un número, una cadena o un símbolo) o una lista completa, devuelve ese objeto de Lisp.
    * Ejemplo: `(read)`

---

26. **`eq`**: Compara si dos objetos son el mismo objeto en memoria. 
    * Ejemplo: `(eq 'a 'a)` ; -> `T`

---

27. **`eql`**: Compara si dos objetos son el mismo objeto o si son números con el mismo valor y tipo.
    * Ejemplo: `(eql 5.0 5.0)` ; -> `T`

---

28. **`equal`**: Compara si dos objetos tienen la misma estructura y contenido. Es una comparación profunda que revisa el contenido de listas, cadenas de texto, vectores y otras estructuras de datos de manera recursiva. Recorre y compara el contenido de estructuras de datos como listas y cadenas.
    * Ejemplo: `(equal '(1 2) '(1 2))` ; -> `T`

---

29. **`null`**: Devuelve `T` si el argumento es `NIL` o la lista vacía `()`.
    * Ejemplo: `(null '())` ; -> `T`

---

30. **`numberp`**: Un predicado que devuelve `T` si el argumento es un número.
    * Ejemplo: `(numberp 10)` ; -> `T`

---

31. **`listp`**: Un predicado que devuelve `T` si el argumento es una lista.
    * Ejemplo: `(listp '(a b))` ; -> `T`

---

32. **`loop`**: Una macro de iteración extremadamente versátil que puede manejar bucles de diferentes tipos.
    * Ejemplo: `(loop for i from 1 to 3 do (princ i))`

---

33. **`dotimes`**: Una macro para bucles que itera un número fijo de veces.
    * Ejemplo: `(dotimes (i 3) (format t "~a " i))`

---

34. **`dolist`**: Una macro para bucles que itera sobre los elementos de una lista.
    * Ejemplo: `(dolist (x '(a b c)) (princ x))` ; -> `abc`

---

35. **`mapcar`**: Aplica una función a cada elemento de una o más listas y devuelve una nueva lista con los resultados.
    * Ejemplo: `(mapcar #'(lambda (x) (* x 2)) '(1 2 3))` ; -> `(2 4 6)`

---

36. **`funcall`**: Llama a una función con los argumentos dados.
    * Ejemplo: `(funcall #'+ 1 2 3)`

---

37. **`apply`**: Llama a una función con los argumentos dados en una lista.
    * Ejemplo: `(apply #'+ '(1 2 3))`

---

38. **`lambda`**: Crea una función anónima (sin nombre).
    * Ejemplo: `#'(lambda (x) (+ x 1))`

---

39. **`progn`**: Evalúa secuencialmente una serie de expresiones y devuelve el resultado de la última. Es útil para agrupar varias expresiones en un solo bloque.
    * Ejemplo: `(progn (setq x 10) (setq y 20) (+ x y))`

---

40. **`quote` (`'`)**: Previene que la expresión sea evaluada.
    * Ejemplo: `(quote (1 2 3))` es equivalente a `'(1 2 3)`

---

41. **`defvar`**: Declara una variable global.
    * Ejemplo: `(defvar *mi-variable* 100)`

---

42. **`defparameter` vs. `defvar`**:
    * **`defvar`**: Declara una variable y asigna el valor inicial solo si la variable no tiene un valor actualmente. Es buena para variables que mantienen un estado persistente que no debe ser reiniciado cada vez que tu código se carga.
        * Ejemplo:
          ```lisp
          (defvar *contador* 0)
          (setq *contador* 10)
          (defvar *contador* 0) ; El valor sigue siendo 10
          *contador* ; -> 10
          ```
    * **`defparameter`**: Declara una variable y siempre asigna el valor inicial, incluso si la variable ya tiene uno. Esto es útil para configuraciones globales o parámetros que te gustaría poder cambiar fácilmente al recargar tu código durante el desarrollo.
        * Ejemplo:
          ```lisp
          (defparameter *debug-mode* nil)
          (setq *debug-mode* t)
          (defparameter *debug-mode* nil) ; El valor se restablece a nil
          *debug-mode* ; -> nil
          ```