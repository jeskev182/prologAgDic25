## 1. Diferencia entre las Estructuras Condicionales (`if`, `cond`, `when`, `unless`)

| Estructura | Función Principal | Comportamiento | Similitud con otros lenguajes |
| :--- | :--- | :--- | :--- |
| **`if`** | **Decisión Binaria** | Evalúa una condición. Si es **verdadera**, ejecuta la primera acción. Si es **falsa**, ejecuta la acción alternativa (opcional). | `if...else` |
| **`cond`** | **Múltiples Casos** | Revisa una serie de pares `(condición acción)`. Ejecuta las acciones del **primer par** cuya condición se cumpla. Permite una condición final genérica (`T`) para capturar el resto de los casos. | `switch` o múltiples `if...elif...else` |
| **`when`** | **Solo Si se Cumple** | Ejecuta un bloque de código **únicamente si la condición es verdadera** (`T`). Si es falsa, no hace nada y devuelve `NIL`. | Un `if` sin `else` |
| **`unless`** | **Solo Si NO se Cumple** | Ejecuta un bloque de código **únicamente si la condición es falsa** (`NIL`). Es la negación lógica de `when`. | Un `if` que actúa sobre la negación de la condición |

---

## 2. Retorno y Combinación de `car` y `cdr`
#### ¿Qué devuelven?

* **`car`**: Devuelve el **primer elemento** de una lista. (Piensa en `C`ar como el **C**omienzo de la lista).
* **`cdr`**: Devuelve el **resto de la lista**, excluyendo el primer elemento. El resultado siempre será otra lista (o `NIL` si la lista original solo tenía un elemento). (Piensa en `D`r como el **D**eclive o el resto)

#### ¿Cómo se combinan?
Puedes **anidar** (`nesting`) estas funciones para navegar más allá del primer elemento:
* Para obtener el **segundo elemento**, usas `(car (cdr lista))` o su abreviatura **`cadr`**.
    * `cdr` obtiene el resto de la lista (comenzando en el segundo elemento).
    * `car` obtiene el primer elemento de ese resto (es decir, el segundo elemento de la lista original).
* Para el tercer elemento, usarías `(caddr lista)`, y así sucesivamente (ej. `cadddr`, `cadr`). Esta combinación anidada de `car` y `cdr` permite acceder a cualquier elemento de la lista sin usar índices numéricos.




## Ejercicio 1 – N-ésimo elemento con car/cdr
Escribe una función ‘(n-esimo n lista)‘ que devuelva el n-ésimo elemento de una lista utilizando solo ‘car‘ y ‘cdr‘. Ejemplo:
(n-esimo 3 '(a b c d e)) ;; => c

(defun n-esimo (n lista)
  (if (= n 1)
      (car lista)                 
      (n-esimo (- n 1) (cdr lista))
  )
)




## Ejercicio 2 – Filtrar positivos con when
Escribe una función ‘(filtra-positivos lista)‘ que reciba una lista de números y devuelva una nueva lista con solo los números positivos. Usa ‘when‘ dentro de un ‘mapcar‘ o ‘loop‘. Ejemplo:
(filtra-positivos '(-2 0 3 -5 7)) ;; => (3 7)

(defun filtra-positivos (lista)
  (loop for x in lista
        when (> x 0)
        do (print x)
  )
)




## Ejercicio 3 – Clasificación con cond
Escribe una función ‘(clasifica-numero n)‘ que:
• Devuelva ‘"Negativo"‘ si n < 0
• Devuelva ‘"Cero"‘ si n = 0
• Devuelva ‘"Pequeño"‘ si 1 <= n <= 10
• Devuelva ‘"Mediano"‘ si 11 <= n <= 100
• Devuelva ‘"Grande"‘ si n > 100
Ejemplo:
(clasifica-numero 57) ;; => "Mediano"

(defun clasifica-numero (n)
  (cond
    ((< n 0) '(Negativo))
    ((= n 0) '(Cero))
    ((and (>= n 1) (<= n 10)) '(Pequeño))
    ((and (>= n 11) (<= n 100)) '(Mediano))
    ((> n 100) '(Grande))
  )
)




## Ejercicio 4 – Suma de pares con unless
Escribe una función ‘(suma-pares lista)‘ que:
• Devuelva la suma de todos los números pares en la lista.
• Ignore los impares usando ‘unless‘.
Ejemplo:
(suma-pares '(1 2 3 4 5 6)) ;; => 12

(defun suma-pares (lista)
  (loop for x in lista
        unless (oddp x)
        sum x)
) 




## Ejercicio 5 – Procesamiento de listas con car y cdr
Escribe una función ‘(procesa-lista lista)‘ que:

Si la lista está vacía → devuelve ‘"Lista vacía"‘.
Si el primer elemento (‘car‘) es un número mayor a 50 → devuelve ‘"Grande"‘.
Si el primer elemento es una sublista → devuelve ‘"Sublista detectada"‘.
En cualquier otro caso → devuelve ‘"Caso general"‘.
Ejemplos:
(procesa-lista '()) ;; => "Lista vacía"
(procesa-lista '(60 1 2)) ;; => "Grande"
(procesa-lista '((1 2) 3 4)) ;; => "Sublista detectada"
(procesa-lista '(10 20 30)) ;; => "Caso general"
(defun procesa-lista (lista)
    (cond
    ((null lista) '(Lista Vacia))
    ((and (numberp (car lista)) (> (car lista) 50)) '(Grande))
    ((listp (car lista)) '(Sublista Detectada))
    (t '(Caso General))
    )
)