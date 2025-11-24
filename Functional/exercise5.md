## Ejercicios de Preorden (Clisp)

Estos ejercicios muestran la conversión de expresiones matemáticas **infijas** (la forma estándar) a la **notación preorden** (la forma de lista ampliamente utilizada en áreas computacionales y específicamente en Common lisp)


## Proceso de pasar algo a preorden

Tenemos que ir pasando los operadores a la izquierda de la expresión, en base a su orden de jerarquía matemática.
De izquierda a derecha y colocando el que se mueve justo al inicio del renglón, es decir, antes que cualquier caracter.
Las partes de las que se les movió ya su operador de consideran ya como un sólo atomo
| Tipo de Notación | Expresión |
| :---: | :--- |
| **Infija** (Estándar) | **$3 + 6 + 8 \cdot 3 \cdot 5 + 6 + 3 + 3 \cdot 2 - 1 + 10 / 2$** |
| **Preorden** (Clisp) | '3 + 6 + \cdot \cdot 8 3 5 + 6 + 3 + \cdot 3 2 - 1 + / 10 2' |
| --                   | '+ + 3 6 \cdot \cdot 8 3 5 + 6 + 3 + \cdot 3 2 - 1 + / 10 2' |
| --                   | '+ + + + + 3 6 \cdot \cdot 8 3 5 6 3 \cdot 3 2 - 1 + / 10 2' |
| --                   | '- + + + + + 3 6 \cdot \cdot 8 3 5 6 3 \cdot 3 2 1 + / 10 2' |
| --                   | '+ - + + + + + 3 6 \cdot \cdot 8 3 5 6 3 \cdot 3 2 1 / 10 2' |
---

### 1. Ejercicio de Cálculo Complejo

| Tipo de Notación | Expresión |
| :---: | :--- |
| **Infija** (Estándar) | **$3 + 6 + 8 \cdot 3 \cdot 5 + 6 + 3 + 3 \cdot 2 - 1 + 10 / 2$** |
| **Preorden** (Clisp) | `(+ (- (+ (+ (+ (+ 3 6 (* (* 8 3) 5)) 6) 3) (* 3 2)) 1) (/ 10 2))` |
| **Resultado** | **148** |

---

### 2. Ejercicio con Multiplicaciones y Divisiones Entrelazadas

| Tipo de Notación | Expresión |
| :---: | :--- |
| **Infija** (Estándar) | **$6 + 6 \cdot 5 \cdot 3 + 2 - 1 + 3 + 6 / 2 \cdot 5 + 2$** |
| **Preorden** (Clisp) | `(+ (+ (+ (- (+ (+ 6 (* (* 6 5) 3)) 2) 1) 3) (* (/ 6 2) 5)) 2)` |
| **Resultado** | **117** |

---

### 3. Ejercicio de Sumas y Restas

| Tipo de Notación | Expresión |
| :---: | :--- |
| **Infija** (Estándar) | **$10 + 10 - 2 + 2 \cdot 3 + 5 + 6 - 3 + 6 \cdot 3 + 4$** |
| **Preorden** (Clisp) | `(+ (+ (- (+ (+ (+ (- (+ 10 10) 2) (* 2 3)) 5) 6) 3) (* 6 3)) 4)` |
| **Resultado** | **54** |

---

### 4. Ejercicio con Producto de Tres Factores

| Tipo de Notación | Expresión |
| :---: | :--- |
| **Infija** (Estándar) | **$11 + 2 + 3 + 12 - 3 \cdot 5 \cdot 3 + 2 + 10 / 2 + 6 \cdot 3$** |
| **Preorden** (Clisp) | `(+ (+ (+ (- (+ (+ (+ 11 2) 3) 12) (* (* 3 5) 3)) 2) (/ 10 2)) (* 6 3))` |
| **Resultado** | **8** |

---

### 5. Ejercicio de Solo Sumas y Productos

| Tipo de Notación | Expresión |
| :---: | :--- |
| **Infija** (Estándar) | **$11 \cdot 6 + 12 + 13 + 2 \cdot 4 + 6 \cdot 5 + 7 \cdot 3 + 21$** |
| **Preorden** (Clisp) | `(+ (+ (+ (+ (+ (+ (* 11 6) 12) 13) (* 2 4)) (* 6 5)) (* 7 3)) 21)` |
| **Resultado** | **171** |

---