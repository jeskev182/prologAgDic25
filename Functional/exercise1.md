### 1. Sueldo Anual

Este algoritmo calcula el sueldo anual de un empleado en función de los años que ha trabajado en la empresa, aplicando un aumento de salario según un conjunto de reglas


```lisp
(defun calcular-sueldo (anos-en-empresa)
  (let ((sueldo-base 40000))
    (cond
      ((> anos-en-empresa 10)
       (* sueldo-base 1.10))
      ((> anos-en-empresa 5)
       (* sueldo-base 1.07))
      ((> anos-en-empresa 3)
       (* sueldo-base 1.05))
      (t
       (* sueldo-base 1.03)))))
```
cond: Es la estructura de control ideal para este problema, ya que permite evaluar múltiples condiciones en secuencia.

t: La última cláusula con t actúa como un caso por defecto, cubriendo la condición de "menos de 3 años"

### 2. Lavadora

Este algoritmo determina el nivel de llenado de una lavadora y la cantidad de agua necesaria basándose en el peso de la ropa en libras.

```lisp
(defun nivel-lavadora (peso-libras)
  (cond
    ((> peso-libras 30)
     '(no-funcionara "El peso es demasiado alto."))
    ((>= peso-libras 22)
     '(nivel-maximo "Cantidad de agua: 120 litros"))
    ((>= peso-libras 15)
     '(nivel-alto "Cantidad de agua: 80 litros"))
    ((>= peso-libras 8)
     '(nivel-medio "Cantidad de agua: 50 litros"))
    (t
     '(nivel-minimo "Cantidad de agua: 30 litros"))))
```
`>`: Se utiliza el operador "mayor que" para la primera condición, ya que la lavadora no funciona con más de 30 libras.

`>=`: El operador "mayor o igual que" se usa para los siguientes rangos de peso.

Se devuelven listas para agrupar el nivel y la información sobre el agua.

### 3. XV años

Este algoritmo verifica la edad de una persona para determinar si puede entrar a la fiesta de 15 años y qué requisitos debe cumplir.

```lisp
(defun verificar-entrada-fiesta (edad)
  (cond
    ((> edad 15)
     "Entra, pero debe traer un regalo.")
    ((= edad 15)
     "Entra, gratis.")
    ((< edad 15)
     "No puede entrar a la fiesta.")
    (t
     "Edad no válida.")))
```
Las condiciones son mutuamente excluyentes y se evalúan de forma secuencial.

La función cond permite una lógica clara y fácil de seguir.

Las funciones devuelven cadenas de texto con el requisito o la instrucción de entrada.

