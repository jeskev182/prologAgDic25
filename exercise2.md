# División con Restas

La división de un número `a` entre un número `b` se puede calcular restando `b` de `a` repetidamente hasta que `a` sea menor que
 `b`. El número de restas es el resultado de la división, y el valor final de `a` es el residuo.

---

### Solución
Cada llamada a la función se encarga de una sola resta. 
El **caso base** es cuando el dividendo (`a`) es menor que el divisor (`b`).

```lisp
(defun division-recursiva (dividendo divisor)
  (if (< dividendo divisor)
      0
      (+ 1 (division-recursiva (- dividendo divisor) divisor))))
```

Caso Base: (if (< dividendo divisor) 0 ...): Si el dividendo es menor que el divisor, ya no podemos restar más, por lo que el resultado es 0.

Paso Recursivo: (+ 1 (division-recursiva (- dividendo divisor) divisor)): Si el dividendo es mayor o igual, sumamos 1 al resultado de la llamada recursiva, que resta el divisor del dividendo.

# Potencia con multiplicaciones

La potencia de un número b elevado x un exponente e (b^x) se puede calcular multiplicando b por sí mismo x veces.

---

### Solución
```lisp
(defun potencia-recursiva (base exponente)
  (if (= exponente 0)
      1
      (* base (potencia-recursiva base (- exponente 1)))))
```

Caso Base: (if (= exponente 0) 1 ...): Cualquier número elevado a la potencia 0 es 1. Esta es la condición de salida.

Paso Recursivo: (* base (potencia-recursiva base (- exponente 1))): Multiplica la base por el resultado de la llamada recursiva con un exponente menor.

# Descomposiciones

La función principal, particiones, llama a una función auxiliar (descomponer) que realiza el trabajo recursivo.

La función descomponer toma tres argumentos:
El número n que se quiere descomponer.
El número más grande que se puede usar en la suma (max-num). Esto evita repeticiones y asegura que las sumas se generen en orden.
Una lista acumulador que guarda los números que ya se han elegido para la suma.
El caso base de la recursión se da cuando n es 0. Esto significa que hemos encontrado una combinación de números que suman exactamente el número original. En este punto, imprimimos la lista del acumulador que contiene nuestra solución.
El paso recursivo intenta descomponer n con números más pequeños. En cada llamada, se prueba con un número i (desde max-num hasta 1), se resta i de n y se agrega i al acumulador. Luego, se llama recursivamente a descomponer con los nuevos valores.

---

### Solución
```lisp
(defun particiones (n)
  (if (<= n 0)
      (format t "Por favor, introduce un número natural mayor que cero.~%")
      (descomponer n n '())))

(defun descomponer (n max-num acumulador)
  (if (= n 0)
      (imprimir-particion (reverse acumulador))
      
      (loop for i from (min n max-num) downto 1
            do (descomponer (- n i) i (cons i acumulador)))))

(defun imprimir-particion (lista)
  (loop for num in lista
        for first = t then nil
        do (unless first (princ "+"))
        do (princ num))
  (format t "~%"))
```

Caso Base: (if (= exponente 0) 1 ...): Cualquier número elevado a la potencia 0 es 1. Esta es la condición de salida.

Paso Recursivo: (* base (potencia-recursiva base (- exponente 1))): Multiplica la base por el resultado de la llamada recursiva con un exponente menor.