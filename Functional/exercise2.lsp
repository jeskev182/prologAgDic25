(defun division-recursiva (dividendo divisor)
  (if (< dividendo divisor)
      0
      (+ 1 (division-recursiva (- dividendo divisor) divisor))))



(defun potencia-recursiva (base exponente)
  (if (= exponente 0)
      1
      (* base (potencia-recursiva base (- exponente 1)))))



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