(defun calcular-sueldo (anios-en-empresa)
  (let ((sueldo-base 40000))
    (cond
      ((> anios-en-empresa 10)
       (* sueldo-base 1.10))
      ((> anios-en-empresa 5)
       (* sueldo-base 1.07))
      ((> anios-en-empresa 3)
       (* sueldo-base 1.05))
      (t
       (* sueldo-base 1.03)))))

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