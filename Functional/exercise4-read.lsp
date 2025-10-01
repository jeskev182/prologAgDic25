;; Constante para PI
(defconstant +pi+ 3.1415926535d0)

;; --- Hash Tables para almacenar las funciones lambda ---
;; Creamos un hash table para las fórmulas de ÁREA.
;; Cada clave es un símbolo de figura (ej: :CIRCULO) y el valor es la función lambda.
(defvar *area-formulas* (make-hash-table))

;; Creamos un hash table para las fórmulas de VOLUMEN.
(defvar *volumen-formulas* (make-hash-table))

;; --- Definición de Fórmulas Lambda (10 Área, 10 Volumen) ---

;; Fórmulas de ÁREA
(setf (gethash :circulo *area-formulas*)
      ;; Lambda: requiere 'radio'
      (lambda (radio) (* +pi+ radio radio)))

(setf (gethash :cuadrado *area-formulas*)
      ;; Lambda: requiere 'lado'
      (lambda (lado) (* lado lado)))

(setf (gethash :triangulo *area-formulas*)
      ;; Lambda: requiere 'base' y 'altura'
      (lambda (base altura) (* 0.5 base altura)))

(setf (gethash :rectangulo *area-formulas*)
      ;; Lambda: requiere 'largo' y 'ancho'
      (lambda (largo ancho) (* largo ancho)))

(setf (gethash :trapecio *area-formulas*)
      ;; Lambda: requiere 'base1', 'base2' y 'altura'
      (lambda (base1 base2 altura) (* 0.5 (+ base1 base2) altura)))

(setf (gethash :rombo *area-formulas*)
      ;; Lambda: requiere 'diagonal1' y 'diagonal2'
      (lambda (d1 d2) (* 0.5 d1 d2)))

(setf (gethash :elipse *area-formulas*)
      ;; Lambda: requiere 'radio-mayor' y 'radio-menor'
      (lambda (a b) (* +pi+ a b)))

(setf (gethash :cilindro *area-formulas*)
      ;; Lambda: requiere 'radio' y 'altura' (Área Lateral)
      (lambda (radio altura) (* 2 +pi+ radio altura)))

(setf (gethash :esfera-area *area-formulas*)
      ;; Lambda: requiere 'radio' (Área Superficial)
      (lambda (radio) (* 4 +pi+ radio radio)))

(setf (gethash :hexagono *area-formulas*)
      ;; Lambda: requiere 'apotema' y 'perimetro'
      (lambda (apotema perimetro) (* 0.5 apotema perimetro)))

;; ------------------------------------------------------------------
;; Fórmulas de VOLUMEN
(setf (gethash :cubo *volumen-formulas*)
      ;; Lambda: requiere 'lado'
      (lambda (lado) (* lado lado lado)))

(setf (gethash :esfera *volumen-formulas*)
      ;; Lambda: requiere 'radio'
      (lambda (radio) (* (/ 4 3.0) +pi+ radio radio radio)))

(setf (gethash :cilindro-volumen *volumen-formulas*)
      ;; Lambda: requiere 'radio' y 'altura'
      (lambda (radio altura) (* +pi+ radio radio altura)))

(setf (gethash :cono *volumen-formulas*)
      ;; Lambda: requiere 'radio' y 'altura'
      (lambda (radio altura) (* (/ 1 3.0) +pi+ radio radio altura)))

(setf (gethash :piramide *volumen-formulas*)
      ;; Lambda: requiere 'area-base' y 'altura'
      (lambda (area-base altura) (* (/ 1 3.0) area-base altura)))

(setf (gethash :prisma *volumen-formulas*)
      ;; Lambda: requiere 'area-base' y 'altura'
      (lambda (area-base altura) (* area-base altura)))

(setf (gethash :toroide *volumen-formulas*)
      ;; Lambda: requiere 'radio-mayor' y 'radio-menor'
      (lambda (r-mayor r-menor) (* 2 +pi+ +pi+ r-mayor r-menor r-menor)))

(setf (gethash :elipsoide *volumen-formulas*)
      ;; Lambda: requiere 'a', 'b', 'c' (semiejes)
      (lambda (a b c) (* (/ 4 3.0) +pi+ a b c)))

(setf (gethash :tetraedro *volumen-formulas*)
      ;; Lambda: requiere 'lado' (Tetraedro regular)
      (lambda (lado) (* (/ (sqrt 2.0) 12.0) lado lado lado)))

(setf (gethash :paralelepipedo *volumen-formulas*)
      ;; Lambda: requiere 'largo', 'ancho', 'alto'
      (lambda (largo ancho alto) (* largo ancho alto)))

;; ------------------------------------------------------------------

;; Función para solicitar al usuario los parámetros necesarios
(defun pedir-parametros (params)
  "Solicita al usuario la entrada para cada parámetro en la lista 'params'."
  (loop for param in params
        collect (progn
                  (princ (format nil "Ingresa el valor para ~A: " (string-upcase param)))
                  (finish-output)
                  (read)))) ; Usamos READ para obtener un número/valor

;; Función principal
(defun calcular-geometria ()
  (princ "--- CALCULADORA DE ÁREA Y VOLUMEN (CON LAMBDAS) ---")
  (terpri)

  ;; 1. Obtener Tipo de Cálculo
  (princ "Selecciona el tipo de cálculo (AREA o VOLUMEN): ")
  (finish-output)
  (let* ((tipo-str (string-upcase (read-line)))
         (tipo (intern tipo-str :keyword))
         (formulas (case tipo
                     (:area *area-formulas*)
                     (:volumen *volumen-formulas*)
                     (t (progn (format t "Error: Tipo de cálculo inválido.~%") nil)))))

    (when formulas
      ;; 2. Mostrar Figuras Disponibles
      (format t "~%Figuras disponibles: ~{~A~^, ~}~%" 
              (mapcar #'string-downcase (mapcar #'symbol-name (hash-table-keys formulas))))
      
      ;; 3. Obtener Figura
      (princ "Ingresa la figura a calcular: ")
      (finish-output)
      (let* ((figura-str (string-upcase (read-line)))
             (figura (intern figura-str :keyword))
             (lambda-func (gethash figura formulas)))

        (if lambda-func
            ;; 4. Ejecutar el Cálculo
            (let* ((required-params (lambda-list-arguments lambda-func)) ; Obtiene nombres de argumentos
                   (valores (pedir-parametros required-params))        ; Obtiene valores del usuario
                   (resultado (apply lambda-func valores)))           ; Llama la lambda con los valores

              (format t "~%El resultado de ~A de ~A es: ~A~%"
                      (string-upcase tipo-str)
                      (string-upcase figura-str)
                      resultado))

            ;; Figura no encontrada
            (format t "Error: Figura '~A' no encontrada para el cálculo de ~A.~%" figura-str tipo-str))))))

;; (lambda-list-arguments) es una función para obtener los argumentos de una función lambda
;; y puede requerir cargar un módulo adicional en algunos entornos Lisp (como SBCL o CCL).
;; Si obtienes un error, reemplaza las líneas que usan lambda-list-arguments con los
;; parámetros esperados en la sección de ejecución del cálculo, por ejemplo:
;; (let ((valores (list (read) (read))) (resultado (apply lambda-func valores))) ... )
;; (En Common Lisp estándar, la introspección de argumentos de lambda es compleja sin bibliotecas).

;; Para ejecutar en la consola de Lisp:
;; (calcular-geometria)