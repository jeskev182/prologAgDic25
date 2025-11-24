## 📄 Documentación del Programa de Geometría en Common Lisp

Este programa implementa una **Calculadora de Área y Volumen** que utiliza **funciones lambda** almacenadas en **Hash Tables** para ejecutar cálculos geométricos de diversas figuras. La estructura facilita la adición o modificación de fórmulas sin alterar la lógica principal del programa.

---

### ⚙️ Componentes Principales

#### 1. Constantes y Variables Globales

* **`+pi+`**:
    * **Tipo**: `defconstant`
    * **Valor**: `3.1415926535d0` (Número de punto flotante de doble precisión).
    * **Propósito**: Define el valor de la constante matemática **$\pi$** para su uso en las fórmulas.
* **`*area-formulas*`**:
    * **Tipo**: `defvar` (Hash Table)
    * **Propósito**: Almacena un conjunto de **funciones lambda** donde la **clave** es el **símbolo de la figura** (ej: `:CIRCULO`) y el **valor** es la función que calcula su área.
* **`*volumen-formulas*`**:
    * **Tipo**: `defvar` (Hash Table)
    * **Propósito**: Almacena un conjunto de **funciones lambda** donde la **clave** es el **símbolo de la figura** (ej: `:CUBO`) y el **valor** es la función que calcula su volumen.

---

### 🧮 Fórmulas Almacenadas (Funciones Lambda)

Las fórmulas están definidas como funciones lambda anónimas y se mapean directamente a sus respectivas Hash Tables.

#### A. Fórmulas de Área (`*area-formulas*`)

| Clave (Figura) | Parámetros Requeridos | Descripción de la Fórmula |
| :---: | :---: | :--- |
| `:CIRCULO` | `radio` | $A = \pi \cdot r^2$ |
| `:CUADRADO` | `lado` | $A = l^2$ |
| `:TRIANGULO` | `base`, `altura` | $A = 0.5 \cdot b \cdot h$ |
| `:RECTANGULO` | `largo`, `ancho` | $A = l \cdot a$ |
| `:TRAPECIO` | `base1`, `base2`, `altura` | $A = 0.5 \cdot (b_1 + b_2) \cdot h$ |
| `:ROMBO` | `d1`, `d2` | $A = 0.5 \cdot d_1 \cdot d_2$ |
| `:ELIPSE` | `a`, `b` (radio-mayor, radio-menor) | $A = \pi \cdot a \cdot b$ |
| `:CILINDRO` | `radio`, `altura` | $A_{\text{lateral}} = 2 \cdot \pi \cdot r \cdot h$ |
| `:ESFERA-AREA` | `radio` | $A_{\text{superficial}} = 4 \cdot \pi \cdot r^2$ |
| `:HEXAGONO` | `apotema`, `perimetro` | $A = 0.5 \cdot a \cdot p$ |

#### B. Fórmulas de Volumen (`*volumen-formulas*`)

| Clave (Figura) | Parámetros Requeridos | Descripción de la Fórmula |
| :---: | :---: | :--- |
| `:CUBO` | `lado` | $V = l^3$ |
| `:ESFERA` | `radio` | $V = \frac{4}{3} \cdot \pi \cdot r^3$ |
| `:CILINDRO-VOLUMEN` | `radio`, `altura` | $V = \pi \cdot r^2 \cdot h$ |
| `:CONO` | `radio`, `altura` | $V = \frac{1}{3} \cdot \pi \cdot r^2 \cdot h$ |
| `:PIRAMIDE` | `area-base`, `altura` | $V = \frac{1}{3} \cdot A_{\text{base}} \cdot h$ |
| `:PRISMA` | `area-base`, `altura` | $V = A_{\text{base}} \cdot h$ |
| `:TOROIDE` | `r-mayor`, `r-menor` | $V = 2 \cdot \pi^2 \cdot R \cdot r^2$ |
| `:ELIPSOIDE` | `a`, `b`, `c` (semiejes) | $V = \frac{4}{3} \cdot \pi \cdot a \cdot b \cdot c$ |
| `:TETRAEDRO` | `lado` | $V = \frac{\sqrt{2}}{12} \cdot l^3$ (Regular) |
| `:PARALELEPIPEDO`| `largo`, `ancho`, `alto` | $V = l \cdot a \cdot h$ |

---

### 💻 Funciones del Sistema

#### 1. `(pedir-parametros params)`

* **Propósito**: Solicita de forma interactiva al usuario los valores para cada parámetro listado en `params`.
* **Argumento**: `params` (Lista de símbolos o cadenas con los nombres de los parámetros requeridos).
* **Retorno**: Una **lista** de los valores numéricos ingresados por el usuario, en el orden solicitado.

#### 2. `(calcular-geometria)`

* **Propósito**: **Función principal** que orquesta todo el proceso de cálculo.
* **Flujo de Ejecución**:
    1.  **Solicita** el tipo de cálculo (`AREA` o `VOLUMEN`).
    2.  **Selecciona** la tabla de fórmulas según el tipo.
    3.  **Muestra** las figuras disponibles.
    4.  **Solicita** la figura específica.
    5.  **Recupera** la función lambda asociada.
    6.  **Introspección**: Obtiene la lista de nombres de argumentos (vía `lambda-list-arguments`).
    7.  **Obtiene Valores**: Llama a `pedir-parametros`.
    8.  **Ejecuta Cálculo**: Utiliza **`APPLY`** para invocar la función lambda con los valores.
    9.  **Muestra Resultado**.

* **Nota de Implementación**: La función depende de **`lambda-list-arguments`** para la introspección de la lista de argumentos, una característica que podría requerir librerías adicionales en ciertos entornos Lisp.

Para iniciar la calculadora, ejecute `(calcular-geometria)` en su entorno Common Lisp.


```lisp
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
      (lambda (radio) (* +pi+ radio radio)))

(setf (gethash :cuadrado *area-formulas*)
      (lambda (lado) (* lado lado)))

(setf (gethash :triangulo *area-formulas*)
      (lambda (base altura) (* 0.5 base altura)))

(setf (gethash :rectangulo *area-formulas*)
      (lambda (largo ancho) (* largo ancho)))

(setf (gethash :trapecio *area-formulas*)
      (lambda (base1 base2 altura) (* 0.5 (+ base1 base2) altura)))

(setf (gethash :rombo *area-formulas*)
      (lambda (d1 d2) (* 0.5 d1 d2)))

(setf (gethash :elipse *area-formulas*)
      (lambda (a b) (* +pi+ a b)))

(setf (gethash :cilindro *area-formulas*)
      (lambda (radio altura) (* 2 +pi+ radio altura)))

(setf (gethash :esfera-area *area-formulas*)
      (lambda (radio) (* 4 +pi+ radio radio)))

(setf (gethash :hexagono *area-formulas*)
      (lambda (apotema perimetro) (* 0.5 apotema perimetro)))

;; Fórmulas de VOLUMEN
(setf (gethash :cubo *volumen-formulas*)
      (lambda (lado) (* lado lado lado)))

(setf (gethash :esfera *volumen-formulas*)
      (lambda (radio) (* (/ 4 3.0) +pi+ radio radio radio)))

(setf (gethash :cilindro-volumen *volumen-formulas*)
      (lambda (radio altura) (* +pi+ radio radio altura)))

(setf (gethash :cono *volumen-formulas*)
      (lambda (radio altura) (* (/ 1 3.0) +pi+ radio radio altura)))

(setf (gethash :piramide *volumen-formulas*)
      (lambda (area-base altura) (* (/ 1 3.0) area-base altura)))

(setf (gethash :prisma *volumen-formulas*)
      (lambda (area-base altura) (* area-base altura)))

(setf (gethash :toroide *volumen-formulas*)
      (lambda (r-mayor r-menor) (* 2 +pi+ +pi+ r-mayor r-menor r-menor)))

(setf (gethash :elipsoide *volumen-formulas*)
      (lambda (a b c) (* (/ 4 3.0) +pi+ a b c)))

(setf (gethash :tetraedro *volumen-formulas*)
      (lambda (lado) (* (/ (sqrt 2.0) 12.0) lado lado lado)))

(setf (gethash :paralelepipedo *volumen-formulas*)
      (lambda (largo ancho alto) (* largo ancho alto)))


(defun pedir-parametros (params)
  "Solicita al usuario la entrada para cada parámetro en la lista 'params'."
  (loop for param in params
        collect (progn
                  (princ (format nil "Ingresa el valor para ~A: " (string-upcase param)))
                  (finish-output)
                  (read))))

(defun calcular-geometria ()
  (princ "--- CALCULADORA DE ÁREA Y VOLUMEN (CON LAMBDAS) ---")
  (terpri)

  (princ "Selecciona el tipo de cálculo (AREA o VOLUMEN): ")
  (finish-output)
  (let* ((tipo-str (string-upcase (read-line)))
         (tipo (intern tipo-str :keyword))
         (formulas (case tipo
                     (:area *area-formulas*)
                     (:volumen *volumen-formulas*)
                     (t (progn (format t "Error: Tipo de cálculo inválido.~%") nil)))))

    (when formulas
      (format t "~%Figuras disponibles: ~{~A~^, ~}~%" 
              (mapcar #'string-downcase (mapcar #'symbol-name (hash-table-keys formulas))))
      
      (princ "Ingresa la figura a calcular: ")
      (finish-output)
      (let* ((figura-str (string-upcase (read-line)))
             (figura (intern figura-str :keyword))
             (lambda-func (gethash figura formulas)))

        (if lambda-func
            (let* ((required-params (lambda-list-arguments lambda-func))
                   (valores (pedir-parametros required-params))        
                   (resultado (apply lambda-func valores)))     

              (format t "~%El resultado de ~A de ~A es: ~A~%"
                      (string-upcase tipo-str)
                      (string-upcase figura-str)
                      resultado))

            (format t "Error: Figura '~A' no encontrada para el cálculo de ~A.~%" figura-str tipo-str))))))
```