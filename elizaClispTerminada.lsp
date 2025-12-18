(in-package :cl-user)

;;; ===========================================================
;;; 1. BASES DE CONOCIMIENTO (DATOS COMPLETOS PROLOG)
;;; ===========================================================


;; Definimos las uniones y sus hijos una sola vez
(defparameter *familias*
  '(((eduardo edit) . (kevin uriel jairo))
    ((jairo nil) . (farid))
    ((paco toya) . (edit willy hugo victor fani martel magali))
    ((david_abuelo lupe_abuela) . (eduardo tona salvador))
    ((willy tita) . (maricruz karla boby))
    ((hugo maribel) . (maritza huguito sara toyi))
    ((victor araceli) . (getse gaby victorin))
    ((nan magali) . (kichi chucho chino))
    ((ricardo fani) . (jordan))
    ((martel lupe) . (nico))
    ((tono tona) . (cholda beto berenice karla_p david_p))
    ((salvador patricia) . (gaby_p jimena samantha))))

;; Atributos individuales
(defparameter *atributos*
  '(
    (kevin :es trabajador :rol hijo)
    (uriel :es trabajador :rol hijo)
    (jairo :es noble :rol hijo)
    (farid :es estudiante :rol nieto)
    (eduardo :es trabajador :rol padre)
    (edit :es noble :rol madre)

    ;; --- Familia Materna
    (paco :es trabajador :rol abuelo)
    (toya :es noble :rol abuela)
    (willy :es extranjero :rol tio)
    (hugo :es inteligente :rol tio)
    (victor :es trabajador :rol tio)
    (fani :es lejano :rol tia)
    (martel :es noble :rol tio)
    (magali :es noble :rol tia)
    (maricruz :es alegre :rol prima)
    (karla :es profesional :rol prima)
    (boby :es social :rol primo)
    (maritza :es inteligente :rol prima)
    (huguito :es estudioso :rol primo)
    (sara :es amable :rol prima)
    (toyi :es carismatica :rol prima)
    (getse :es reservada :rol prima)
    (gaby :es carismatica :rol prima)
    (victorin :es social :rol primo)
    (kichi :es extranjero :rol primo)
    (chucho :es joven :rol primo)
    (chino :es joven :rol primo)
    (jordan :es profesional :rol primo)
    (nico :es carismatico :rol primo)

    ;; --- Familia Paterna
    (david_abuelo :es lejano :rol abuelo)
    (lupe_abuela :es noble :rol abuela)
    (tona :es noble :rol tia)
    (salvador :es trabajador :rol tio)
    (cholda :es alegre :rol prima)
    (beto :es profesional :rol primo)
    (berenice :es amable :rol prima)
    (karla_p :es inteligente :rol prima)
    (david_p :es social :rol primo)
    (gaby_p :es carismatica :rol prima)
    (jimena :es inteligente :rol prima)
    (samantha :es trabajadora :rol prima)

    ;; --- The Big Bang Theory ---
    
    (sheldon :profesion fisico_teorico :pareja amy :amigo leonard :miedo germenes :hobby trenes)
    (leonard :profesion fisico_experimental :pareja penny :amigo sheldon :miedo lactosa :rasgo inseguro)
    (howard :profesion ingeniero :pareja bernadette :amigo raj :miedo a_su_mama :logro fue_al_espacio)
    (raj :profesion astrofisico :mejor_amigo howard :miedo hablar_con_mujeres :mascota cinnamon)
    (penny :profesion mesera :aspiracion actriz :pareja leonard :rasgo carismatica)
    (amy :profesion neurobiologa :pareja sheldon :amiga penny :instrumento arpa)
    (bernadette :profesion microbiologa :pareja howard :rasgo competitiva :miedo sheldon_enfadado)
    (stuart :profesion dueño_tienda_comics :rasgo solitario :miedo rechazo_social)

    ))


(defparameter *punct-chars* ".,;:()?!\"'")

;; Base (Actividades 1, 2, 9)
(defparameter *enfermedades*
  '((septicemia :sintomas (fiebre_alta_persistente escalofrios_intensos confusion_mental respiracion_acelerada taquicardia)
                :tratamiento "Hospitalizacion, antibioticos intravenosos y soporte vital urgente." :especialista intensivista)
    (gripe :sintomas (tos cansancio fiebre dolor_cabeza)
           :tratamiento "Reposo, hidratacion, paracetamol, aislamiento. Medicinas: contrex, jarabe." :especialista otorrino)
    (hepatitis :sintomas (nauseas diarrea ictericia)
               :tratamiento "Pastillas y manejo de soporte." :especialista endocrinologo)
    (anemia :sintomas (cansancio apatia nauseas)
            :tratamiento "Vitaminas y suplementos." :especialista nutricionista)
    (tuberculosis :sintomas (tos cansancio fiebre escalofrios)
                  :tratamiento "Terapia farmacologica prolongada." :especialista nutricionista)
    (malaria :sintomas (escalofrios fiebre diarrea ictericia)
             :tratamiento "Vacuna y medicacion antimalarica." :especialista medicogeneral)
    (rubeola :sintomas (fiebre jaqueca secrecion)
             :tratamiento "Vacuna y manejo de sintomas." :especialista medicogeneral)
    (candidiasis_oral :sintomas (placas_blancas ardor_bucal dolor_al_tragar sangrado_leve)
                      :tratamiento "Antifungicos (Nistatina, fluconazol)." :especialista otorrino)
    (cancer_de_prostata :sintomas (dificultad_para_orinar miccion_frecuente_nocturna flujo_de_orina_debil sangre_en_la_orina)
                    :tratamiento "Cirugia, radioterapia, quimioterapia o terapia hormonal." :especialista urologo)
    (colera :sintomas (diarrea_acuosa_repentina vomitos deshidratacion_severa)
            :tratamiento "Rehidratacion_oral, antibioticos (doxiciclina)." :especialista infectologo)
    (alergia :sintomas (estornudos picazon_ojos congestion)
             :tratamiento "Antihistaminicos y evitar el alergeno." :especialista alergologo)
    (migrana :sintomas (dolor_cabeza_severo sensibilidad_luz nauseas)
             :tratamiento "Medicacion especifica y ambiente oscuro." :especialista neurologo)
    (resfriado :sintomas (estornudos congestion dolor_garganta)
               :tratamiento "Liquidos calientes y vitamina C." :especialista otorrino)))

;; Contradicciones
(defparameter *contradicciones*
  '((fiebre . picazon_ojos)             
    (nauseas . estornudos)              
    (ictericia . dificultad_para_orinar)
    (escalofrios . picazon_ojos)        
    (placas_blancas . diarrea)          
    (sangre_en_la_orina . secrecion)    
    (ictericia . estornudos)            
    (confusion_mental . picazon_ojos)   
    (taquicardia . cansancio)))         



(defvar *sintomas-confirmados* nil)


;;; MOTOR 

(defun clean-and-tokenize (line)
  (let* ((lower (string-downcase line))
         (clean (map 'string (lambda (c) (if (search (string c) *punct-chars*) #\Space c)) lower)))
    (labels ((split (str) (loop for i = 0 then (1+ j) as j = (position #\Space str :start i) collect (subseq str i j) while j)))
      (remove "" (split clean) :test #'string=))))

(defun element-match-p (templ-el token)
  (cond ((null templ-el) t)
        ((and (consp templ-el) (eq (first templ-el) 's)) (not (null token)))
        ((symbolp templ-el) (and token (string-equal (symbol-name templ-el) token)))
        (t nil)))

(defun match-template (stim input)
  (labels ((rec (slist ilist)
             (cond ((null slist) t) ((null ilist) nil)
                   (t (if (element-match-p (first slist) (first ilist)) (rec (rest slist) (rest ilist)) nil)))))
    (rec stim input)))

(defun get-token-at (input pos) (if (and input (>= pos 0) (< pos (length input))) (nth pos input) ""))

;;; LÓGICA

(defvar *sintomas-negados* nil) ;; recordar los no

(defun preguntar (sintoma)
  (let ((s-sym (intern (string-upcase (format nil "~a" sintoma)))))
    (cond 
      ((member s-sym *sintomas-confirmados*) t)
      ((member s-sym *sintomas-negados*) nil)
      (t (format t "¿El paciente tiene ~a? (si/no): " sintoma) (finish-output)
         (let ((resp (read-line)))
           (cond 
             ((member resp '("si" "s" "y") :test #'string-equal)
              (pushnew s-sym *sintomas-confirmados*)
              ;; Act 6: Avisar si lo que acaba de decir choca con algo previo
              (let ((contra (checar-contradicciones)))
                (when contra
                  (format t "!!! ADVERTENCIA: Sintomas contradictorios detectados: ~a !!!~%" contra)))
              t)
             (t 
              (pushnew s-sym *sintomas-negados*) nil)))))))

(defun checar-contradicciones ()
  (let ((resultados nil))
    (loop for (s1 . s2) in *contradicciones* do
      (let ((sym1 (intern (string-upcase (symbol-name s1))))
            (sym2 (intern (string-upcase (symbol-name s2)))))
        (when (and (member sym1 *sintomas-confirmados*) (member sym2 *sintomas-confirmados*))
          (push (list sym1 sym2) resultados))))
    resultados))

(defun generar-reporte-completo (enf)
  (let* ((data (cdr (assoc enf *enfermedades*)))
         (s-total (if data (getf data :sintomas) '(fiebre))))
    (format t "~%Confirmando todos los detalles para ~a...~%" enf)
    (loop for s in s-total do (preguntar s))
    (let* ((conf (intersection *sintomas-confirmados* s-total))
           (num-total (max 1 (length s-total)))
           (prob (* (/ (length conf) (float num-total)) 100))
           (sev (cond ((>= (length conf) 4) "Severa") ((>= (length conf) 2) "Moderada") (t "Leve")))
           (riesgo (cond ((member 'CONFUSION_MENTAL *sintomas-confirmados*) "ALTO")
                         ((and (eq enf 'SEPTICEMIA) (string= sev "Severa")) "ALTO")
                         (t "BAJO")))
           (contra (checar-contradicciones)))
      (format t "~%===================================================~%")
      (format t "REPORTE MEDICO FINAL (ACTIVIDAD 12)~%")
      (format t "===================================================~%")
      (format t "Diagnostico Final: ~a~%Probabilidad: ~,1f%~%Severidad: ~a~%" enf prob sev)
      (format t "Riesgo: ~a | Especialista: ~a~%" riesgo (if data (getf data :especialista) "MEDICOGENERAL"))
      (format t "Tratamiento: ~a~%" (if data (getf data :tratamiento) "Observacion."))
      (when contra (format t "ADVERTENCIA (Act 6): Contradicciones detectadas: ~a~%" contra))
      (format t "===================================================~%")
      (list "Diagnostico" "finalizado."))))


;;  Diagnóstico por Síntoma Exclusivo
(defun diagnostico-exclusivo-p ()
  "Busca si entre los síntomas confirmados hay alguno que solo pertenezca a una enfermedad."
  (loop for s-conf in *sintomas-confirmados*
        do (let ((coincidencias (loop for enf in *enfermedades*
                                     when (member s-conf (getf (cdr enf) :sintomas))
                                     collect (car enf))))
             (when (= (length coincidencias) 1)
               (return (car coincidencias))))))

;; Tratamiento Combinado
(defun obtener-tratamiento-combinado ()
  "Recolección de todos los tratamientos de enfermedades con al menos un síntoma confirmado."
  (remove-duplicates
   (loop for enf in *enfermedades*
         when (intersection *sintomas-confirmados* (getf (cdr enf) :sintomas))
         collect (getf (cdr enf) :tratamiento))
   :test #'string=))


(defun generar-reporte-final (enf)
  (let* ((datos (cdr (assoc enf *enfermedades*)))
         (prob (calcular-probabilidad enf))
         (sev (determinar-severidad prob))
         (riesgo (determinar-riesgo enf sev))
         (similares (buscar-similares enf))
         (tratamientos-p (obtener-tratamiento-combinado))
         ;; Recomendación según severidad
         (recom (cond ((string= riesgo "ALTO") "URGENTE: Acuda a emergencias de inmediato.")
                      ((string= sev "Severa") "Consulte a un especialista hoy mismo.")
                      (t "Reposo absoluto y monitoreo de temperatura."))))
    (format t "~%===================================================~%")
    (format t "          REPORTE MEDICO MAESTRO (Act 12)          ~%")
    (format t "===================================================~%")
    (format t "SINTOMAS CONFIRMADOS: ~{~a~^, ~}~%" *sintomas-confirmados*)
    (format t "---------------------------------------------------~%")
    (format t "DIAGNOSTICO PRINCIPAL: ~a (~,1f%)~%" enf prob)
    (format t "SEVERIDAD: ~a | RIESGO: ~a~%" sev riesgo)
    (format t "ESPECIALISTA: ~a~%" (getf datos :especialista))
    (format t "---------------------------------------------------~%")
    (format t "TRATAMIENTO SUGERIDO: ~a~%" (getf datos :tratamiento))
    (format t "TRATAMIENTOS COMBINADOS: ~{~% - ~a~}~%" tratamientos-p)
    (format t "---------------------------------------------------~%")
    (format t "RECOMENDACION: ~a~%" recom)
    (when similares
      (format t "ENFERMEDADES SIMILARES: ~{~a~^, ~}~%" similares))
    (format t "===================================================~%")
    (list "Diagnostico" "concluido." "Reporte" "generado.")))

(defun arbol-diagnostico ()
  "Recorre las enfermedades y pregunta síntomas de forma dinámica basándose en la lista *enfermedades*."
  (format t "~%--- Iniciando Diagnóstico Experto Dinámico ---~%")
  (let ((enfermedad-final nil))
    (loop for enf in *enfermedades* do
      (let* ((nombre (car enf))
             (sintomas (getf (cdr enf) :sintomas))
             (confirmado t))
        (loop for s in sintomas do
          (unless (preguntar s) 
            (setf confirmado nil)
            (return)))
        (when confirmado
          (setf enfermedad-final nombre)
          (return))))
    enfermedad-final))


;;; TEMPLATES
(defun obtener-padres (hijo)
  "Busca en qué familia se encuentra el hijo y devuelve a la pareja de padres."
  (loop for familia in *familias*
        when (member hijo (cdr familia) :test #'string-equal)
        return (car familia)))

(defun obtener-pareja (persona)
  "Busca quién es la pareja de una persona basándose en las familias registradas."
  (let ((pareja (loop for familia in *familias*
                      when (member persona (car familia) :test #'string-equal)
                      return (remove persona (car familia) :test #'string-equal))))
    (if (car pareja) (car pareja) nil)))

(defun obtener-hijos (persona)
  "Busca todos los hijos de una persona, ya sea que esté como primer o segundo miembro de la pareja."
  (loop for familia in *familias*
        when (member persona (car familia) :test #'string-equal)
        append (cdr familia)))

(defun obtener-abuelos (hijo)
  "Inferencia dinámica: Encuentra los padres de los padres."
  (let ((padres (obtener-padres hijo)))
    (remove-duplicates 
     (loop for p in padres append (obtener-padres p)))))

(defun obtener-tios (persona)
  "Inferencia: Hermanos de mis padres (hijos de mis abuelos que no son mis padres)."
  (let* ((padres (obtener-padres persona))
         ;; los abuelitos
         (abuelos (remove-duplicates (loop for p in padres append (obtener-padres p))))
         ;; hijos de esos abuelos (padres + tíos)
         (hijos-de-abuelos (loop for a in abuelos append (obtener-hijos a))))
    ;; 3Mis tids
    (remove-duplicates 
     (remove-if (lambda (h) (member h padres)) hijos-de-abuelos))))

(defun obtener-primos (persona)
  "Inferencia: Hijos de los hermanos de mis padres."
  (let* ((padres (obtener-padres persona))
         (abuelos (loop for p in padres append (obtener-padres p)))
         (tios (loop for a in abuelos 
                     append (remove-if (lambda (h) (member h padres)) 
                                       (obtener-hijos a)))))
    ;; hijos de esos tios
    (remove-duplicates 
     (loop for tio in tios append (obtener-hijos tio)))))


(defun calcular-probabilidad (enf)
  (let* ((datos (cdr (assoc enf *enfermedades*)))
         (s-totales (getf datos :sintomas))
         (s-conf (intersection *sintomas-confirmados* s-totales))
         (num-conf (length s-conf))
         (num-tot (length s-totales)))
    (if (zerop num-tot) 0 (* (/ num-conf (float num-tot)) 100))))

(defun determinar-severidad (prob)
  (cond ((>= prob 80) "Severa")
        ((>= prob 40) "Moderada")
        (t "Leve")))

(defun determinar-riesgo (enf sev)
  (cond ((or (member 'CONFUSION_MENTAL *sintomas-confirmados*)
             (and (eq enf 'SEPTICEMIA) (string= sev "Severa"))) "ALTO")
        ((string= sev "Moderada") "MEDIO")
        (t "BAJO")))

(defun buscar-similares (enf-base)
  (let ((s-base (getf (cdr (assoc enf-base *enfermedades*)) :sintomas)))
    (loop for enf in *enfermedades*
          as nombre = (car enf)
          as s-enf = (getf (cdr enf) :sintomas)
          when (and (not (eq nombre enf-base))
                    (>= (length (intersection s-base s-enf)) 2))
          collect nombre)))

(defun buscar-tratamiento-por-sintoma (sintoma-user)
  (let ((enfermedad (loop for enf in *enfermedades*
                          when (member sintoma-user (getf (cdr enf) :sintomas))
                          return (cdr enf))))
    (if enfermedad
        (getf enfermedad :tratamiento)
        "No tengo un tratamiento registrado para ese sintoma especifico.")))

(defun handle-flag (flag indices input)
  (let* ((arg (and indices (get-token-at input (first indices))))
         (arg-sym (and arg (intern (string-upcase (format nil "~a" arg)))))
         (datos-attr (cdr (assoc arg-sym *atributos*))))
         ;; Logicas
    (case flag
      (flagFam
       (cond 
         ((member 'abuelo input :test #'string-equal)
          (let ((abuelos (obtener-abuelos arg-sym)))
            (if abuelos 
                (append (list "Los" "abuelos" "de" arg "son:") abuelos)
                (list "No" "encontre" "abuelos" "para" arg))))

         ((member 'tios input :test #'string-equal)
          (let ((tios (obtener-tios arg-sym)))
            (if tios 
                (append (list "Los" "tios" "de" arg "son:") tios)
                (list "No" "encontre" "tios" "para" arg))))

          ((member 'padre input :test #'string-equal)
          (let ((padres (obtener-padres arg-sym)))
            (if padres 
                (append (list "El" "padre" "de" arg "es") (list padres))
                (list "No" "tengo" "registrado" "al" "padre" "de" arg))))
         ((member 'primos input :test #'string-equal)
          (let ((primos (obtener-primos arg-sym)))
            (if primos 
                (append (list "Los" "primos" "de" arg "son:") primos)
                (list "Los" "primos" "de" arg "son" primos))))

         ((member 'pareja input :test #'string-equal)
          (let ((p (or (getf datos-attr :pareja) (obtener-pareja arg-sym))))
            (if p (list "La" "pareja" "de" arg "es" p) (list arg "no" "tiene" "pareja" "registrada"))))

          ((member 'dedica input :test #'string-equal)
          (let ((prof (getf datos-attr :profesion)))
            (if prof (list arg "se" "dedica" "a" "ser" prof) (list "No" "se" "su" "profesion"))))

          ((member 'miedo input :test #'string-equal)
          (let ((miedo (getf datos-attr :miedo)))
            (if miedo (list arg "le" "tiene" "miedo" "a" miedo) (list arg "es" "muy" "valiente"))))

          ((member 'amigo input :test #'string-equal)
          (let ((amigo (getf datos-attr :amigo)))
            (if amigo (list arg "tiene" "como" "mejor" "amigo" amigo) (list arg "no" "tiene" "mejor" "amigo"))))

         (datos-attr 
            (append (list (symbol-name arg-sym) "tiene" "estos" "datos:") 
                    (loop for (k v) on datos-attr by #'cddr 
                          collect (format nil "~a: ~a" k v))))

         ((obtener-padres arg-sym)
          (list arg "esta" "en" "el" "arbol" "genealogico," "pero" "no" "tengo" "sus" "atributos."))

         (t (list "No" "conozco" "a" arg))))
      
        (flagTengo 
       (let* ((s-sym (intern (string-upcase (format nil "~a" arg)))))
         (pushnew s-sym *sintomas-confirmados*)
         (let ((posibles (loop for enf in *enfermedades*
                               when (member s-sym (getf (cdr enf) :sintomas))
                               collect (car enf))))
           (if posibles
               (append (list "Anotado." arg "esta" "presente" "en:") posibles 
                       (list ". Dime" "que" "mas" "sientes" "para" "darte" "un" "diagnostico."))
               (list "Anotado," "aunque" "el" "sintoma" arg "no" "esta" "en" "mi" "base" "medica.")))))

      (flagDiagInteractivo 
       (let ((enf (or (arbol-diagnostico)           ;  Intenta arbol
                      (diagnostico-exclusivo-p)     ;  Intenta Exclusivo
                      ;; Busca la de mayor prob
                      (car (first (sort (loop for e in *enfermedades* collect (list (car e) (calcular-probabilidad (car e))))
                                        #'> :key #'second))))))
         (if (and enf (> (calcular-probabilidad enf) 0))
             (generar-reporte-final enf) ; Genera el reporte
             (list "No" "hay" "sintomas" "suficientes" "para" "un" "diagnostico."))))
      (flagMedicina
 (let ((medicamento (buscar-tratamiento-por-sintoma arg-sym)))
   (list "El" "tratamiento" "sugerido" "es:" medicamento)))
      (t (list "Explica" "un" "poco" "mas")))))

(defparameter *saludos*
  '("Hola, soy Eliza. ¿En qué puedo ayudarte?" "Saludos. Soy tu asistente médica virtual." "¡Hola! Cuéntame, ¿qué síntomas tienes hoy?" "Buen día. ¿Qué tema te gustaría consultar?" "Hola, mi nombre es Eliza. ¿Cómo te sientes?" "Bienvenido al consultorio virtual. Dime." "Hola. Estoy lista para escucharte." "Saludos cordiales. ¿En qué te asesoro?" "¿Qué tal? Soy Eliza, tu chatbot experto." "Hola. ¿Qué dudas tienes hoy?" "Buen día. ¿A quién buscamos en el árbol genealógico?" "Hola. Iniciando protocolo médico." "Saludos. ¿Cómo va tu salud hoy?" "Hola, soy Eliza. ¿Qué diagnóstico necesitas?" "¡Hola! Te escucho con atención." "Buen día. ¿Qué miembro de la familia consultamos?" "Hola. Soy tu interfaz experta en CLISP." "Hola de nuevo. ¿Qué hay de nuevo?" "Saludos. ¿Cómo puedo facilitar tu día hoy?" "Hola. Cuéntame tus dudas."))

(defparameter *despedidas*
  '("Adiós. ¡Cuídate mucho!" "Hasta pronto. Espero haberte ayudado." "Nos vemos. No olvides seguir el tratamiento." "Que tengas un excelente día." "Adiós. Vuelve si tienes más dudas." "Terminando sesión. ¡Saludos!" "Chao. Fue un gusto platicar." "Hasta luego. Cuida tu salud." "Adiós. Espero verte pronto." "Desconexión completa. ¡Cuídate!" "¡Nos vemos! Un placer asesorarte." "Que te vaya bien." "Adiós. Consulta a tu médico real si es necesario." "Hasta la próxima." "Chao. Eliza fuera." "Adiós. Fue un diálogo productivo." "Nos vemos. ¡Sigue adelante!" "Bye bye. ¡Un gusto!" "Hasta pronto. Estaré aquí si me necesitas." "Adiós. Finalizando diagnóstico."))

(defparameter *templates*
  (list 
   ;; --- INFERENCIA DE PARENTESCO  ---
   (list (list 'quien 'es 'el 'padre 'de (list 's)) '(flagFam) '(5))
   (list (list 'quien 'es 'el 'abuelo 'de (list 's)) '(flagFam) '(5))
   (list (list 'quienes 'son 'los 'tios 'de (list 's)) '(flagFam) '(5))
   (list (list 'quienes 'son 'los 'primos 'de (list 's)) '(flagFam) '(5))
   
   ;; --- CONSULTAS DE TBBT ---
   (list (list 'quien 'es 'la 'pareja 'de (list 's)) '(flagFam) '(5))
   (list (list 'a 'que 'se 'dedica (list 's)) '(flagFam) '(4))
   (list (list 'a 'que 'le 'tiene 'miedo (list 's)) '(flagFam) '(5))
   (list (list 'quien 'es 'el 'mejor 'amigo 'de (list 's)) '(flagFam) '(6))
   
   
   ;; --- CONSULTAS MÉDICAS Y DIAGNÓSTICO ---
   (list (list 'quiero 'un 'diagnostico) '(flagDiagInteractivo) nil)
   (list (list 'medicina 'para 'la (list 's)) '(flagMedicina) '(3))
   (list (list 'medicina 'para 'el (list 's)) '(flagMedicina) '(3))
   (list (list 'tengo (list 's)) '(flagTengo) '(1))

   ;; --- SALUDOS Y FALLBACK ---
   (list (list 'quien 'es (list 's)) '(flagFam) '(2))
   (list (list 'hola) '("Hola, soy Eliza Médica Experta.") nil)
   (list nil '("Continua, te escucho.") nil)))

(defun respond-to (input)
  (let* ((tpl (find-if (lambda (tl) (if (null (car tl)) t (match-template (car tl) input))) *templates*))
         (resp (second tpl)) (inds (third tpl)))
    (if (and (symbolp (car resp)) (search "FLAG" (symbol-name (car resp))))
        (format t "~{~a~^ ~}~%" (handle-flag (car resp) inds input))
        (format t "~{~a~^ ~}~%" (mapcan (lambda (e) (cond ((integerp e) (list (get-token-at input (nth e inds))))
                                                       ((symbolp e) (list (symbol-name e))) (t (list e)))) resp)))))

(defun eliza-loop ()
  (setf *sintomas-confirmados* nil)
  (setf *sintomas-negados* nil) ;; Limpiar memoria de "no"
  ;; Selecciona saludo al azar
  (format t "~a~%" (nth (random (length *saludos*)) *saludos*))
  (loop (format t "~%> ") (finish-output)
     (let* ((line (read-line *query-io* nil nil)))
       (when (or (null line) (string-equal line "adios"))
         ;; Selecciona despedida al azar
         (return (format t "~a~%" (nth (random (length *despedidas*)) *despedidas*))))
       (let ((tokens (clean-and-tokenize line))) 
         (if tokens (respond-to tokens) (format t "Dime algo...~%"))))))