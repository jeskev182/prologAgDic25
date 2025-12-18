% ==========================================================%
% PREDICADOS AUXILIARES %
% ==========================================================%

% Auxiliar: Convierte un átomo (frase) a una lista de palabras (átomos) para la respuesta de Eliza.
atom_to_list_of_words(Atom, ListOfWords) :-
    % Usamos un mecanismo comun para dividir el atomo en palabras
    atom_string(Atom, String),
    split_string(String, " ", " ", StringList),
    maplist(atom_string, ListOfWords, StringList).

% ==========================================================%
% HECHOS: BASE DE CONOCIMIENTO MEDICA %
% ==========================================================%

%% Nueva Enfermedad Grave: Septicemia
tiene_sintoma(septicemia, fiebre_alta_persistente).
tiene_sintoma(septicemia, escalofrios_intensos).
tiene_sintoma(septicemia, confusion_mental).
tiene_sintoma(septicemia, respiracion_acelerada).
tiene_sintoma(septicemia, taquicardia).

tratamiento(septicemia, 'Hospitalizacion, antibioticos intravenosos de amplio espectro y soporte vital urgente.').
especialista(septicemia, intensivista).

%% 1. Enfermedades y Sintomas
tiene_sintoma(gripe, tos).
tiene_sintoma(gripe, cansancio).
tiene_sintoma(gripe, fiebre).
tiene_sintoma(gripe, dolor_cabeza).

tiene_sintoma(hepatitis, nauseas).
tiene_sintoma(hepatitis, diarrea).
tiene_sintoma(hepatitis, ictericia).

tiene_sintoma(anemia, cansancio).
tiene_sintoma(anemia, apatia).
tiene_sintoma(anemia, nauseas).

tiene_sintoma(tuberculosis, tos).
tiene_sintoma(tuberculosis, cansancio).
tiene_sintoma(tuberculosis, fiebre).
tiene_sintoma(tuberculosis, escalofrios).

tiene_sintoma(malaria, escalofrios).
tiene_sintoma(malaria, fiebre).
tiene_sintoma(malaria, diarrea).
tiene_sintoma(malaria, ictericia).

tiene_sintoma(rubeola, fiebre).
tiene_sintoma(rubeola, jaqueca).
tiene_sintoma(rubeola, secrecion).

tiene_sintoma(candidiasis_oral, placas_blancas).
tiene_sintoma(candidiasis_oral, ardor_bucal).
tiene_sintoma(candidiasis_oral, dolor_al_tragar).
tiene_sintoma(candidiasis_oral, sangrado_leve).
tiene_sintoma(candidiasis_oral, agrietamiento_comisuras_boca).

tiene_sintoma(cancer_de_prostata, dificultad_para_orinar).
tiene_sintoma(cancer_de_prostata, miccion_frecuente_nocturna).
tiene_sintoma(cancer_de_prostata, flujo_de_orina_debil).
tiene_sintoma(cancer_de_prostata, sangre_en_la_orina).
tiene_sintoma(cancer_de_prostata, dolor_huesos).

tiene_sintoma(colera, diarrea_acuosa_repentina).
tiene_sintoma(colera, vomitos).
tiene_sintoma(colera, deshidratacion_severa).
tiene_sintoma(colera, sed_extrema).
tiene_sintoma(colera, calambres_musculares).

tiene_sintoma(alergia, estornudos).
tiene_sintoma(alergia, picazon_ojos).
tiene_sintoma(alergia, congestion).

tiene_sintoma(migrana, dolor_cabeza_severo).
tiene_sintoma(migrana, sensibilidad_luz).
tiene_sintoma(migrana, nauseas).

tiene_sintoma(resfriado, estornudos).
tiene_sintoma(resfriado, congestion).
tiene_sintoma(resfriado, dolor_garganta).


%%  Tratamientos/Medicamentos (tratamiento/2)
tratamiento(gripe, 'Reposo, hidratacion, paracetamol, aislamiento. Medicinas: contrex, jarabe.').
tratamiento(rubeola, 'Vacuna y manejo de sintomas.').
tratamiento(malaria, 'Vacuna y medicacion antimalarica.').
tratamiento(hepatitis, 'Pastillas y manejo de soporte.').
tratamiento(tuberculosis, 'Pastillas (terapia farmacologica prolongada).').
tratamiento(anemia, 'Vitaminas y suplementos.').
tratamiento(candidiasis_oral, 'Nistatina, fluconazol o clotrimazol.').
tratamiento(cancer_de_prostata, 'Cirugia, radioterapia, terapia_hormonal o quimioterapia.').
tratamiento(colera, 'Rehidratacion_oral, antibioticos (doxiciclina).').
tratamiento(alergia, 'Antihistaminicos y evitar el alergeno conocido.').
tratamiento(migrana, 'Medicacion especifica, ambiente oscuro y tranquilo.').
tratamiento(resfriado, 'Liquidos calientes, descongestionantes y vitamina C.').


% ==========================================================%
% HECHOS %
% ==========================================================%

%% 4. Sintomas Contradictorios (contradictorio/2)
contradictorio(fiebre, picazon_ojos). % Inconsistencia entre infeccion/gripe (fiebre) y alergia (picazon)
contradictorio(nauseas, estornudos). % Inconsistencia comun entre problemas digestivos/migrana (nauseas) y resfriado/alergia (estornudos)
contradictorio(ictericia, dificultad_para_orinar). % Inconsistencia entre ictericia (problemas hepaticos/biliares) y problemas prostaticos/urinarios.


%% 3. Especialistas (especialista/2)
especialista(gripe, otorrino).
especialista(anemia, nutricionista).
especialista(hepatitis, endocrinologo).
especialista(rubeola, medicogeneral).
especialista(tuberculosis, nutricionista).
especialista(malaria, medicogeneral).
especialista(candidiasis_oral, otorrino).
especialista(cancer_de_prostata, urologo).
especialista(colera, infectologo).
especialista(alergia, alergologo).
especialista(migrana, neurologo).
especialista(resfriado, otorrino).


% ==========================================================%
%   INTERACCION %
% ==========================================================%
:- dynamic sintoma/2.

enfermedad(E) :- tiene_sintoma(E, _).
sintomade(S, E) :- tiene_sintoma(E, S).

reset_paciente(P) :- retractall(sintoma(P,_)).

% Sistema de interaccion
pregunta(Paciente, Sintoma) :-
    sintoma(Paciente, Sintoma), !.

pregunta(Paciente, Sintoma) :-
    format('¿El paciente ~w tiene ~w? (si/no): ', [Paciente, Sintoma]),
    readln(Input),
    ((Input == [si] ; Input == [s]) ->
        assertz(sintoma(Paciente, Sintoma))
    ;
        fail
    ).

% ==========================================================%
% DIAGN INTERACTIVO %
% ==========================================================%

% varifica si un sintoma es exclusivo de una enfermedad.
sintoma_exclusivo(Sintoma) :-
    % 1. Encontrar la enfermedad 'E' que tiene este 'Sintoma'
    tiene_sintoma(E, Sintoma),
    % 2. Verificar que NO exista OTRA enfermedad 'E2' que tenga el mismo 'Sintoma'
    \+ (
        tiene_sintoma(E2, Sintoma),
        E \== E2 % Aseguramos que E y E2 sean diferentes
    ).

%  Diagnostico por Sintoma Exclusivo
diagnostico_exclusivo(Paciente, Enfermedad) :-
    % 1. Encontrar un sintoma 'S' que sea exclusivo
    sintoma_exclusivo(S),
    % 2. Preguntar y confirmar que el paciente tiene ese sintoma
    pregunta(Paciente, S),
    % 3. Verificar que NO haya confirmado otro sintoma
    \+ (
        sintoma(Paciente, S2),
        S \== S2
    ),
    % 4. Determinar la enfermedad asociada al sintoma exclusivo
    tiene_sintoma(Enfermedad, S).


diagnostico_basico(Paciente, Enfermedad) :-
    tiene_sintoma(Enfermedad, S),
    pregunta(Paciente, S).

% Arbol de Decision Medico
arbol_diagnostico(P, Enfermedad) :-
    % RAMA 1: Sintomas Sistemico (Fiebre/Infeccion)
    pregunta(P, fiebre), !,
    (
        pregunta(P, confusion_mental), !, Enfermedad = septicemia % Alta prioridad por sintoma grave
    ;
        pregunta(P, tos), pregunta(P, cansancio), !, Enfermedad = gripe % Tipico de gripe/resfriado fuerte
    ;
        pregunta(P, escalofrios), pregunta(P, diarrea), !, Enfermedad = malaria % Sintomas de malaria
    ;
        pregunta(P, ictericia), pregunta(P, nauseas), !, Enfermedad = hepatitis % Sintomas hepaticos
    ;

        pregunta(P, confusion_mental) -> Enfermedad = septicemia 
    ;
        (pregunta(P, tos), pregunta(P, cansancio)) -> Enfermedad = gripe 
    ;
        (pregunta(P, escalofrios), pregunta(P, diarrea)) -> Enfermedad = malaria 
    ;
        (pregunta(P, ictericia), pregunta(P, nauseas)) -> Enfermedad = hepatitis 
    ;
        Enfermedad = 'posible_infeccion_desconocida' % Rama por defecto
    ).

% RAMA 2
arbol_diagnostico(P, Enfermedad) :-
    pregunta(P, dolor_cabeza_severo), !,
    (
        pregunta(P, sensibilidad_luz), !, Enfermedad = migrana
    ;
        Enfermedad = 'necesita_evaluacion_neurologica'
    ).

% RAMA 3
arbol_diagnostico(P, Enfermedad) :-
    pregunta(P, estornudos), !,
    (
        pregunta(P, picazon_ojos), !, Enfermedad = alergia
    ;
        pregunta(P, dolor_garganta), !, Enfermedad = resfriado
    ;
        Enfermedad = 'posible_alergia_o_resfriado_desconocido'
    ).

% RAMA 4
arbol_diagnostico(P, Enfermedad) :-
    pregunta(P, dificultad_para_orinar), !, Enfermedad = cancer_de_prostata.

arbol_diagnostico(P, Enfermedad) :-
    pregunta(P, placas_blancas), !, Enfermedad = candidiasis_oral.

arbol_diagnostico(P, Enfermedad) :-
    pregunta(P, diarrea_acuosa_repentina), !, Enfermedad = colera.

% RAMA FINAL: No hay diagnostico claro del arbol
arbol_diagnostico(_, _) :- fail.

% Diagnostico Preventivo
diagnostico_preventivo(Paciente, Enfermedad) :-
    % 1. La enfermedad debe tener al menos un sintoma confirmado (usa la regla de probabilidad)
    probabilidad(Paciente, Enfermedad, Porcentaje),
    Porcentaje > 0,
    
    % 2. La enfermedad NO debe estar completamente confirmada (Porcentaje < 100)
    Porcentaje < 100,

    tiene_sintoma(Enfermedad, _).

% Coincidencia con TODOS los sintomas
diagnostico_completo(Paciente, Enfermedad) :-
    findall(S, tiene_sintoma(Enfermedad, S), Lista),
    todos_confirmados(Paciente, Lista).

todos_confirmados(_, []).

todos_confirmados(Paciente, [S|R]) :-
    pregunta(Paciente, S),
    todos_confirmados(Paciente, R).

% Distincion Fuerte 
distincion_fuerte(P, gripe) :-
    diagnostico_basico(P, gripe),
    pregunta(P, fiebre),
    \+ pregunta(P, estornudos).

distincion_fuerte(P, resfriado) :-
    diagnostico_basico(P, resfriado),
    pregunta(P, estornudos),
    \+ pregunta(P, fiebre).

% Severidad
contar_sintomas_confirmados(P, Enfermedad, C) :-
    findall(S, (tiene_sintoma(Enfermedad,S), sintoma(P,S)), L),
    length(L, C).

severidad(P, E, 'Severa') :-
    contar_sintomas_confirmados(P, E, C), C >= 3, !.

severidad(P, E, 'Moderada') :-
    contar_sintomas_confirmados(P, E, C), C = 2, !.

severidad(P, E, 'Leve') :-
    contar_sintomas_confirmados(P, E, C), C = 1, !.


% Diagnostico de Riesgo

riesgo(P, _, alto) :-
    % Condicion A: Sintoma de gravedad extrema (confusion_mental)
    sintoma(P, confusion_mental), !.
riesgo(P, septicemia, alto) :-
    % Condicion B: La enfermedad grave es Severa
    severidad(P, septicemia, 'Severa'), !.

% RIESGO MEDIO: Si la severidad es Moderada
riesgo(P, E, medio) :-
    severidad(P, E, 'Moderada'), !.

riesgo(_, _, bajo).


%  Recomendaciones segun Severidad
recomendacion(Paciente, Enfermedad, Texto) :-
    severidad(Paciente, Enfermedad, Severidad),
    
    ( Severidad == 'Severa' ->
        Texto = 'Busque atencion medica inmediata o acuda a urgencias. Esta condicion requiere supervision profesional urgente.'
    ; Severidad == 'Moderada' ->
        Texto = 'Se recomienda consultar a su medico en las proximas 24-48 horas. Monitoree de cerca sus sintomas.'
    ; Severidad == 'Leve' ->
        Texto = 'Descanse, mantengase hidratado y aplique el tratamiento sugerido. Si los sintomas empeoran, contacte a un medico.'
    ; % Fallback
        Texto = 'Siga las instrucciones del especialista para la enfermedad detectada.'
    ).


% Diagnosticar y Tratar en un Solo Paso
diagnosticar_y_tratar(Paciente, Diagnostico, TratamientoFinal) :-
    reset_paciente(Paciente),
    
    ( arbol_diagnostico(Paciente, Diagnostico)          
    ; diagnostico_exclusivo(Paciente, Diagnostico)      
    ; distincion_fuerte(Paciente, Diagnostico)          
    ; diagnostico_preventivo(Paciente, Diagnostico)     
    ; diagnostico_basico(Paciente, Diagnostico)         
    )
    ->
    ( Diagnostico == 'sin_diagnostico_del_arbol' ->
        fail
    ;
        tratamiento(Diagnostico, TratamientoFinal)
    ).
    
% Fallback si ningun diagnostico pudo ser establecido
diagnosticar_y_tratar(_, 'No se pudo establecer un diagnostico.', 'No hay tratamiento sugerido.').

%  imprimir diagnosticos posibles
imprimir_posible_diagnostico(Paciente, E) :-
    probabilidad(Paciente, E, P),
    format('  - ~w: ~1f por ciento de probabilidad~n', [E, P]).


reporte(Paciente) :-
    writeln(''),
    writeln('==================================================='),
    format('INICIANDO REPORTE MEDICO DEL PACIENTE: ~w~n', [Paciente]),
    writeln('==================================================='),

    ( diagnosticar_y_tratar(Paciente, DiagnosticoFinal, TratamientoPrincipal) ->
        % Diagnostico exitoso
        true
    ;
        % Fallback si no hay diagnostico
        DiagnosticoFinal = 'No se pudo establecer un diagnostico.',
        TratamientoPrincipal = 'No hay tratamiento sugerido.'
    ),
    
    writeln(''),

    writeln('*** SINTOMAS CONFIRMADOS ***'),
    ( findall(S, sintoma(Paciente, S), ListaSintomas),
        ListaSintomas \== []
    ->
        atomic_list_concat(ListaSintomas, ', ', SintomasStr),
        format('  ~w~n', [SintomasStr])
    ;
        writeln('  -- Ningun sintoma confirmado --')
    ),
    writeln(''),

    writeln('*** ANALISIS DE POSIBLES ENFERMEDADES (Coincidencia Basica) ***'),
    ( findall(E, diagnostico_basico(Paciente, E), PosiblesDiagnosticos),
        PosiblesDiagnosticos \== []
    ->
        % Imprimir cada posible diagnostico con su probabilidad
        maplist(imprimir_posible_diagnostico(Paciente), PosiblesDiagnosticos)
    ;
        writeln('  -- No se encontraron coincidencias basicas --')
    ),
    writeln(''),
    
    writeln('*** DIAGNOSTICO FINAL Y METRICAS ***'),
    format('Diagnostico Principal: ~w~n', [DiagnosticoFinal]),

    ( DiagnosticoFinal \== 'No se pudo establecer un diagnostico.' ->
        % Calcular metricas
        severidad(Paciente, DiagnosticoFinal, Severidad),
        probabilidad(Paciente, DiagnosticoFinal, Porcentaje),
        riesgo(Paciente, DiagnosticoFinal, NivelRiesgo),
        tratamiento_combinado(Paciente, ListaCombinada),
        recomendacion(Paciente, DiagnosticoFinal, TextoRecomendacion),
        atomic_list_concat(ListaCombinada, ' / ', CombinadoStr),

        format('Probabilidad de ~w: ~1f por ciento~n', [DiagnosticoFinal, Porcentaje]),
        format('Severidad del Caso: ~w~n', [Severidad]),
        format('Nivel de Riesgo: ~w~n', [NivelRiesgo]),
        writeln('---------------------------------------------------'),
        format('Tratamiento Principal: ~w~n', [TratamientoPrincipal]),
        format('Tratamiento Combinado (Posibles Comorbilidades): ~w~n', [CombinadoStr]),
        writeln('---------------------------------------------------'),
        format('Recomendacion: ~w~n', [TextoRecomendacion])
        
    ;
        writeln('No fue posible calcular metricas adicionales por falta de diagnostico.') % No hacer nada si no hay diagnostico
    ),

    writeln('===================================================').


% Tratamiento Combinado
% Lista todos los tratamientos de las enfermedades que pasan el diagnostico basico.
tratamiento_combinado(Paciente, ListaTratamientos) :-
    % Usamos findall para encontrar todas las enfermedades (E) que cumplen con:
    findall(T, 
        (   % 1. La enfermedad E cumple con la condicion de diagnostico basico:
            diagnostico_basico(Paciente, E),
            % 2. Recuperamos el tratamiento para esa enfermedad:
            tratamiento(E, T) 
        ), 
        ListaTratamientosBruta), % Lista con posibles duplicados

    % 3. Eliminamos duplicados si varias enfermedades tuvieran el mismo tratamiento exacto
    sort(ListaTratamientosBruta, ListaTratamientos).

%  Diagnostico por Probabilidad
probabilidad(Paciente, Enfermedad, Porcentaje) :-
    findall(S, tiene_sintoma(Enfermedad, S), ListaTotal),
    length(ListaTotal, Totales),

    findall(S, (sintoma(Paciente, S), tiene_sintoma(Enfermedad, S)), ListaConfirmados),
    length(ListaConfirmados, Confirmados),

    ( Totales > 0 ->
        Porcentaje is (Confirmados / Totales) * 100
    ;
        Porcentaje is 0
    ).

%  Enfermedades Similares
% Dos enfermedades son similares si comparten al menos dos sintomas.
enfermedades_similares(E1, E2) :-
    E1 \== E2,
    
    tiene_sintoma(E1, _),
    
    tiene_sintoma(E2, _),

    findall(S, (tiene_sintoma(E1, S), tiene_sintoma(E2, S)), SintomasComunes),
    
    length(SintomasComunes, Cantidad),
    Cantidad >= 2.


sintomascontradictorios(Paciente) :-
    sintoma(Paciente, S1),
    sintoma(Paciente, S2),

    ( contradictorio(S1, S2)
    ; contradictorio(S2, S1)
    ),
    
    format('ADVERTENCIA: Se detectan sintomas potencialmente contradictorios: ~w y ~w.~n', [S1, S2]).

% ==========================================================%
% LOGICA DE DIAGNOSTICO PORCENTAJE
% ==========================================================%

buscar([], _, 0).
buscar(X, E, 1) :- sintomade(X, E).
buscar([X|Xs], E, P) :-
    enfermedad(E),
    (sintomade(X, E) -> S1 = 1 ; S1 = 0), % Corregido para manejar fallas
    buscar(Xs, E, S2),
    P is S1 + S2.

cantSint(E, C) :-
    findall(X, sintomade(X, E), L),
    length(L, C).

diagnostico(Sintomas, E, K) :-
    buscar(Sintomas, E, P),
    cantSint(E, T),
    T > 0, % Evita division por cero
    K is P * 100 / T.


% ==========================================================%
% PREDICADOS DE CONVERSACION ELIZA %
% ==========================================================%

% Predicado principal: Inicia la conversacion seleccionando un saludo aleatorio.
eliza:-
    greeting_message(Greetings),
    length(Greetings, MaxIndex),
    MaxIndex > 0, 
    RandomIndex is random(MaxIndex),
    nth0(RandomIndex, Greetings, Greeting),
    
    writeln(Greeting),
    writeln('por favor ingresa tu consulta, usar solo minusculas sin . al final:'),
    readln(Input),
    eliza(Input),!.

eliza(Input):- 
    (Input == ['adios']; Input == ['adios', '.']), 
    farewell_message(Farewells),
    length(Farewells, MaxIndex),
    MaxIndex > 0, 
    RandomIndex is random(MaxIndex),
    nth0(RandomIndex, Farewells, FarewellMsg),
    writeln(FarewellMsg), 
    !.

% --- MENSAJES ALEATORIOS (20 SALUDOS) ---
greeting_message([
    'Hola, soy Eliza, tu chatbot. ¿En que puedo ayudarte hoy?',
    'Saludos. Mi nombre es Eliza. Dime que te trae por aqui.',
    '¡Hola! Soy Eliza. Estoy lista para conversar, ¿cual es tu consulta?',
    'Buenas tardes. Soy Eliza, tu asistente virtual. ¿Que necesitas?',
    'Hola. Soy Eliza, un gusto. Por favor, escribe tu pregunta.',
    'Bienvenido/a. Me llamo Eliza. ¿Como podemos empezar?',
    '¿Que tal? Soy Eliza. Te escucho, recuerda usar minusculas.',
    'Hola, mi nombre es Eliza. ¿Tienes alguna pregunta o comentario?',
    'Un placer. Soy Eliza. Adelante, dime lo que piensas.',
    'Saludos cordiales. Eliza a tu servicio. ¿Cual es el tema de hoy?',
    'Hola, soy Eliza, tu companera de chat. Inicia la conversacion.',
    'Buenos dias. Soy Eliza. ¿Como puedo hacer tu dia mas facil?',
    'Hey, soy Eliza. No seas timido, cuentame que buscas.',
    'Hola. Te habla Eliza. Recuerda usar minusculas. ¿Cual es tu consulta?',
    '¡Que gusto! Me llamo Eliza. ¿Que tema quieres explorar?',
    'Adelante. Soy Eliza. Estoy lista para el dialogo.',
    'Hola. Soy Eliza, tu chatbot. ¿En que te puedo asesorar?',
    'Saludos. Mi nombre es Eliza. Dime que tienes en mente.',
    'Hola de nuevo. Soy Eliza. ¿Como va todo? Ingresa tu pregunta.',
    'Eliza aqui. ¿Que misterios vamos a desentranar hoy?'
]).

% --- MENSAJES ALEATORIOS (20 DESPEDIDAS) ---
farewell_message([
    '¡Adios! Espero que la conversacion te haya sido util.',
    'Hasta pronto. Fue un placer charlar contigo.',
    'Adios. Si tienes mas preguntas, vuelve cuando quieras.',
    '¡Nos vemos! Que tengas un excelente dia.',
    'Chao. Gracias por tu tiempo. Cuidate.',
    'Me despido. Espero verte pronto de nuevo.',
    'Adios. ¡Vuelve a consultarme si lo necesitas!',
    'Que te vaya bien. Gracias por conversar con Eliza.',
    'Hasta luego. Fue una interaccion interesante.',
    'Adios. ¡Recuerda seguir explorando el mundo de Prolog!',
    'Cierro sesion. Que tengas una buena jornada.',
    'Me retiro. ¡No dudes en llamarme de nuevo!',
    'Adios, adios. Todo listo por hoy.',
    'Finalizamos. Espero haber resuelto tus dudas.',
    'Nos despedimos. Ha sido un dialogo productivo.',
    'Hasta la proxima. Aqui estare.',
    'Adios. Exito en tus proximos proyectos.',
    'Que descanses. ¡Vuelve a visitarme pronto!',
    'Fue un honor. Adios y gracias.',
    'Desconexion. ¡Cuidate mucho!'
]).

% --- PREDICADO DE PROCESAMIENTO (eliza/1) ---

eliza(Input) :-
    template(Stim, Resp, IndStim),
    match(Stim, Input),
    replace0(IndStim, Input, 0, Resp, R),
    writeln(R),
    readln(Input1),
    eliza(Input1), !.

% --- MODULO FAMILIA Y TBBT ---
template([quien, es, el, padre, de, s(_)], [flagRelPadre], [5]).
template([quien, es, la, madre, de, s(_)], [flagRelMadre], [5]).
template([como, es, s(_)], [personalidad_fam], [2]).
template([quien, es, tio, de, s(_)], [flagTio], [4]).
template([quien, es, tia, de, s(_)], [flagTia], [4]).
template([quien, es, primo, de, s(_)], [flagPrimo], [4]).
template([quien, es, prima, de, s(_)], [flagPrima], [4]).
template([quien, es, abuelo, de, s(_)], [flagAbuelo], [4]).
template([quien, es, el, abuelo, de, s(_)], [flagAbuelo], [5]).

template([que, sabes, de, sheldon], [info_sheldon], []).
template([quien, es, la, pareja, de, s(_)], [flagPareja], [5]).
template([bazinga], [flagBazinga], []).

template([a, que, se, dedica, s(_)], [flagProfesion], [4]).

template([a, que, le, tiene, miedo, s(_)], [flagMiedo], [5]).

template([quien, es, el, mejor, amigo, de, s(_)], [flagAmigo], [6]).


template([tengo, s(_), y, s(_)], [flagDiagnostico2], [1, 3]).
template([tengo, s(_)], [flagDiagnostico1], [1]).
template([sufro, de, s(_), y, s(_)], [flagDiagnostico2], [2, 4]).

template([medicina, para, la, s(_)], [flagMedicinaEnfermedad], [3]).
template([que, medicina, sirve, para, la, s(_)], [flagMedicinaEnfermedad], [5]).

template([especialista, para, la, s(_)], [flagEspecialistaEnfermedad], [3]).

template([que, doctor, atiende, la, s(_)], [flagEspecialistaSintoma], [4]).
template([quien, atiende, la, s(_)], [flagEspecialistaSintoma], [3]).

template([me, gustaria, un, diagnostico], [flagDiagnosticoInteractivo], []).
template([puedes, diagnosticarme], [flagDiagnosticoInteractivo], []).

template([hola, mi, nombre, es, s(_)], ['Hola', 0, 'Como', estas, tu, '?'], [4]).
template([buendia, mi, nombre, es, s(_)], ['buen dia', 'Como', estas, tu, 0, '?'], [4]).

template([hola, ',', mi, nombre, es, s(_)], ['Hola', 0, 'Como', estas, tu, '?'], [5]).
template([buendia, ',', mi, nombre, es, s(_)], ['Buendia', 'Como', estas, tu, 0, '?'], [5]).

template([hola, _], ['Hola', 'como', estas, tu, '?'], []).
template([buendia, _], ['Buendia', 'Como', estas, tu, '?'], []).

template([yo, s(_), yo, soy, s(_)], [por, que, 0, eres, 1, '?'], [1, 4]).
template([yo, s(_), tu], [why, do, you, 0, me ,'?'], [1]).
template([yo, soy, s(_)], [porque, eres, tu, 0, '?'], [2]).

template([te, gustan, las, s(_)], [flagLike], [3]).
template([te, gustan, los, s(_)], [flagLike], [3]).
template([te, gusta, el, s(_)], [flagLike], [3]).
template([te, gusta, la, s(_)], [flagLike], [3]).

template([tu, eres, s(_), _], [flagDo], [2]).
% pregunta algo que es eliza
template([que, eres, tu, s(_)], [flagIs], [3]).
template([eres, s(_)], [flagIs], [1]).
template([eres, s(_), '?'], [flagIs], [1]).
template([tu, eres, s(_)], [flagIs], [2]).




template([como, estas, tu, '?'], [yo, estoy, bien, ',', gracias, por, preguntar], []).

template([yo, pienso, que, _], [bueno, esa, es, tu, opinion], []).
template([porque, _], [esa, no, es, una, buena, razon], []).
template([i, have, s(_), with, s(_)], ['You', have, to, deal, with, your, 0, and, your, 1, in, a, mature, way], [2, 4]).
template([i, s(_), _], [i, can, recommend, you, a, book, about, that, issue], []).
template([please, s(_), _], ['No', i, can, not, help, ',', i, am, just, a, machine], []). 
template([tell, me, a, s(_), _], ['No', i, can, not, ',', i, am, bad, at, that], []).
template(_, ['Porfa', explica, un, poco, mas], []). 

% --- GUSTOS DE ELIZA (10 items) ---
% Lo que le gusta a eliza : flagLike
elizaLikes(X, R):- likes(X), R = ['Asies', me, gusta, X].
elizaLikes(X, R):- \+likes(X), R = ['Nou', 'no', me, gustan, X].
likes(apples).
likes(ponies).
likes(zombies).
likes(manzanas).
likes(computadoras).
likes(carros).
likes(libros).
likes(musica).
likes(programacion).
likes(cafe).

% --- HABILIDADES DE ELIZA ---
% lo que hace eliza: flagDo
elizaDoes(X, R):- does(X), R = ['Yes', i, X, and, i, love, it].
elizaDoes(X, R):- \+does(X), R = ['No', i, do, not, X ,'.', it, is, too, hard, for, me].
does(study).
does(cook).
does(work).

% lo que es eliza: flagIs
elizaIs(X, R):- is0(X), R = ['Yes', yo, soy, X].
elizaIs(X, R):- \+is0(X), R = ['No', i, am, not, X].
is0(dumb).
is0(weird).
is0(nice).
is0(fine).
is0(happy).
is0(redundant).

% Implementacion de las flags ---------------------

elizaDiagnosticoInteractivo(R) :-
    Paciente = juan,
    reset_paciente(Paciente),
    
    writeln('Comenzare la sesion de preguntas. Por favor, responde con "si" o "no" (en minusculas y sin punto).'),

    ( 
        (writeln('--- INICIANDO DIAGNOSTICO POR ARBOL DE DECISION ---'), arbol_diagnostico(Paciente, EnfSosp), Regla = arbol)
        ; 
        (writeln('--- INICIANDO DIAGNOSTICO POR SINTOMA EXCLUSIVO ---'), diagnostico_exclusivo(Paciente, EnfSosp), Regla = exclusivo)
        ; 
        (writeln('--- INICIANDO DIAGNOSTICO POR DISTINCION FUERTE ---'), distincion_fuerte(Paciente, EnfSosp), Regla = fuerte)
        ; 
        (writeln('--- INICIANDO DIAGNOSTICO PREVENTIVO (PROBABILIDAD) ---'), diagnostico_preventivo(Paciente, EnfSosp), Regla = preventivo)
        ; 
        (writeln('--- INICIANDO DIAGNOSTICO BASICO (A LA MALA) ---'), diagnostico_basico(Paciente, EnfSosp), Regla = basico)
    ) ->
    
    ( EnfSosp \== 'sin_diagnostico_del_arbol' ->
        findall(S, tiene_sintoma(EnfSosp, S), ListaSintomas),
        % El forall obliga a preguntar cada sintoma de la lista
        forall(member(Sint, ListaSintomas), (pregunta(Paciente, Sint) ; true))
    ; true ),
    
    Enfermedad = EnfSosp,
    
    ( Enfermedad = 'sin_diagnostico_del_arbol' -> 
        R = ['El', arbol, de, decision, no, fue, concluyente, '.']
    ;
        severidad(Paciente, Enfermedad, Severidad),
        tratamiento(Enfermedad, TratamientoPrincipal),
        especialista(Enfermedad, Especialista),
        probabilidad(Paciente, Enfermedad, Porcentaje),
        riesgo(Paciente, Enfermedad, NivelRiesgo),
        recomendacion(Paciente, Enfermedad, TextoRecomendacion),
        
        % Revisar si hay contradicciones despues de todas las preguntas
        (sintomascontradictorios(Paciente) -> true ; true),
        
        % Cálculo de Tratamiento Combinado
        ( (Regla == preventivo ; Regla == basico), tratamiento_combinado(Paciente, ListaCombinada), ListaCombinada \== [] ->
            atomic_list_concat(ListaCombinada, ' / ', CombinadoStr),
            format(atom(Msg), 'El diagnostico mas probable es ~w (~1f por ciento). Nivel de Riesgo: ~w. Tratamiento Principal: ~w. Tratamiento Combinado (Posibles): ~w. Consulte a un ~w. RECOMENDACION: ~w', 
                        [Enfermedad, Porcentaje, NivelRiesgo, TratamientoPrincipal, CombinadoStr, Especialista, TextoRecomendacion])
        ;
            format(atom(Msg), 'El diagnostico es ~w con una probabilidad de ~1f por ciento. Nivel de Riesgo: ~w. Severidad: ~w. Consulte a un ~w. Tratamiento sugerido: ~w. RECOMENDACION: ~w', 
                        [Enfermedad, Porcentaje, NivelRiesgo, Severidad, Especialista, TratamientoPrincipal, TextoRecomendacion])
        ), 
        
        atom_to_list_of_words(Msg, R)
    )
    
    % CUERPO ELSE
    ;
        (
        R = [lo, siento, no, pude, encontrar, ningun, sintoma, coincidente, para, darte, un, diagnostico, fiable]
    ).

% Flag: Diagnostico con un sintoma (e.g., 'tengo tos') porcen
elizaDiagnostico1(Sintoma, R) :-
    findall(E-K, (enfermedad(E), diagnostico([Sintoma], E, K), K >= 20), Diagnosticos),
    sort(2, @>=, Diagnosticos, SortedDiagnosticos),
    ( SortedDiagnosticos = [E1-K1 | _] ->
        format(atom(Msg), 'Basado en el sintoma ~w, tu diagnostico mas probable es ~w con un ~1f por ciento de coincidencia.', [Sintoma, E1, K1]),
        atom_to_list_of_words(Msg, R)
    ;
        R = ['Ese', sintoma, no, genera, un, diagnostico, claro, '.']
    ).

% Fallback para el diagnostico con un sintoma
elizaDiagnostico1(Sintoma, R) :-
    \+ sintomade(Sintoma, _),
    R = ['Ese', sintoma, no, esta, registrado, en, mi, base, de, datos].

% Flag: Diagnostico con dos sintomas (e.g., 'tengo tos y fiebre') - Por porcentaje
elizaDiagnostico2(Sintoma1, Sintoma2, R) :-
    findall(E-K, (enfermedad(E), diagnostico([Sintoma1, Sintoma2], E, K), K >= 20), Diagnosticos),
    sort(2, @>=, Diagnosticos, SortedDiagnosticos),
    ( SortedDiagnosticos = [E1-K1 | _] ->
        format(atom(Msg), 'Teniendo en cuenta ~w y ~w, el diagnostico mas probable es ~w con un ~1f por ciento de coincidencia.', [Sintoma1, Sintoma2, E1, K1]),
        atom_to_list_of_words(Msg, R)
    ;
        R = ['Los', sintomas, no, generan, un, diagnostico, claro, '.']
    ).

% Fallback para el diagnostico con dos sintomas
elizaDiagnostico2(Sintoma1, Sintoma2, R) :-
    \+ sintomade(Sintoma1, _),
    \+ sintomade(Sintoma2, _),
    R = ['Ni', Sintoma1, ni, Sintoma2, estan, registrados, en, mi, base, de, datos].

% Flag: Medicina para una enfermedad
elizaMedicina(Enfermedad, R) :-
    enfermedad(Enfermedad),
    tratamiento(Enfermedad, Tratamiento),
    format(atom(Msg), 'Para la enfermedad ~w se recomienda: ~w', [Enfermedad, Tratamiento]),
    atom_to_list_of_words(Msg, R).

% Fallback de Medicina
elizaMedicina(Enfermedad, R) :-
    \+ enfermedad(Enfermedad),
    R = ['La', enfermedad, no, esta, en, mi, registro].
elizaMedicina(Enfermedad, R) :-
    enfermedad(Enfermedad),
    \+ tratamiento(Enfermedad, _),
    R = ['No', tengo, registro, de, medicinas, para, 0].

% Flag: Especialista para una enfermedad
elizaEspecialistaEnfermedad(Enfermedad, R) :-
    enfermedad(Enfermedad),
    especialista(Enfermedad, Esp),
    format(atom(Msg), 'Para la enfermedad ~w debes consultar a un ~w', [Enfermedad, Esp]),
    atom_to_list_of_words(Msg, R).

% Fallback de Especialista
elizaEspecialistaEnfermedad(Enfermedad, R) :-
    \+ enfermedad(Enfermedad),
    R = ['La', enfermedad, no, esta, en, mi, registro].

% Flag: Especialista para un sintoma
elizaEspecialistaSintoma(Sintoma, R) :-
    findall(Enf, sintomade(Sintoma, Enf), Enfs),
    Enfs \= [],
    % Tomar la primera enfermedad encontrada para la explicacion
    Enfs = [Enf|_], 
    especialista(Enf, Esp),
    format(atom(Msg), 'El sintoma ~w esta asociado con ~w, que puede ser atendido por un ~w', [Sintoma, Enf, Esp]),
    atom_to_list_of_words(Msg, R).

% Fallback de Especialista
elizaEspecialistaSintoma(Sintoma, R) :-
    \+ sintomade(Sintoma, _),
    R = ['Ese', sintoma, no, esta, asociado, a, ninguna, enfermedad, en, mi, base, de, datos].


% --- PREDICADOS AUXILIARES Y DE REEMPLAZO 

match([],[]).
match([], _):- true.

match([S|Stim],[I|Input]) :-
    atom(S), % si I es un s(X) devuelve falso
    S == I,
    match(Stim, Input),!.

match([S|Stim],[_|Input]) :-
    \+atom(S),
    match(Stim, Input),!.


% --- RESPUESTAS MODULO FAMILIA Y TBBT ---
replace0([I|_], Input, _, [flagRelPadre], R) :- 
    nth0(I, Input, Nombre),
    (padre_de(P, Nombre) -> R = [el, padre, de, Nombre, es, P] ; R = [no, se, quien, es, su, padre]), !.

replace0([I|_], Input, _, [flagRelMadre], R) :- 
    nth0(I, Input, Nombre),
    (madre_de(M, Nombre) -> R = [la, madre, de, Nombre, es, M] ; R = [no, se, quien, es, su, madre]), !.

replace0([I|_], Input, _, [personalidad_fam], R) :- 
    nth0(I, Input, Nombre),
    (es(Nombre, Adj) -> R = [se, dice, que, Nombre, es, muy, Adj] ; R = [no, conozco, la, personalidad, de, Nombre]), !.

replace0([I|_], Input, _, [flagPareja], R) :-
    nth0(I, Input, Nombre),
    (pareja_de(Nombre, P) -> R = [la, pareja, de, Nombre, es, P] ; 
        pareja_de(P, Nombre) -> R = [la, pareja, de, Nombre, es, P] ;
        R = [no, estoy, segura, de, si, Nombre, tiene, pareja]), !.


% Respuesta para Tíos (Hombres)
replace0([I|_], Input, _, [flagTio], R) :-
    nth0(I, Input, Nombre),
    (setof(T, (es_tio_de(T, Nombre), hombre(T)), Lista) -> 
        R = [los, tios, de, Nombre, son, Lista] ; 
        R = [no, encontre, tios, para, Nombre]), !.

% Respuesta para Tías (Mujeres)
replace0([I|_], Input, _, [flagTia], R) :-
    nth0(I, Input, Nombre),
    (setof(T, (es_tio_de(T, Nombre), mujer(T)), Lista) -> 
        R = [las, tias, de, Nombre, son, Lista] ; 
        R = [no, encontre, tias, para, Nombre]), !.

% Respuesta para Primos
replace0([I|_], Input, _, [flagPrimo], R) :-
    nth0(I, Input, Nombre),
    (setof(C, (es_primo_de(C, Nombre), hombre(C)), Lista) -> 
        R = [los, primos, de, Nombre, son, Lista] ; 
        R = [no, conozco, a, los, primos, de, Nombre]), !.

% Respuesta para Primas
replace0([I|_], Input, _, [flagPrima], R) :-
    nth0(I, Input, Nombre),
    (setof(C, (es_primo_de(C, Nombre), mujer(C)), Lista) -> 
        R = [las, primas, de, Nombre, son, Lista] ; 
        R = [no, encontre, primas, para, Nombre]), !.

        

replace0([I|_], Input, _, [flagAbuelo], R) :-
    nth0(I, Input, Nombre),
    (setof(A, abuelo_de(A, Nombre), Lista) -> 
        R = [los, abuelos, de, Nombre, son, Lista] ; 
        R = [no, tengo, registrados, los, abuelos, de, Nombre]), !.

replace0([], _, _, [info_sheldon], [sheldon, cooper, es, un, fisico, teorico, que, ama, los, trenes, y, dice, bazinga]) :- !.

replace0([], _, _, [flagBazinga], [pense, que, era, una, broma, pero, veo, que, sabes, de, sheldon]) :- !.

% Respuesta para Profesión
replace0([I|_], Input, _, [flagProfesion], R) :-
    nth0(I, Input, Nombre),
    (profesion(Nombre, Prof) -> R = [Nombre, es, Prof] ; R = [no, se, a, que, se, dedica, Nombre]), !.

% Respuesta para Miedos
replace0([I|_], Input, _, [flagMiedo], R) :-
    nth0(I, Input, Nombre),
    (miedo(Nombre, M) -> R = [Nombre, le, tiene, miedo, M] ; R = [parece, que, Nombre, es, muy, valiente]), !.

% Respuesta para Mejor Amigo
replace0([I|_], Input, _, [flagAmigo], R) :-
    nth0(I, Input, Nombre),
    (mejor_amigo_de(Nombre, Amigo) -> R = [el, mejor, amigo, de, Nombre, es, Amigo] ; 
     mejor_amigo_de(Amigo, Nombre) -> R = [el, mejor, amigo, de, Nombre, es, Amigo] ;
     R = [no, se, quien, sea, su, mejor, amigo]), !.

% 1. PRIORIDAD MAXIMA: Si la respuesta es el flag de diagnostico, se ejecuta la logica medica.
replace0(_, _, _, [flagDiagnosticoInteractivo], R):- 
    elizaDiagnosticoInteractivo(R), !.

% 2. DIAGNOSTICO DE 2 SINTOMAS: (Ej. tengo tos y fiebre)
replace0([I1, I2|_], Input, _, [flagDiagnostico2], R):-
    nth0(I1, Input, Atom1),
    nth0(I2, Input, Atom2),
    elizaDiagnostico2(Atom1, Atom2, R), !.

% 3. LOGICA PARA FLAGS CON 1 INDICE (Likes, Diagnostico1, Medicinas, Especialistas)
replace0([I|_], Input, _, Resp, R):-
    nth0(I, Input, Atom),
    nth0(0, Resp, X),
    (X == flagLike -> elizaLikes(Atom, R);
        X == flagDo -> elizaDoes(Atom, R);
        X == flagIs -> elizaIs(Atom, R);
        X == flagDiagnostico1 -> elizaDiagnostico1(Atom, R);
        X == flagMedicinaEnfermedad -> elizaMedicina(Atom, R);
        X == flagEspecialistaEnfermedad -> elizaEspecialistaEnfermedad(Atom, R);
        X == flagEspecialistaSintoma -> elizaEspecialistaSintoma(Atom, R)), !.

% 4. REEMPLAZO ESTANDAR PARA FRASES COMUNES 
replace0([I|Index], Input, N, Resp, R):-
    length(Index, M), M =:= 0,
    nth0(I, Input, Atom),
    select(N, Resp, Atom, R1), append(R1, [], R), !.

replace0([I|Index], Input, N, Resp, R):-
    nth0(I, Input, Atom),
    length(Index, M), M > 0,
    select(N, Resp, Atom, R1),
    N1 is N + 1,
    replace0(Index, Input, N1, R1, R), !.

% 5. FALLBACK FINAL: Si no hay indices que reemplazar, devuelve la respuesta tal cual
replace0([], _, _, Resp, R):- append(Resp, [], R), !.





% ===================================================
% BASE DE CONOCIMIENTO: FAMILIA Y TBBT
% ===================================================

% --- GENEALOGIA FAMILIA ---
padre_de(eduardo, kevin). padre_de(eduardo, uriel). padre_de(eduardo, jairo).
madre_de(edit, kevin). madre_de(edit, uriel). madre_de(edit, jairo).
padre_de(jairo, farid).
padre_de(paco, edit). padre_de(paco, willy). padre_de(paco, hugo). padre_de(paco, victor).
madre_de(toya, edit). madre_de(toya, willy). madre_de(toya, hugo). madre_de(toya, victor).

% Tío Willy y Tita
padre_de(willy, maricruz). padre_de(willy, karla). padre_de(willy, boby).
madre_de(tita, maricruz). madre_de(tita, karla). madre_de(tita, boby).
es(willy, extranjero). es(tita, extranjero). es(maricruz, extranjero). 
es(karla, extranjero). es(boby, padrino).

% CONECTANDO A LOS HERMANOS (Hijos de Paco y Toya)
padre_de(paco, edit).
padre_de(paco, magali).
padre_de(paco, willy).
padre_de(paco, hugo).
padre_de(paco, victor).
padre_de(paco, fani).
padre_de(paco, martel).

madre_de(toya, edit).
madre_de(toya, magali).
madre_de(toya, willy).
madre_de(toya, hugo).
madre_de(toya, victor).
madre_de(toya, fani).
madre_de(toya, martel).

% CONECTANDO A LOS HERMANOS PATERNOS (Hijos de David y Lupe)
padre_de(david_abuelo, eduardo).
padre_de(david_abuelo, tona).
padre_de(david_abuelo, salvador).

madre_de(lupe_abuela, eduardo).
madre_de(lupe_abuela, tona).
madre_de(lupe_abuela, salvador).

% Tío Hugo y Maribel
padre_de(hugo, maritza). padre_de(hugo, huguito). padre_de(hugo, sara). padre_de(hugo, toyi).
madre_de(maribel, maritza). madre_de(maribel, huguito). madre_de(maribel, sara). madre_de(maribel, toyi).
es(hugo, trabajador). es(maribel, inteligente). es(maritza, profesional). 
es(huguito, inteligente). es(sara, profesional). es(toyi, inteligente).

% Tío Victor y Araceli
padre_de(victor, getse). padre_de(victor, gaby). padre_de(victor, victorin).
madre_de(araceli, getse). madre_de(araceli, gaby). madre_de(araceli, victorin).
es(victor, trabajador). es(araceli, lejano). es(getse, lejano). 
es(gaby, carismatica). es(victorin, social).

% Tía Magali y Nan
padre_de(nan, kichi). padre_de(nan, chucho). padre_de(nan, chino).
madre_de(magali, kichi). madre_de(magali, chucho). madre_de(magali, chino).
es(magali, noble). es(nan, trabajador). es(kichi, extranjero). 
es(chucho, chavo). es(chino, chavo).

% Tía Fani y Ricardo
padre_de(ricardo, jordan). madre_de(fani, jordan).
es(fani, lejano). es(ricardo, lejano). es(jordan, lejano).

% Tío Martel y Lupe
padre_de(martel, nico). madre_de(lupe, nico).
es(martel, noble). es(lupe, lejano). es(nico, carismatico).

% --- CONTINUACIÓN FAMILIA PATERNA ---
% Abuelos Paternos
padre_de(david_abuelo, tona). padre_de(david_abuelo, salvador).
madre_de(lupe_abuela, tona). madre_de(lupe_abuela, salvador).
es(lupe_abuela, noble). es(david_abuelo, lejano).

% Tía Toña y Toño
padre_de(tono, cholda). padre_de(tono, beto). padre_de(tono, berenice). padre_de(tono, karla_p). padre_de(tono, david_p).
madre_de(tona, cholda). madre_de(tona, beto). madre_de(tona, berenice). madre_de(tona, karla_p). madre_de(tona, david_p).
es(tona, noble). es(tono, lejano). es(cholda, lejano). es(beto, profesional). 
es(berenice, lejano). es(karla_p, lejano). es(david_p, lejano).

% Tío Salvador y Patricia
padre_de(salvador, gaby_p). padre_de(salvador, jimena). padre_de(salvador, samantha).
madre_de(patricia, gaby_p). madre_de(patricia, jimena). madre_de(patricia, samantha).
es(salvador, lejano). es(patricia, lejano). es(gaby_p, carismatico). 
es(jimena, inteligente). es(samantha, trabajador).

% --- GENEROS ---
hombre(kevin). hombre(uriel). hombre(jairo). hombre(farid). hombre(eduardo). hombre(paco).
hombre(willy). hombre(hugo). hombre(victor). hombre(sheldon). hombre(leonard).
mujer(edit). mujer(toya). mujer(penny). mujer(amy). mujer(magali).
hombre(raj). hombre(howard).

mujer(tita). mujer(maribel). mujer(araceli). mujer(fani). mujer(lupe). 
mujer(tona). mujer(patricia). mujer(maricruz). mujer(karla). mujer(maritza). 
mujer(sara). mujer(toyi). mujer(getse). mujer(gaby). mujer(kichi). 
mujer(lupe_abuela). mujer(bernadette). mujer(gaby_p). mujer(jimena). mujer(samantha).

hombre(paco). hombre(willy). hombre(hugo). hombre(victor). hombre(martel). 
hombre(nan). hombre(ricardo). hombre(tono). hombre(salvador). hombre(david_abuelo).
hombre(huguito). hombre(boby). hombre(victorin). hombre(chucho). hombre(chino). 
hombre(nico). hombre(jordan). hombre(beto). hombre(david_p). hombre(howard). hombre(raj).

es(uriel, trabajador). es(jairo, noble). es(farid, noble). es(edit, noble). 
es(eduardo, trabajador). es(kevin, trabajador). es(paco, trabajador). es(toya, noble).
es(sheldon, inteligente). es(penny, carismatica).


pareja_de(willy, tita).
pareja_de(hugo, maribel).
pareja_de(victor, araceli).
pareja_de(nan, magali).
pareja_de(ricardo, fani).
pareja_de(martel, lupe).
pareja_de(tono, tona).
pareja_de(salvador, patricia).

% --- THE BIG BANG THEORY ---
pareja_de(sheldon, amy). pareja_de(leonard, penny). pareja_de(howard, bernadette).
mejor_amigo_de(sheldon, leonard).

% Profesiones
profesion(amy, neurobiologa).
profesion(leonard, fisico_experimental).
profesion(sheldon, fisico_teorico).

% Miedos
miedo(raj, a_las_mujeres).
miedo(howard, a_su_mama).

% Amistad
mejor_amigo_de(sheldon, leonard).

% --- REGLAS LOGICAS DE PARENTESCO ---
abuelo_de(A, N) :- progenitor(A, P), progenitor(P, N).

% --- REGLAS DE PARENTESCO AVANZADAS ---

% Regla de Progenitor (Padre o Madre)
progenitor(X, Y) :- padre_de(X, Y) ; madre_de(X, Y).

% Regla de Hermanos (Comparten al menos un progenitor)
hermano_de(X, Y) :- progenitor(P, X), progenitor(P, Y), X \== Y.

% Regla de Tíos (Sangre: hermano del padre/madre)
tio_sangre_de(T, S) :- progenitor(P, S), hermano_de(T, P).

% Regla de Tíos (Políticos: pareja de un hermano del padre/madre)
tio_politico_de(TP, S) :- tio_sangre_de(TS, S), (pareja_de(TS, TP) ; pareja_de(TP, TS)).

% Tío General
es_tio_de(T, S) :- tio_sangre_de(T, S) ; tio_politico_de(T, S).

% Primo General
es_primo_de(C, Yo) :- es_tio_de(T, Yo), progenitor(T, C).