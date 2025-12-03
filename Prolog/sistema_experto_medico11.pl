% ==========================================================
% HECHOS: Enfermedades y Síntomas
% ==========================================================

tiene_sintoma(gripe, fiebre).
tiene_sintoma(gripe, dolor_cabeza).
tiene_sintoma(gripe, congestion).

tiene_sintoma(alergia, estornudos).
tiene_sintoma(alergia, picazon_ojos).
tiene_sintoma(alergia, congestion).

tiene_sintoma(migrana, dolor_cabeza_severo).
tiene_sintoma(migrana, sensibilidad_luz).
tiene_sintoma(migrana, nauseas).

tiene_sintoma(resfriado, estornudos).
tiene_sintoma(resfriado, congestion).
tiene_sintoma(resfriado, dolor_garganta).

% --- Nuevas Enfermedades ---

% Candidiasis Oral (Muguet)
tiene_sintoma(candidiasis_oral, placas_blancas).
tiene_sintoma(candidiasis_oral, ardor_bucal).
tiene_sintoma(candidiasis_oral, dolor_al_tragar).
tiene_sintoma(candidiasis_oral, sangrado_leve).
tiene_sintoma(candidiasis_oral, agrietamiento_comisuras_boca).

% Cáncer de Próstata
tiene_sintoma(cancer_de_prostata, dificultad_para_orinar).
tiene_sintoma(cancer_de_prostata, miccion_frecuente_nocturna).
tiene_sintoma(cancer_de_prostata, flujo_de_orina_debil).
tiene_sintoma(cancer_de_prostata, sangre_en_la_orina).
tiene_sintoma(cancer_de_prostata, dolor_huesos).

% Cólera
tiene_sintoma(colera, diarrea_acuosa_repentina).
tiene_sintoma(colera, vomitos).
tiene_sintoma(colera, deshidratacion_severa).
tiene_sintoma(colera, sed_extrema).
tiene_sintoma(colera, calambres_musculares).


% ==========================================================
% HECHOS: Tratamientos
% ==========================================================

tratamiento(gripe, 'Reposo, hidratacion, paracetamol y aislamiento.').
tratamiento(alergia, 'Antihistaminicos y evitar el alergeno conocido.').
tratamiento(migrana, 'Medicacion especifica, ambiente oscuro y tranquilo.').
tratamiento(resfriado, 'Liquidos calientes, descongestionantes y vitamina C.').

% --- Nuevos Tratamientos ---

tratamiento(candidiasis_oral, 'Medicamentos antifungicos (nistatina, fluconazol) y buena higiene bucal.').
tratamiento(cancer_de_prostata, 'Opciones incluyen cirugia, radioterapia, terapia hormonal y quimioterapia, dependiendo de la etapa.').
tratamiento(colera, 'Rehidratacion oral o intravenosa intensiva e inmediata, y antibioticos (ej. doxiciclina).').


% ==========================================================
% PREDICADO DINÁMICO
% ==========================================================

% Define 'sintoma' como un predicado dinámico para poder agregar hechos en tiempo de ejecución
:- dynamic sintoma/2.

% Predicado para eliminar todos los síntomas asociados a un paciente (para un nuevo diagnóstico)
reset_paciente(P) :- retractall(sintoma(P,_)).

% Sistema de interacción: verifica si un síntoma ya ha sido preguntado y registrado,
% si no lo ha sido, pregunta al usuario y registra el resultado.
pregunta(Paciente, Sintoma) :-
    % 1. Si ya se conoce el síntoma para el paciente, se confirma automáticamente (cut, !)
    sintoma(Paciente, Sintoma), !.

pregunta(Paciente, Sintoma) :-
    % 2. Si no se conoce, se pregunta al usuario
    write('¿El paciente '), write(Paciente),
    write(' tiene '), write(Sintoma), write('? (si/no): '),
    read(Resp),
    ( Resp = si ->
        % Si la respuesta es 'si', se registra el hecho y tiene éxito
        assertz(sintoma(Paciente, Sintoma))
    ;
        % Si la respuesta es 'no', la regla falla, indicando que el paciente no tiene el síntoma
        fail
    ).

% ==========================================================
% DIAGNÓSTICO BÁSICO (Por un síntoma clave)
% ==========================================================

% Intenta diagnosticar si el paciente tiene un síntoma clave de la Enfermedad
diagnostico_basico(Paciente, Enfermedad) :-
    tiene_sintoma(Enfermedad, S), % Encuentra un síntoma S de la Enfermedad
    pregunta(Paciente, S).        % Pregunta o confirma si el paciente tiene ese síntoma S

% ==========================================================
% DIAGNÓSTICO COMPLETO (Por todos los síntomas)
% ==========================================================

% Intenta diagnosticar si el paciente tiene TODOS los síntomas de la Enfermedad
diagnostico_completo(Paciente, Enfermedad) :-
    % Encuentra todos los síntomas S de la Enfermedad y los almacena en Lista
    findall(S, tiene_sintoma(Enfermedad, S), Lista),
    % Verifica que todos los síntomas en Lista estén confirmados para el Paciente
    todos_confirmados(Paciente, Lista).

% Base case: La lista de síntomas está vacía
todos_confirmados(_, []).
% Recursive case: Pregunta/confirma el primer síntoma S y llama recursivamente con el resto R
todos_confirmados(Paciente, [S|R]) :-
    pregunta(Paciente, S),
    todos_confirmados(Paciente, R).

% ==========================================================
% DISTINCIÓN FUERTE Y TRATAMIENTOS (Para casos ambiguos como gripe vs resfriado)
% ==========================================================

% Regla de distinción para la Gripe: Diagnóstico básico + Síntoma clave (fiebre) + Ausencia de otro síntoma clave (estornudos)
distincion_fuerte(P, gripe) :-
    diagnostico_basico(P, gripe),
    pregunta(P, fiebre),
    \+ pregunta(P, estornudos).

% Regla de distinción para el Resfriado: Diagnóstico básico + Síntoma clave (estornudos) + Ausencia de otro síntoma clave (fiebre)
distincion_fuerte(P, resfriado) :-
    diagnostico_basico(P, resfriado),
    pregunta(P, estornudos),
    \+ pregunta(P, fiebre).

% Obtiene el tratamiento: Primero intenta la distinción fuerte, si falla, usa el diagnóstico básico.
obtener_tratamiento(P, Trat) :-
    (distincion_fuerte(P, E) ; diagnostico_basico(P, E)), % Intenta distinguir fuertemente o diagnosticar
    tratamiento(E, Trat).

% ==========================================================
% SEVERIDAD (Basado en el conteo de síntomas confirmados)
% ==========================================================

% Cuenta cuántos síntomas de una Enfermedad (E) han sido confirmados para un Paciente (P)
contar_sintomas_confirmados(P, Enfermedad, C) :-
    % Encuentra todos los síntomas S que tiene la enfermedad Y están registrados para el paciente
    findall(S, (tiene_sintoma(Enfermedad,S), sintoma(P,S)), L),
    % Cuenta la longitud de la lista de síntomas confirmados
    length(L, C).

% Severidad: 3 o más síntomas confirmados
severidad(P, E, 'Severa') :-
    contar_sintomas_confirmados(P, E, C), C >= 3, !.

% Severidad: 2 síntomas confirmados
severidad(P, E, 'Moderada') :-
    contar_sintomas_confirmados(P, E, C), C = 2, !.

% Severidad: 1 síntoma confirmado
severidad(P, E, 'Leve') :-
    contar_sintomas_confirmados(P, E, C), C = 1, !.

% Severidad: 0 síntomas confirmados (no se aplica en un diagnóstico exitoso)
severidad(_, _, 'No aplica') :- !.