% --- SISTEMA DE DIAGNÓSTICO MÉDICO EXPERTO (Base de Conocimiento) ---

%% Declaraciones de enfermedades
enfermedad(gripe).
enfermedad(rubeola).
enfermedad(malaria).
enfermedad(hepatitis).
enfermedad(tuberculosis).
enfermedad(anemia).
enfermedad(candidiasis_oral).
enfermedad(cancer_de_prostata).
enfermedad(colera).

%% Declaraciones de síntomas según enfermedad
sintomade(tos, gripe).
sintomade(cansancio, gripe).
sintomade(fiebre, gripe).
sintomade(dolorcabeza, gripe).

sintomade(nauseas, hepatitis).
sintomade(diarrea, hepatitis).
sintomade(ictericia, hepatitis).

sintomade(cansancio, anemia).
sintomade(apatia, anemia).
sintomade(nauseas, anemia).

sintomade(tos, tuberculosis).
sintomade(cansancio, tuberculosis).
sintomade(fiebre, tuberculosis).
sintomade(escalofrios, tuberculosis).

sintomade(escalofrios, malaria).
sintomade(fiebre, malaria).
sintomade(diarrea, malaria).
sintomade(ictericia, malaria).

sintomade(fiebre, rubeola).
sintomade(jaqueca, rubeola).
sintomade(secrecion, rubeola).

sintomade(placas_blancas, candidiasis_oral).
sintomade(ardor_bucal, candidiasis_oral).
sintomade(dolor_al_tragar, candidiasis_oral).
sintomade(sangrado_leve, candidiasis_oral).
sintomade(agrietamiento_comisuras_boca, candidiasis_oral).

sintomade(dificultad_para_orinar, cancer_de_prostata).
sintomade(miccion_frecuente_nocturna, cancer_de_prostata).
sintomade(flujo_de_orina_debil, cancer_de_prostata).
sintomade(sangre_en_la_orina, cancer_de_prostata).
sintomade(dolor_huesos, cancer_de_prostata).

sintomade(diarrea_acuosa_repentina, colera).
sintomade(vomitos, colera).
sintomade(deshidratacion_severa, colera).
sintomade(sed_extrema, colera).
sintomade(calambres_musculares, colera).

%% Buscar coincidencias de síntomas (Lógica de diagnóstico)
buscar([], _, 0).
buscar(X, E, 1) :- sintomade(X, E).
buscar([X|Xs], E, P) :-
    enfermedad(E),
    buscar(X, E, S1),
    buscar(Xs, E, S2),
    P is S1 + S2.

%% Contar cantidad total de síntomas de una enfermedad
cantSint(E, C) :-
    findall(X, sintomade(X, E), L),
    length(L, C).

%% Diagnóstico (porcentaje de coincidencia)
diagnostico(Sintomas, E, K) :-
    buscar(Sintomas, E, P),
    cantSint(E, T),
    T > 0, % Evita división por cero si la enfermedad no tiene síntomas definidos
    K is P * 100 / T.

%% Medicamentos por enfermedad
medicinade(contrex, gripe).
medicinade(jarabe, gripe).
medicinade(pastillas, tuberculosis).
medicinade(vacuna, malaria).
medicinade(vacuna, rubeola).
medicinade(vitaminas, anemia).
medicinade(pastillas, hepatitis).
medicinade(nistatina, candidiasis_oral).
medicinade(fluconazol, candidiasis_oral).
medicinade(clotrimazol, candidiasis_oral).
medicinade(cirugia, cancer_de_prostata).
medicinade(radioterapia, cancer_de_prostata).
medicinade(terapia_hormonal, cancer_de_prostata).
medicinade(quimioterapia, cancer_de_prostata).
medicinade(rehidratacion_oral, colera).
medicinade(antibioticos, colera).
medicinade(doxiciclina, colera).

%% Receta médica según síntoma
recetade(M, S) :-
    sintomade(S, Z),
    medicinade(M, Z).

%% Especialistas por enfermedad
especialistade(otorrino, gripe).
especialistade(nutricionista, anemia).
especialistade(endocrinologo, hepatitis).
especialistade(medicogeneral, rubeola).
especialistade(nutricionista, tuberculosis).
especialistade(medicogeneral, malaria).
especialistade(otorrino, candidiasis_oral).
especialistade(urologo, cancer_de_prostata).
especialistade(infectologo, colera).

%% Qué especialista atiende un síntoma
atiende_especialista(Esp, Sintoma) :-
    sintomade(Sintoma, Enf),
    especialistade(Esp, Enf).

%% Qué especialista y medicina corresponden a una enfermedad
mereceta(Esp, Med, Enf) :-
    medicinade(Med, Enf),
    sintomade(_, Enf),
    especialistade(Esp, Enf).

% --- PREDICADOS DE CONVERSACIÓN ELIZA ---

% Predicado principal: Inicia la conversación seleccionando un saludo aleatorio.
eliza:-
    % 1. Selecciona un saludo al azar de la lista
    greeting_message(Greetings),
    length(Greetings, MaxIndex),
    % Generar un índice aleatorio entre 0 y MaxIndex - 1
    MaxIndex > 0, % Asegurar que la lista no esté vacía
    RandomIndex is random(MaxIndex),
    nth0(RandomIndex, Greetings, Greeting),
    
    writeln(Greeting),
    writeln('por favor ingresa tu consulta, usar solo minúsculas sin . al final:'),
    readln(Input),
    eliza(Input),!.

% Regla de salida: Verifica si la entrada es 'Adios' o 'Adios .' y se despide.
eliza(Input):- 
    (Input == ['adios']; Input == ['adios', '.']), % Cambiado a minúsculas
    % 2. Selecciona una despedida al azar
    farewell_message(Farewells),
    length(Farewells, MaxIndex),
    MaxIndex > 0, % Asegurar que la lista no esté vacía
    RandomIndex is random(MaxIndex),
    nth0(RandomIndex, Farewells, FarewellMsg),
    writeln(FarewellMsg), 
    !.

% --- MENSAJES ALEATORIOS (20 SALUDOS) ---
greeting_message([
    'Hola, soy Eliza, tu chatbot. ¿En qué puedo ayudarte hoy?',
    'Saludos. Mi nombre es Eliza. Dime qué te trae por aquí.',
    '¡Hola! Soy Eliza. Estoy lista para conversar, ¿cuál es tu consulta?',
    'Buenas tardes. Soy Eliza, tu asistente virtual. ¿Qué necesitas?',
    'Hola. Soy Eliza, un gusto. Por favor, escribe tu pregunta.',
    'Bienvenido/a. Me llamo Eliza. ¿Cómo podemos empezar?',
    '¿Qué tal? Soy Eliza. Te escucho, recuerda usar minúsculas.',
    'Hola, mi nombre es Eliza. ¿Tienes alguna pregunta o comentario?',
    'Un placer. Soy Eliza. Adelante, dime lo que piensas.',
    'Saludos cordiales. Eliza a tu servicio. ¿Cuál es el tema de hoy?',
    'Hola, soy Eliza, tu compañera de chat. Inicia la conversación.',
    'Buenos días. Soy Eliza. ¿Cómo puedo hacer tu día más fácil?',
    'Hey, soy Eliza. No seas tímido, cuéntame qué buscas.',
    'Hola. Te habla Eliza. Recuerda usar minúsculas. ¿Cuál es tu consulta?',
    '¡Qué gusto! Me llamo Eliza. ¿Qué tema quieres explorar?',
    'Adelante. Soy Eliza. Estoy lista para el diálogo.',
    'Hola. Soy Eliza, tu chatbot. ¿En qué te puedo asesorar?',
    'Saludos. Mi nombre es Eliza. Dime qué tienes en mente.',
    'Hola de nuevo. Soy Eliza. ¿Cómo va todo? Ingresa tu pregunta.',
    'Eliza aquí. ¿Qué misterios vamos a desentrañar hoy?'
]).

% --- MENSAJES ALEATORIOS (20 DESPEDIDAS) ---
farewell_message([
    '¡Adiós! Espero que la conversación te haya sido útil.',
    'Hasta pronto. Fue un placer charlar contigo.',
    'Adiós. Si tienes más preguntas, vuelve cuando quieras.',
    '¡Nos vemos! Que tengas un excelente día.',
    'Chao. Gracias por tu tiempo. Cuídate.',
    'Me despido. Espero verte pronto de nuevo.',
    'Adiós. ¡Vuelve a consultarme si lo necesitas!',
    'Que te vaya bien. Gracias por conversar con Eliza.',
    'Hasta luego. Fue una interacción interesante.',
    'Adiós. ¡Recuerda seguir explorando el mundo de Prolog!',
    'Cierro sesión. Que tengas una buena jornada.',
    'Me retiro. ¡No dudes en llamarme de nuevo!',
    'Adiós, adiós. Todo listo por hoy.',
    'Finalizamos. Espero haber resuelto tus dudas.',
    'Nos despedimos. Ha sido un diálogo productivo.',
    'Hasta la próxima. Aquí estaré.',
    'Adiós. Éxito en tus próximos proyectos.',
    'Que descanses. ¡Vuelve a visitarme pronto!',
    'Fue un honor. Adiós y gracias.',
    'Desconexión. ¡Cuídate mucho!'
]).

% --- PREDICADO DE PROCESAMIENTO (eliza/1) ---

eliza(Input) :-
    template(Stim, Resp, IndStim),
    match(Stim, Input),
    % si he llegado aquí es que he
    % hallado el template correcto:
    replace0(IndStim, Input, 0, Resp, R),
    writeln(R),
    readln(Input1),
    eliza(Input1), !.

% --- TEMPLATES MÉDICOS EXPERTOS (NUEVOS) ---

% 1. Consulta de síntomas para diagnóstico (E.g. tengo tos y fiebre)
template([tengo, s(_), y, s(_)], [flagDiagnostico2], [1, 3]).
template([tengo, s(_)], [flagDiagnostico1], [1]).
template([sufro, de, s(_), y, s(_)], [flagDiagnostico2], [2, 4]).

% 2. Consulta de medicamentos para una enfermedad (E.g. medicina para la gripe)
template([medicina, para, la, s(_)], [flagMedicinaEnfermedad], [3]).
template([que, medicina, sirve, para, la, s(_)], [flagMedicinaEnfermedad], [5]).

% 3. Consulta de especialista para una enfermedad (E.g. especialista para la malaria)
template([especialista, para, la, s(_)], [flagEspecialistaEnfermedad], [3]).

% 4. Consulta de especialista para un síntoma (E.g. que doctor atiende la tos)
template([que, doctor, atiende, la, s(_)], [flagEspecialistaSintoma], [4]).
template([quien, atiende, la, s(_)], [flagEspecialistaSintoma], [3]).

% --- TEMPLATES GENERALES (EXISTENTES) ---
template([hola, mi, nombre, es, s(_)], ['Hola', 0, 'Como', estas, tu, '?'], [4]).
template([buendia, mi, nombre, es, s(_)], ['buen dia', 'Como', estas, tu, 0, '?'], [4]).

template([hola, ',', mi, nombre, es, s(_)], ['Hola', 0, 'Como', estas, tu, '?'], [5]).
template([buendia, ',', mi, nombre, es, s(_)], ['Buendia', 'Como', estas, tu, 0, '?'], [5]).

template([hola, _], ['Hola', 'como', estas, tu, '?'], []).
template([buendia, _], ['Buendia', 'Como', estas, tu, '?'], []).

template([yo, s(_), yo, soy, s(_)], [por, que, 0, eres, 1, '?'], [1, 4]).
template([yo, s(_), tu], [why, do, you, 0, me ,'?'], [1]).
template([yo, soy, s(_)], [porque, eres, tu, 0, '?'], [2]).

% pregunta algo que le gusta a eliza
template([te, gustan, las, s(_), _], [flagLike], [3]).
template([te, gustan, los, s(_), _], [flagLike], [3]).

% pregunta algo que hace eliza
template([tu, eres, s(_), _], [flagDo], [2]).
% pregunta algo que es eliza
template([que, eres, tu, s(_)], [flagIs], [2]).
template([eres, s(_), '?'], [flagIs], [2]).

template([como, estas, tu, '?'], [yo, estoy, bien, ',', gracias, por, preguntar], []).

template([yo, pienso, que, _], [bueno, esa, es, tu, opinion], []).
template([porque, _], [esa, no, es, una, buena, razon], []).
template([i, have, s(_), with, s(_)], ['You', have, to, deal, with, your, 0, and, your, 1, in, a, mature, way], [2, 4]).
template([i, s(_), _], [i, can, recommend, you, a, book, about, that, issue], []).
template([please, s(_), _], ['No', i, can, not, help, ',', i, am, just, a, machine], []). 
template([tell, me, a, s(_), _], ['No', i, can, not, ',', i, am, bad, at, that], []).
template(_, ['Please', explain, a, little, more], []). 

% --- GUSTOS DE ELIZA (10 items) ---
% Lo que le gusta a eliza : flagLike
elizaLikes(X, R):- likes(X), R = ['Yeah', i, like, X].
elizaLikes(X, R):- \+likes(X), R = ['Nope', i, do, not, like, X].
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

% --- LÓGICA DE RESPUESTA MÉDICA (Implementación de las flags) ---

% Flag: Diagnóstico con un síntoma (e.g., 'tengo tos')
elizaDiagnostico1(Sintoma, R) :-
    findall(E-K, (enfermedad(E), diagnostico([Sintoma], E, K), K >= 20), Diagnosticos),
    sort(2, @>=, Diagnosticos, SortedDiagnosticos),
    SortedDiagnosticos = [E1-K1 | Resto],
    format(atom(Msg), 'Basado en el sintoma ~w, tu diagnostico mas probable es ~w con un ~1f por ciento de coincidencia.', [Sintoma, E1, K1]),
    atom_chars(Msg, Chars),
    atomic_list_concat(R, ' ', Chars).

% Fallback para el diagnóstico con un síntoma
elizaDiagnostico1(Sintoma, R) :-
    \+ sintomade(Sintoma, _),
    R = ['Ese', sintoma, no, esta, registrado, en, mi, base, de, datos].

% Flag: Diagnóstico con dos síntomas (e.g., 'tengo tos y fiebre')
elizaDiagnostico2(Sintoma1, Sintoma2, R) :-
    findall(E-K, (enfermedad(E), diagnostico([Sintoma1, Sintoma2], E, K), K >= 20), Diagnosticos),
    sort(2, @>=, Diagnosticos, SortedDiagnosticos),
    SortedDiagnosticos = [E1-K1 | Resto],
    format(atom(Msg), 'Teniendo en cuenta ~w y ~w, el diagnostico mas probable es ~w con un ~1f por ciento de coincidencia.', [Sintoma1, Sintoma2, E1, K1]),
    atom_chars(Msg, Chars),
    atomic_list_concat(R, ' ', Chars).

% Fallback para el diagnóstico con dos síntomas
elizaDiagnostico2(Sintoma1, Sintoma2, R) :-
    \+ sintomade(Sintoma1, _),
    \+ sintomade(Sintoma2, _),
    R = ['Ni', 0, ni, 1, estan, registrados, en, mi, base, de, datos].

% Flag: Medicina para una enfermedad (e.g., 'medicina para la gripe')
elizaMedicina(Enfermedad, R) :-
    enfermedad(Enfermedad),
    findall(M, medicinade(M, Enfermedad), Medicinas),
    Medicinas \= [],
    atomic_list_concat(Medicinas, ', ', MedicinasStr),
    format(atom(Msg), 'Para la enfermedad ~w se recomienda: ~w', [Enfermedad, MedicinasStr]),
    atom_chars(Msg, Chars),
    atomic_list_concat(R, ' ', Chars).

% Fallback de Medicina
elizaMedicina(Enfermedad, R) :-
    \+ enfermedad(Enfermedad),
    R = ['La', enfermedad, 0, no, esta, en, mi, registro].
elizaMedicina(Enfermedad, R) :-
    enfermedad(Enfermedad),
    \+ medicinade(_, Enfermedad),
    R = ['No', tengo, registro, de, medicinas, para, 0].

% Flag: Especialista para una enfermedad (e.g., 'especialista para la tuberculosis')
elizaEspecialistaEnfermedad(Enfermedad, R) :-
    enfermedad(Enfermedad),
    especialistade(Esp, Enfermedad),
    format(atom(Msg), 'Para la enfermedad ~w debes consultar a un ~w', [Enfermedad, Esp]),
    atom_chars(Msg, Chars),
    atomic_list_concat(R, ' ', Chars).

% Fallback de Especialista (Enfermedad)
elizaEspecialistaEnfermedad(Enfermedad, R) :-
    \+ enfermedad(Enfermedad),
    R = ['La', enfermedad, 0, no, esta, en, mi, registro].

% Flag: Especialista para un síntoma (e.g., 'que doctor atiende la tos')
elizaEspecialistaSintoma(Sintoma, R) :-
    sintomade(Sintoma, Enf),
    especialistade(Esp, Enf),
    format(atom(Msg), 'El sintoma ~w esta asociado con ~w, que puede ser atendido por un ~w', [Sintoma, Enf, Esp]),
    atom_chars(Msg, Chars),
    atomic_list_concat(R, ' ', Chars).

% Fallback de Especialista (Síntoma)
elizaEspecialistaSintoma(Sintoma, R) :-
    \+ sintomade(Sintoma, _),
    R = ['Ese', sintoma, no, esta, asociado, a, ninguna, enfermedad, en, mi, base, de, datos].


% --- PREDICADOS AUXILIARES Y DE REEMPLAZO (Asegurando compatibilidad con nuevas flags) ---

match([],[]).
match([], _):- true.

match([S|Stim],[I|Input]) :-
    atom(S), % si I es un s(X) devuelve falso
    S == I,
    match(Stim, Input),!.

match([S|Stim],[_|Input]) :-
% I es un s(X), lo ignoro y continúo con el resto de la lista
    \+atom(S),
    match(Stim, Input),!.

replace0([], _, _, Resp, R):- append(Resp, [], R),!.

% Eliza medical: Diagnostico 1 sintoma
replace0([I|_], Input, _, Resp, R):-
    nth0(I, Input, Atom),
    nth0(0, Resp, X),
    X == flagDiagnostico1,
    elizaDiagnostico1(Atom, R).

% Eliza medical: Diagnostico 2 sintomas
replace0([I1, I2|_], Input, _, Resp, R):-
    nth0(I1, Input, Atom1),
    nth0(I2, Input, Atom2),
    nth0(0, Resp, X),
    X == flagDiagnostico2,
    elizaDiagnostico2(Atom1, Atom2, R).

% Eliza medical: Medicina por enfermedad
replace0([I|_], Input, _, Resp, R):-
    nth0(I, Input, Atom),
    nth0(0, Resp, X),
    X == flagMedicinaEnfermedad,
    elizaMedicina(Atom, R).

% Eliza medical: Especialista por enfermedad
replace0([I|_], Input, _, Resp, R):-
    nth0(I, Input, Atom),
    nth0(0, Resp, X),
    X == flagEspecialistaEnfermedad,
    elizaEspecialistaEnfermedad(Atom, R).

% Eliza medical: Especialista por sintoma
replace0([I|_], Input, _, Resp, R):-
    nth0(I, Input, Atom),
    nth0(0, Resp, X),
    X == flagEspecialistaSintoma,
    elizaEspecialistaSintoma(Atom, R).

% Eliza general: Likes
replace0([I|_], Input, _, Resp, R):-
    nth0(I, Input, Atom),
    nth0(0, Resp, X),
    X == flagLike,
    elizaLikes(Atom, R).

% Eliza general: Does
replace0([I|_], Input, _, Resp, R):-
    nth0(I, Input, Atom),
    nth0(0, Resp, X),
    X == flagDo,
    elizaDoes(Atom, R).

% Eliza general: Is
replace0([I|_], Input, _, Resp, R):-
    nth0(I, Input, Atom),
    nth0(0, Resp, X),
    X == flagIs,
    elizaIs(Atom, R).

% Reemplazo estándar (Cálculo de índices)
replace0([I|Index], Input, N, Resp, R):-
    length(Index, M), M =:= 0,
    nth0(I, Input, Atom),
    select(N, Resp, Atom, R1), append(R1, [], R),!.

replace0([I|Index], Input, N, Resp, R):-
    nth0(I, Input, Atom),
    length(Index, M), M > 0,
    select(N, Resp, Atom, R1),
    N1 is N + 1,
    replace0(Index, Input, N1, R1, R),!.