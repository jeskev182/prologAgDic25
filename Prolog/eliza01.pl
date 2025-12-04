% --- PREDICADOS DE INICIO Y FIN ---

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
    (Input == ['Adios']; Input == ['Adios', '.']),
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

% --- TEMPLATES ---
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
template([i, s(_),  _], [i, can, recommend, you, a, book, about, that, issue], []).
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
likes(carros). % Corregido el error tipográfico 'like' por 'likes'
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

% --- PREDICADOS AUXILIARES ---

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

% Eliza likes:
replace0([I|_], Input, _, Resp, R):-
    nth0(I, Input, Atom),
    nth0(0, Resp, X),
    X == flagLike,
    elizaLikes(Atom, R).

% Eliza does:
replace0([I|_], Input, _, Resp, R):-
    nth0(I, Input, Atom),
    nth0(0, Resp, X),
    X == flagDo,
    elizaDoes(Atom, R).

% Eliza is:
replace0([I|_], Input, _, Resp, R):-
    nth0(I, Input, Atom),
    nth0(0, Resp, X),
    X == flagIs,
    elizaIs(Atom, R).

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