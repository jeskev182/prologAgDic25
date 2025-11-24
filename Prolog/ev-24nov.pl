% La solución es una lista de 5 investigadores (I1 a I5).
solucion(ListaInvestigadores) :-
    % definicion de todos los conjuntos de atributos
    Nombres = [ana, bruno, carlos, diana, elisa],
    Especialidades = [genetica, microbiologia, bioquimica, inmunologia, neurociencia],
    Horarios = [6, 8, 10, 12, 14], % 14 es 2pm
    Bebidas = [cafe, te, jugo, mate, agua],
    Equipos = [microscopio, centrifuga, pcr, espectrometro, incubadora],
    Paises = [mexico, chile, espana, argentina, peru],

    resolver(Nombres, Especialidades, Horarios, Bebidas, Equipos, Paises),

    % El predicado resolver/6 devolverá 5 estructuras de investigador.
    ListaInvestigadores = [
        investigador(Ana, Esp_Ana, Hor_Ana, Beb_Ana, Eq_Ana, Pais_Ana),
        investigador(Bruno, Esp_Bruno, Hor_Bruno, Beb_Bruno, Eq_Bruno, Pais_Bruno),
        investigador(Carlos, Esp_Carlos, Hor_Carlos, Beb_Carlos, Eq_Carlos, Pais_Carlos),
        investigador(Diana, Esp_Diana, Hor_Diana, Beb_Diana, Eq_Diana, Pais_Diana),
        investigador(Elisa, Esp_Elisa, Hor_Elisa, Beb_Elisa, Eq_Elisa, Pais_Elisa)
    ],

    todos_distintos(ListaInvestigadores), 

    Carlos = carlos, Diana = diana, Eq_Carlos = espectrometro, Esp_Diana = microbiologia,

    % Buscar la combinacion en la lista Final
    
    % Reglas de 2 atributos
    member(investigador(_, genetica, 6, _, _, _), ListaInvestigadores),           
    member(investigador(_, _, 10, _, _, peru), ListaInvestigadores),                 
    member(investigador(_, _, _, mate, _, argentina), ListaInvestigadores),         
    member(investigador(_, _, 8, _, microscopio, _), ListaInvestigadores),          
    member(investigador(_, neurociencia, _, _, _, espana), ListaInvestigadores),     

    % Reglas de 3 o mas atributos
    member(investigador(_, _, _, te, centrifuga, _), ListaInvestigadores),            
    member(investigador(_, inmunologia, Hor_PCR, pcr, _, _), ListaInvestigadores),    
    member(investigador(_, bioquimica, _, _, _, chile), ListaInvestigadores),        
    member(investigador(_, _, 14, _, incubadora, _), ListaInvestigadores),           

    % C. Reglas de Restricción/Negación
    Ana \= ana, Esp_Ana \= genetica, Esp_Ana \= neurociencia,                  
    Beb_Elisa \= te, Beb_Elisa \= cafe,                                        
    Bruno \= bruno, Pais_Bruno \= mexico,                                            % bruno no es de Mexico.
    
    % D. Reglas con Relación Temporal (necesitan la búsqueda de horarios)
    member(investigador(_, _, Hor_Cafe, cafe, _, _), ListaInvestigadores),
    member(investigador(_, _, Hor_Jugo, jugo, _, _), ListaInvestigadores),
    Hor_Jugo is Hor_Cafe + 2,                                                        % caf llega 2h antes que Jugo.

    member(investigador(_, microbiologia, Hor_Micro, _, _, _), ListaInvestigadores),
    Hor_PCR > Hor_Micro,                                                             % Pcr llega despus de Microbiologia.

    member(investigador(_, neurociencia, Hor_Neuro, _, _, _), ListaInvestigadores),
    Hor_Neuro > Hor_Jugo,                                                            % 19. Neurociencia llega despurs de Jugo.

    % E. Reglas que combinan atributos y negación
    member(investigador(_, _, _, _, Eq_Mexico, mexico), ListaInvestigadores),
    Eq_Mexico \= microscopio, Eq_Mexico \= incubadora,                                % 17. Mexico no usa Microscopio/Incubadora.

    member(investigador(_, _, _, agua, Eq_Agua, _), ListaInvestigadores),
    Eq_Agua \= pcr, Eq_Agua \= espectrometro,                                        % 18. Agua no usa PCR/Espectrometro.
    
    member(investigador(_, _, _, Beb_Peru, _, peru), ListaInvestigadores),
    Beb_Peru \= agua.                                                                % 20. peru no bebe agua



% resolver(Nombres, Especialidades, Horarios, Bebidas, Equipos, Paises):
% Genera las 5 estructuras de investigador a través de la unificación.
resolver([], [], [], [], [], []).

resolver([Nombre|RN], [Esp|RE], [Hor|RH], [Bebida|RB], [Equipo|RQ], [Pais|RP]) :-
    % Genera la estructura de investigador:
    investigador(Nombre, Esp, Hor, Bebida, Equipo, Pais), 
    
    % Recursivamente, asigna el resto de los atributos
    resolver(RN, RE, RH, RB, RQ, RP).


% a|segurar que todos los atributos distintos


todos_distintos(L) :-
    separa_atributos(L, Nombres, Especialidades, Horarios, Bebidas, Equipos, Paises),

    son_distintos(Nombres), 
    son_distintos(Especialidades),
    son_distintos(Horarios),
    son_distintos(Bebidas),
    son_distintos(Equipos),
    son_distintos(Paises).

% forza que todos los elementos de una lista sean únicos.
son_distintos([]).
son_distintos([H|T]) :-
    \+ member(H, T), % Asegura que el elemento principal (H) no esté en el resto de la lista (T).
    son_distintos(T).

% separar la lista de estructuras de investigador en 6 listas de atributos.
separa_atributos([], [], [], [], [], [], []).
separa_atributos([investigador(N, E, H, B, Q, P)|T], [N|TN], [E|TE], [H|TH], [B|TB], [Q|TQ], [P|TP]) :-
    separa_atributos(T, TN, TE, TH, TB, TQ, TP).