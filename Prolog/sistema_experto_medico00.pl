%% Declaraciones de enfermedades
enfermedad(gripe).
enfermedad(rubeola).
enfermedad(malaria).
enfermedad(hepatitis).
enfermedad(tuberculosis).
enfermedad(anemia).

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

%% Buscar coincidencias de síntomas
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
    K is P * 100 / T.

%% Medicamentos por enfermedad
medicinade(contrex, gripe).
medicinade(jarabe, gripe).
medicinade(pastillas, tuberculosis).
medicinade(vacuna, malaria).
medicinade(vacuna, rubeola).
medicinade(vitaminas, anemia).
medicinade(pastillas, hepatitis).

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

%% Qué especialista atiende un síntoma
atiende_especialista(Esp, Sintoma) :-
    sintomade(Sintoma, Enf),
    especialistade(Esp, Enf).

%% Qué especialista y medicina corresponden a una enfermedad
mereceta(Esp, Med, Enf) :-
    medicinade(Med, Enf),
    sintomade(_, Enf),
    especialistade(Esp, Enf).
