%% --- Declaraciones de enfermedades ---
enfermedad(gripe).
enfermedad(rubeola).
enfermedad(malaria).
enfermedad(hepatitis).
enfermedad(tuberculosis).
enfermedad(anemia).
enfermedad(candidiasis_oral).     
enfermedad(cancer_de_prostata).    
enfermedad(colera).               

%% --- Declaraciones de síntomas según enfermedad ---
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

%% Síntomas de Candidiasis oral (Añadidos)
sintomade(placas_blancas, candidiasis_oral).
sintomade(ardor_bucal, candidiasis_oral).
sintomade(dolor_al_tragar, candidiasis_oral).
sintomade(sangrado_leve, candidiasis_oral).
sintomade(agrietamiento_comisuras_boca, candidiasis_oral).

%% Síntomas de Cáncer de próstata (Añadidos)
sintomade(dificultad_para_orinar, cancer_de_prostata).
sintomade(miccion_frecuente_nocturna, cancer_de_prostata).
sintomade(flujo_de_orina_debil, cancer_de_prostata).
sintomade(sangre_en_la_orina, cancer_de_prostata).
sintomade(dolor_huesos, cancer_de_prostata).

%% Síntomas de Cólera (Añadidos)
sintomade(diarrea_acuosa_repentina, colera).
sintomade(vomitos, colera).
sintomade(deshidratacion_severa, colera).
sintomade(sed_extrema, colera).
sintomade(calambres_musculares, colera).

%% --- Lógica de Diagnóstico (Sin cambios) ---
buscar([], _, 0).
buscar(X, E, 1) :- sintomade(X, E).
buscar([X|Xs], E, P) :-
    enfermedad(E),
    buscar(X, E, S1),
    buscar(Xs, E, S2),
    P is S1 + S2.

cantSint(E, C) :-
    findall(X, sintomade(X, E), L),
    length(L, C).

diagnostico(Sintomas, E, K) :-
    buscar(Sintomas, E, P),
    cantSint(E, T),
    T > 0, %% Asegurarse de no dividir por cero
    K is P * 100 / T.

%% --- Medicamentos por enfermedad ---
medicinade(contrex, gripe).
medicinade(jarabe, gripe).
medicinade(pastillas, tuberculosis).
medicinade(vacuna, malaria).
medicinade(vacuna, rubeola).
medicinade(vitaminas, anemia).
medicinade(pastillas, hepatitis).

%% Medicamentos de Candidiasis oral (Añadidos)
medicinade(nistatina, candidiasis_oral).
medicinade(fluconazol, candidiasis_oral).
medicinade(clotrimazol, candidiasis_oral).

%% Medicamentos de Cáncer de próstata (Añadidos)
medicinade(cirugia, cancer_de_prostata).
medicinade(radioterapia, cancer_de_prostata).
medicinade(terapia_hormonal, cancer_de_prostata).
medicinade(quimioterapia, cancer_de_prostata).

%% Medicamentos de Cólera (Añadidos)
medicinade(rehidratacion_oral, colera).
medicinade(antibioticos, colera).
medicinade(doxiciclina, colera).

%% Receta médica según síntoma (Sin cambios)
recetade(M, S) :-
    sintomade(S, Z),
    medicinade(M, Z).

%% --- Especialistas por enfermedad ---
especialistade(otorrino, gripe).
especialistade(nutricionista, anemia).
especialistade(endocrinologo, hepatitis).
especialistade(medicogeneral, rubeola).
especialistade(nutricionista, tuberculosis).
especialistade(medicogeneral, malaria).

%% Especialistas de las nuevas enfermedades (Añadidos)
especialistade(otorrino, candidiasis_oral).    %% Puede ser un otorrinolaringólogo o estomatólogo
especialistade(urologo, cancer_de_prostata).   %% Urólogo
especialistade(infectologo, colera).           %% Infectólogo

%% Qué especialista atiende un síntoma (Sin cambios)
atiende_especialista(Esp, Sintoma) :-
    sintomade(Sintoma, Enf),
    especialistade(Esp, Enf).

%% Qué especialista y medicina corresponden a una enfermedad (Sin cambios)
mereceta(Esp, Med, Enf) :-
    medicinade(Med, Enf),
    sintomade(_, Enf),
    especialistade(Esp, Enf).