%empieza base de conocimiento
%nombre, salud, escudo, energia, velocidad
warframe(ash, 455, 270, 100, 1.15).
warframe(atlas, 270, 270, 175, 0.9).
warframe(banshee, 270, 270, 175, 1.1).
warframe(baruuk, 180, 270, 200, 1.2).
warframe(caliban, 270, 550, 140, 1.1).
warframe(chroma, 270, 270, 175, 1).
warframe(citrine, 400, 270, 130, 1).
warframe(degath, 566, 150, 175, 1.1).
warframe(ember, 270, 270, 175, 1.1).
warframe(equinox, 270, 270, 175, 1.15).
warframe(excalibur, 270, 270, 100, 1).
warframe(frost, 270, 455, 100, 0.95).
warframe(gara, 270, 270, 175, 1.15).
warframe(garuda, 270, 270, 140, 1).
warframe(gauss, 270, 455, 175, 1.4).
warframe(grendel, 1095, 95, 175, 0.95).
warframe(gyre, 270, 550, 190, 1).
warframe(harrow, 270, 455, 100, 1).
warframe(hildryn, 180, 1280, 0, 1).
warframe(hydroid, 270, 365, 140, 1.05).
warframe(inaros, 2110, 0, 100, 1).
warframe(ivara, 180, 270, 215, 1.15).
warframe(khora, 365, 270, 140, 1.05).
warframe(kullervo, 1005, 0, 175, 1.1).
warframe(lavos, 540, 270, 0, 1.15).
warframe(limbo, 270, 180, 175, 1.15).
warframe(loki, 180, 180, 175, 1.25).
warframe(mag, 180, 455, 140, 1).
warframe(mesa, 365, 180, 100, 1.1).
warframe(mirage, 200, 200, 175, 1.2).
%faccion, salud, escudo, energia, velocidad
faccion(tenno, 1.05, 1.05, 1.05, 1.05).
faccion(grinner, 1, 1, 1.1, 1).
faccion(corpus, 1, 1.1, 1, 1).
faccion(infestacion, 0.95, 1.15, 1, 1).
faccion(orokin, 1, 1.1, 1, 1.1).
faccion(consciente, 1.15, 0.95, 1, 1).
faccion(stalker, 1, 1, 0.95, 1.15).
faccion(sindicato, 1, 1.15, 1, 0.95).
faccion(sinafiliacion, 1, 1, 1, 1).
%termina base de conocimiento
:- dynamic contador/1.
contador(0).

%inicia actualizar_faccion
actualizar_faccion(WF,F,S1,ES1,EN1,V1,WF2,F2,S2,ES2,EN2,V2) :-
    warframe(WF, S1b, ES1b, EN1b, V1b),
    faccion(F, FS1, FES1, FEN1, FV1),
	warframe(WF2, S2b, ES2b, EN2b, V2b),
    faccion(F2, FS2, FES2, FEN2, FV2),
    
    S1 is S1b * FS1,
    ES1 is ES1b * FES1,
    EN1 is EN1b * FEN1,
    V1 is V1b * FV1,
    
    S2 is S2b * FS2,
    ES2 is ES2b * FES2,
    EN2 is EN2b * FEN2,
    V2 is V2b * FV2.
%termina actualizar_faccion

%empieza actualizar_rango
actualizar_rango(WF, F, R1, S1, ES1, EN1, WF2, F2, R2, S2, ES2, EN2,T):-
    actualizar_faccion(WF, F, S1b, ES1b, EN1b, V1b, WF2, F2, S2b, ES2b, EN2b, V2b),
    
    (R2 @> 30 -> R2b is 30
    ;
   		(R2 @< 0 -> R2b is 1
    	;
     		R2b is R2
    	)
    ),

    (R1 @> 30 -> R1b is 30;
    	(R1 @< 0 -> R1b is 1
    	;
     		R1b is R1
    	)
    ),

    S1 is S1b + (S1b * 0.05 * R1b), 
    ES1 is ES1b + (ES1b * 0.05 * R1b), 
    EN1 is EN1b + (EN1b * 0.05 * R1b), 
    V1 is V1b + (V1b * 0.05 * R1b), 
    
    S2 is S2b + (S2b * 0.05 * R2b),
    ES2 is ES2b + (ES2b * 0.05 * R2b),
    EN2 is EN2b + (EN2b * 0.05 * R2b),
    V2 is V2b + (V2b * 0.05 * R2b),
	
	(V1 >= V2 -> T = 1
    ;
     V1 @< V2 -> T = 2
    ).
%termina actualizar_rango

%inicia batel
batel(WIN, TURN, S1, ES1, EN1, S2, ES2, EN2) :-
    (TURN =:= 1 ->
        (EN1 > ES2 -> S2b is S2 - (EN1 - ES2), ES2b is ES2
        ;   
            (EN1 < ES2 -> ES2c is ES2 - ((ES2 - EN1) * 1.5),S2b is S2,
            	(ES2c<0-> ES2b is 0; ES2b is ES2c)
            ;   
                EN1 =:= ES2 -> ES2b is ES2 * 0.85,S2b is S2
            )
        ),  
    
        (S2b @> 0 -> batel(WIN, 2, S1, ES1, EN1, S2b, ES2b, EN2)
        ;   
            WIN = 1
        )
        
    ;   
    	(EN2 > ES1 -> S1b is S1 - (EN2 - ES1), ES1b is ES1
        ;   
            (EN2 < ES1 -> ES1c is ES1 - ((ES1 - EN2) * 1.5),S1b is S1,
                (ES1c<0-> ES1b is 0; ES1b is ES1c)
            ;  
                EN2 =:= ES1 -> ES1b is ES1 * 0.85,S1b is S1
            )
        ),
        
        (S1b @> 0 -> batel(WIN, 1, S1b, ES1b, EN1, S2, ES2, EN2)
        ;   
            WIN = -1
        )
    ).
%termina batel

%inicia ganador
ganador(WIN):-
    (WIN == 1 ->
        write('Ganó el luchador uno'),nl
    ;
        write('Ganó el luchador dos'),nl
    ).
%termina ganador
    
%empieza inicio_batalla
inicio_batalla(WF1, F1, R1, WF2, F2, R2):-
    actualizar_rango(WF1, F1, R1, S1, ES1, EN1, WF2, F2, R2, S2, ES2, EN2,T), 
    batel(WIN, T, S1, ES1, EN1, S2, ES2, EN2),
    ganador(WIN).
%termina inicio_batalla
%Empieza la batalla de un warframe lista 1 contra los 5 de la lista 2
batalla2(_,_,_,[]).
batalla2(WF, F, R, [[WF1,F1,R1]|Equipo2]):-
    actualizar_rango(WF, F, R, S, ES, EN, WF1, F1, R1, S1, ES1, EN1, T),
    batel(WIN, T, S, ES, EN, S1, ES1, EN1),
    contador(C),
    A is C + WIN,
    retractall(contador(_)),
	assert(contador(A)),
    batalla2(WF, F, R,Equipo2).
%TERMINA la batalla de un warframe lista 1 contra los 5 de la lista 2

%Empieza la batalla de un warframe lista 1 contra los de la lista 2
batalla1([],_).
batalla1([[WF, F, R]|Equipo1], [[WF1,F1,R1]|Equipo2]) :-
	Lista = [[WF1,F1,R1]|Equipo2],
    batalla2(WF, F, R, Lista),
    batalla1(Equipo1, Lista).

inicio_equipo([[WF, F, R]|Equipo1], [[WF1,F1,R1]|Equipo2]):-
    L1=[[WF, F, R]|Equipo1],
    L2=[[WF1,F1,R1]|Equipo2],
    batalla1(L1,L2),
    contador(C),
    (C @> 0 ->
        write('Ganó el equipo uno'),nl
    ;
        write('Ganó el equipo dos'),nl
    ).
