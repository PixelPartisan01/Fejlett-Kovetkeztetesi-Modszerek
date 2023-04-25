tortenet(tortenetek(X,_,_,_,_), 1, X).
tortenet(tortenetek(_,X,_,_,_), 2, X).
tortenet(tortenetek(_,_,X,_,_), 3, X).
tortenet(tortenetek(_,_,_,X,_), 4, X).
tortenet(tortenetek(_,_,_,_,X), 5, X).

nap(t(X,_,_,_), X).
szerkeszto(t(_,X,_,_), X).
konyvcim(t(_,_,X,_), X).
ar(t(_,_,_,X), X).

szerkesztok(abafi_bea, no).
szerkesztok(palos_kata, no).
szerkesztok(dalos_rezso, ferfi).
szerkesztok(gemes_imre, ferfi).
szerkesztok(joszkin_ede, ferfi).

arak(1500).
arak(1700).
arak(2000).
arak(2200).
arak(2350).

cimek(csengo).
cimek(dobos_torta).
cimek(marvanyfejek).
cimek(tul_mindenen).
cimek(zene_bona).

napok(hetfo, 1).
napok(kedd, 2).
napok(szerda, 3).
napok(csutortok, 4).
napok(pentek, 5).

index(R) :- napok(_C, R).

test([]) :- true.
test([H|T]) :- member(H, T), false.
test([H|T]) :- not(member(H, T)), test(T).


%Program futtatása ?- megold(M).
%A programnak nagyjából 1,5 - 2 percig tartott míg lefutott a gépemen.

% Csak így tudtam megoldani, hogy egy megoldást csak egyszer kapjunk
% meg.
megold(M) :- setof(_X,_Y^solve(M),_).

solve(M) :-

    %1. Sem a hétfõn boltokba került könyvet, sem a Csengõ címû önéletrajzot nem nõi szerkesztõ dolgozta át.
    tortenet(M,AI,A), AI = 1, nap(A, ANAP), napok(ANAP, AI), szerkeszto(A, ASZERK), szerkesztok(ASZERK, ferfi), konyvcim(A, ACIM), cimek(ACIM), ACIM \= csengo ,ar(A,AAR), arak(AAR),
    tortenet(M,BI,B), index(BI), nap(B, BNAP), napok(BNAP, BI), konyvcim(B, BCIM), BCIM = csengo, szerkeszto(B, BSZERK), szerkesztok(BSZERK, ferfi), ar(B, BAR), arak(BAR),

    %2. Mind a két elõbb említett mû kevesebbe kerül, mint az Abafi Bea által átszerkesztett könyv, ami egy nappal a Joszkin Ede keze munkáját dicsérõ kiadvány elõtt jelent meg.
    tortenet(M,CI,C), index(CI),  nap(C, CNAP), napok(CNAP, CI), szerkeszto(C,CSZERK), CSZERK = abafi_bea, konyvcim(C, CCIM), cimek(CCIM), ar(C, CAR), arak(CAR), CAR > AAR, CAR > BAR,
    tortenet(M,DI,D), index(DI), nap(D, DNAP), napok(DNAP, DI), DI is  CI + 1, DI < 6,  szerkeszto(D, DSZERK), DSZERK = joszkin_ede, konyvcim(D, DCIM), cimek(DCIM), ar(D, DAR), arak(DAR),

    %3. A Túl Mindenen drágább, mint Dalos Rezsõ szerkesztése.
    %tortenet(M,_,C), konyvcim(C, 'Túl Mindenen'), %ar(C, DRAGA), dragab(DRAGA, [1500, 1700, 2000, 2200, 2350]),
    tortenet(M,EI,E), index(EI), nap(E,ENAP), napok(ENAP, EI), szerkeszto(E,ESZERK), szerkesztok(ESZERK, _), konyvcim(E, ECIM), ECIM = tul_mindenen, ar(E,EAR), arak(EAR),
    tortenet(M,E2I,E2), index(E2I), nap(E2,E2NAP), napok(E2NAP, E2I), szerkeszto(E2, E2SZERK), szerkesztok(E2SZERK, ferfi), E2SZERK = dalos_rezso, konyvcim(E2, E2CIM), cimek(E2CIM), ar(E2, E2AR), arak(E2AR), E2AR < EAR, E2AR >= 1500 ,

    %4. A Márványfejeket csütörtök óta lehet kapni.
    tortenet(M,FI,F), index(FI), nap(F,FNAP), napok(FNAP, FI), FNAP = csutortok, szerkeszto(F, FSZERK), szerkesztok(FSZERK, _), konyvcim(F, FCIM), FCIM = marvanyfejek, ar(F, FAR), arak(FAR),

    %5. Az 1700 forintos önéletrajzot Pálos Kata öntötte végsõ formába; ez a mû már a Márványfejek elõtt a boltokban volt.
    tortenet(M,GI,G), index(GI), GI < 4, nap(G, GNAP), napok(GNAP, GI), szerkeszto(G, GSZERK), GSZERK = palos_kata, konyvcim(G, GCIM), cimek(GCIM), ar(G,GAR), GAR = 1700,

    %6. A szerdai megjelenésû könyv volt az öt közül a legolcsóbb.
    tortenet(M,HI,H), index(HI), nap(H, HNAP), nap(H, HNAP), HNAP = szerda, szerkeszto(H, HSZERK), szerkesztok(HSZERK, _), konyvcim(H, HCIM), cimek(HCIM), ar(H, HAR), HAR = 1500,

    %7. A Dobos Torta ára 2350 forint.
    tortenet(M,II,I), index(II), nap(I,INAP), napok(INAP, II), szerkeszto(I, ISZERK), szerkesztok(ISZERK, _), konyvcim(I, ICIM), ICIM = dobos_torta, ar(I, IAR), IAR = 2350,

    %8. Gémes Imre a rock-sztár kéziratát szerkesztette át.
    %A feleadat szövegébõl számomra nem derül ki egyértelmûen, hogy a kézirat melyik címet  viseli az öt lehetségesbõl.

    tortenet(M,JI,J), index(JI), nap(J, JNAP), napok(JNAP, JI), szerkeszto(J, JSZERK) , JSZERK = gemes_imre, konyvcim(J, JCIM), cimek(JCIM), ar(J, JAR), arak(JAR),

    %------TESZT------%

    tortenet(M,_,T1), tortenet(M,_,T2), tortenet(M,_,T3), tortenet(M,_,T4), tortenet(M,_,T5),
    %--- Nincs két egyforma megjelenési nap ---%
    nap(T1,T1NAP), napok(T1NAP,_), nap(T2,T2NAP), napok(T2NAP,_), nap(T3,T3NAP), napok(T3NAP,_), nap(T4,T4NAP), napok(T4NAP,_), nap(T5,T5NAP), napok(T5NAP,_), test([T1NAP,T2NAP,T3NAP,T4NAP,T5NAP]),

    %--- Nincs két egyforma szerkesztõ ---%
    szerkeszto(T1, T1SZERK), szerkesztok(T1SZERK, _) ,szerkeszto(T2, T2SZERK), szerkesztok(T2SZERK, _), szerkeszto(T3, T3SZERK), szerkesztok(T3SZERK, _), szerkeszto(T4, T4SZERK), szerkesztok(T4SZERK, _), szerkeszto(T5, T5SZERK), szerkesztok(T5SZERK, _), test([T1SZERK, T2SZERK, T3SZERK, T4SZERK, T5SZERK]),

    %--- Nincs két egyforma cím ---%
    konyvcim(T1, T1CIM), cimek(T1CIM), konyvcim(T2, T2CIM), cimek(T2CIM), konyvcim(T3, T3CIM), cimek(T3CIM), konyvcim(T4, T4CIM), cimek(T4CIM), konyvcim(T5, T5CIM), cimek(T5CIM), test([T1CIM, T2CIM, T3CIM, T4CIM, T5CIM]),

     %--- Nincs két egyforma ár ---%
    ar(T1,T1AR), arak(T1AR), ar(T2,T2AR), arak(T2AR), ar(T3,T3AR), arak(T3AR), ar(T4,T4AR), arak(T4AR), ar(T5,T5AR), arak(T5AR),test([T1AR,T2AR,T3AR,T4AR,T5AR]).

    %------MEGOLDÁS--------%

    %2 megoldás lesz. Valószínûleg a 8. pont miatt.

    %M = tortenetek(t(hetfo, gemes_imre, tul_mindenen, 2000), t(kedd, palos_kata, zene_bona, 1700), t(szerda, dalos_rezso, csengo, 1500), t(csutortok, abafi_bea, marvanyfejek, 2200), t(pentek, joszkin_ede, dobos_torta, 2350))

    %M = tortenetek(t(hetfo, gemes_imre, zene_bona, 2000), t(kedd, palos_kata, tul_mindenen, 1700), t(szerda, dalos_rezso, csengo, 1500), t(csutortok, abafi_bea, marvanyfejek, 2200), t(pentek, joszkin_ede, dobos_torta, 2350)).
