:- ensure_loaded('checker.pl').

%test_mode(detailed).

% Considerăm următoarele reprezentări:
%
% O integramă este reprezentată prin structura (compusul)
% integ(H, W, Lista, Vocab), unde:
% H este înălțimea integramei
% W este lățimea integramei
% Lista este o listă de tupluri (Poz, Valoare), unde
%   Poz este un tuplu (R, C) conținând rândul și coloana (0-based)
%   Valoare este una dintre:
%     x - dacă celula este neagră (nu poate fi completată cu litere)
%     o literă, dacă celula este completată cu o literă
%     o listă de întrebări, reprezentate ca tupluri (Text, Dir, ID), cu
%       Text - un srting, textul întrebării
%       Dir - una dintre valorile j sau d, indicând direcția întrebării
%       ID - un identificator numeric al întrebării
% Vocab este o listă de stringuri reprezentând cuvinte disponibile
% pentru a rezolva întrebarea.
%
% În ieșirea predicatului intrebări, o întrebare este reprezentată ca
% ((R, C), Text, Dir, ID), unde
% R este rândul căsuței cu întrebarea (0-based)
% C este coloana căsuței cu întrebarea (0-based)
% Text este textul întrebării (un string)
% Dir este j sau d, reprezentând direcția în care trebuie să fie plasat
% răspunsul (jos sau dreapta)
% ID este un identificator numeric al întrebării.

% Puteți vizualiza integramele cu:
% integrama(0, W), print_integrama(W).
% integrama(1, W), print_integrama(W).
% integrama(2, W), print_integrama(W).
% integrama(3, W), print_integrama(W).
%
% Testați cu
% vmtest.
% Testați teste individuale (vedeți predicatul tt din checker.pl) cu
% vmtest(Test).
% de exemplu cu vmtest(intrebari).


% intrebari/2
% intrebari(integ(+H, +W, +Lista, +Vocab), -Lista_intrebari)
% Este adevărat atunci când Lista_intrebari este o lista de tupluri
% ((R, C), Text, Dir, ID), fiecare tuplu corespunzând unei întrebări din
% integramă (rândul, coloana, textul întrebării, direcția (j/d),
% identificatorul).
% BONUS: intrebari are o singură soluție (o singură listă) pentru o
% anumită integramă.

intrebari(integ(_, _, Lista, _), Lista_intrebari) :-
	rezolva(Lista, Lista_intrebari).

make_lista(_, [], []).
make_lista(Pos, [(Text, Dir, ID) | Rest], Curr) :-
	make_lista(Pos, Rest, NewCurr),
	append([(Pos, Text, Dir, ID)], NewCurr, Curr).

rezolva([], []).
rezolva([(_, x) | Rest], Lista_intrebari) :-
	rezolva(Rest, Lista_intrebari).
rezolva([((R, C), Intrebari) | Rest], Lista_intrebari) :-
	is_list(Intrebari),
	rezolva(Rest, New_Lista_intrebari),
	make_lista((R, C), Intrebari, Solved),
	append(Solved, New_Lista_intrebari, Lista_intrebari).

% id_intrebare/2
% id_intrebare(+Integ, ?Intrebare, ?Q_ID)
% Este adevărat dacă în integrama reprezentată ca integ(...), Intrebare
% este un text iar Q_ID este un identificator care corespund aceleași
% întrebări.
id_intrebare(integ(_, _, Lista, _), Intrebare, Q_ID) :- 
	id_intrebare_all(Lista, Intrebare, Q_ID).

id_intrebare_all([], _, _) :- false.
id_intrebare_all([((_, _), Lista)|_], Intrebare, Q_ID) :-
	member((Intrebare, _, Q_ID), Lista),
	!.
id_intrebare_all([_|Rest], Intrebare, Q_ID) :-
	id_intrebare_all(Rest, Intrebare, Q_ID).

% completare/3
% completare(+Integ, +Sol, -Integrama)
% Predicatul produce Integrama, o structură de forma integ(...),
% pornind de la Integ, în care au fost completate celule conform cu
% soluția Sol.
% Soluția este reprezentată ca o listă de perechi (Întrebare, Răspuns),
% unde Întrebarea este textul unei întrebări, iar Răspuns este un cuvând
% de completat; ambele sunt stringuri.
% De exemplu, o soluție parțială pentru integrama 0 poate fi:
% [('Din care plouă', 'NOR'), ('Al doilea număr', 'DOI')]
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), solutie(0, Sol), completare(W, Sol, W2),
%   print_integrama(W2).
% completare(_, _, _).

completare(Integ, [], Integ).
completare(integ(H, W, Lista, Vocab), [(Intrebare, Raspuns) | Rest], Result) :-  
	find_intrebare(Lista, Intrebare, (R, C), Dir),
	atom_chars(Raspuns, Raspuns_List),
	put_sol((R, C), Dir, Raspuns_List, Answer_List),
	append(Answer_List, Lista, FinalList),
	sort(FinalList, NewList),
	completare(integ(H, W, NewList, Vocab), Rest, Result).

find_intrebare([(_, Lista_intrebari)| Rest], Intrebare, (R, C), Dir) :-
	\+ is_list(Lista_intrebari),
	find_intrebare(Rest, Intrebare, (R, C), Dir).
find_intrebare([((R, C), Lista_intrebari) | _], Intrebare, (R, C), Dir) :-
	is_list(Lista_intrebari),
	member((Intrebare, Dir, _), Lista_intrebari),
	!.
find_intrebare([(_, Lista_intrebari) | Rest], Intrebare, (X, Y), Dir) :-
	is_list(Lista_intrebari),
	\+ member((Intrebare, Dir, _), Lista_intrebari),
	find_intrebare(Rest, Intrebare, (X, Y), Dir).

put_sol(_, _, [], []).
put_sol((R, C), j, [Curr | Rest], List):-
	R1 is R + 1,
    put_sol((R1, C), j, Rest, RestList),
    append([((R1, C), Curr)], RestList, List).
put_sol((R, C), d, [Curr | Rest], List):-
	C1 is C + 1,
    put_sol((R, C1), d, Rest, RestList),
    append([((R, C1), Curr)], RestList, List).

% lungime_spatiu/3
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), +Intrebare, -Lungime)
% Returnează lungimea spațiului asociat întrebării date.
% Întrebarea este indicată prin textul ei. De exemplu:
% lungime_spatiu pentru integrama 0 și întrebarea 'Al doilea număr'
% trebuie să lege Lungime la 3.
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), id_intrebare(W, Text, 3), lungime_spatiu(W, Text, X).

lungime_spatiu(integ(_, _, Lista, _), Intrebare, Lungime) :-
	intrebari(integ(_, _, Lista, _), Lista_intrebari),
	member(((R, C), Intrebare, Dir, _), Lista_intrebari),
	calculate_length(Lista, (R, C), Dir, Lungime).

calculate_length(Lista, (R, C), j, Lungime) :-
	R1 is R + 1,
	\+ member(((R1, C), _), Lista),
	calculate_length(Lista, (R1, C), j, Lungime1),
	Lungime is Lungime1 + 1.

calculate_length(Lista, (R, C), j, Lungime) :-
	R1 is R + 1,
	member(((R1, C), _), Lista),
	Lungime is 0,
	!.

calculate_length(Lista, (R, C), d, Lungime) :-
	C1 is C + 1,
	\+ member(((R, C1), _), Lista),
	calculate_length(Lista, (R, C1), d, Lungime1),
	Lungime is Lungime1 + 1,
	!.

calculate_length(Lista, (R, C), d, Lungime) :-
	C1 is C + 1,
	member(((R, C1), _), Lista),
	Lungime is 0,
	!.

% intersectie/5
% intersectie(integ(+H, +W, +Lista, +Voc), +I1, -Poz1, +I2, -Poz2)
% Pentru o integramă și două întrebări date prin textul lor (I1 și I2),
% al căror răspunsuri se intersectează, întoarce în Poz1 indicele din
% răspunsul la I1 la care este intersecția, și în Poz2 indicele din
% răspunsul la I2 la care este intersecția. Indecșii incep de la 0.
%
% De exemplu, în integrama 0:
%  █       █       2↓      3↓      █
%  █       0↓,1→   -       -       █
%  4→      -       -       -       █
%  5→      -       -       -       █
%  █       █       █       █       █
%
%  Întrebările 'Primii 3 din artă' și 'Afirmativ' (3, respectiv 1) se
%  intersectează la pozițiile 0, respectiv 2 (va fi litera A, de la
%  ART, respectiv DA).
intersectie(integ(_, _, Lista, _), I1, Poz1, I2, Poz2) :-
	intrebari(integ(_, _, Lista, _), Lista_intrebari),
	member((_, I1, Dir1, _), Lista_intrebari),
	member((_, I2, Dir2, _), Lista_intrebari),
	Dir1 == Dir2,
	solve_intersectie(_, _, _, _, _, Dir1, Dir2, _, _, Poz1, Poz2).

intersectie(integ(_, _, Lista, _), I1, Poz1, I2, Poz2) :-
	intrebari(integ(_, _, Lista, _), Lista_intrebari),
	member(((R1, C1), I1, Dir1, _), Lista_intrebari),
	member(((R2, C2), I2, Dir2, _), Lista_intrebari),
	Dir1 == d, Dir2 == j,
	solve_intersectie(integ(_, _, Lista, _), R1, C1, R2, C2, Dir1, Dir2, I1, I2, Poz1, Poz2).

intersectie(integ(_, _, Lista, _), I1, Poz1, I2, Poz2) :-
	intrebari(integ(_, _, Lista, _), Lista_intrebari),
	member(((R1, C1), I1, Dir1, _), Lista_intrebari),
	member(((R2, C2), I2, Dir2, _), Lista_intrebari),
	Dir1 == j, Dir2 == d,
	solve_intersectie(integ(_, _, Lista, _), R2, C2, R1, C1, Dir2, Dir1, I2, I1, Poz2, Poz1).


solve_intersectie(_, _, _, _, _, d, d, _, _, _, _) :- false.
solve_intersectie(_, _, _, _, _, j, j, _, _, _, _) :- false.
solve_intersectie(W, Rd, Cd, Rj, Cj, d, j, Id, Ij, Pozd, Pozj) :-
	lungime_spatiu(W, Id, Lung1),
	lungime_spatiu(W, Ij, Lung2),
	\+ bad_conditions(Rd, Cd, Rj, Cj, Lung1, Lung2),
	AuxR is Rd - Rj - 1,
	AuxC is Cj - Cd - 1,
	Pozd is AuxC,
	Pozj is AuxR.


bad_conditions(R1, C1, R2, C2, Lung1, Lung2) :-
	R2aux is R2 + Lung2 + 1,
	C1aux is C1 + Lung1 + 1,
	( R1 > R2aux ; R2 >= R1 ; C2 >= C1aux; C1 >= C2 ).	


% solutii_posibile/2
% solutii_posibile(integ(+H, +W, +Lista, +Vocabular), -Solutii)
% Formează o listă Solutii, conținând perechi de forma
% (Întrebare, Cuvinte), unde
% Întrebare este textul unei întrebări din integramă, iar Cuvinte este o
% listă de cuvinte sunt din Vocabular și au lungimea corectă pentru a fi
% răspuns la întrebare. Solutii conține câte o pereche pentru fiecare
% întrebare din integramă.
% Cuvintele sunt reprezentate ca liste de stringuri, fiecare string
% având lungime 1 (o singură literă).
% De exemplu, pentru integrama 0, Solutii conține 6 perechi, două dintre
% ele fiind:
% ('Afirmativ', [['D', 'A'], ['N', 'U']])
% ('Din care plouă',
% [['N','O','R'],['A','R','T'],['U','I','T'],['D','O','I']])
convert_intrebari([], []).
convert_intrebari([ (_, Intrebare, _, _) | Rest], Intrebari) :-
	convert_intrebari(Rest, Rest_Intrebari),
	append([Intrebare], Rest_Intrebari, Intrebari).

head([], _) :- false.
head([H | _], H).

solutii_posibile(W, Solutii) :-
	intrebari(W, Lista_intrebari),
	convert_intrebari(Lista_intrebari, Intrebari),
	solutii2(W, Intrebari, Solutii).

solutii2(_, [], []).
solutii2(W, [Head | Rest], Solutii) :-
	solutii2(W, Rest, Rest_Solutii),
	lungime_spatiu(W, Head, Lungime),
	W = integ(_, _, _, Vocabular),
	findall(X, (member(X, Vocabular), string_length(X, Lungime)), Output),
	maplist(atom_chars, Output, Final_Output),
	append([(Head, Final_Output)], Rest_Solutii, Solutii).

% rezolvare/2
% rezolvare(+Integ, -Solutie)
% Rezolvare produce în Solutie soluția integramei Integ. Soluția este
% reprezentată ca o listă de perechi de stringuri, fiecare pereche
% conținând textul unei întrebări și cuvântul (ca string) care este
% răspunsul la întrebare.

rezolvare(W, Solutii) :-
    modificare_lista(W, Final_lista),
    genereaza_solutie(W, Final_lista, [], Solutii).

modificare_lista(W, Final_lista) :-
	solutii_posibile(W, Lista),
	adaug_la_lista(W, Lista, [], Aux1),
    sort_tuples_ascending(Aux1, Aux1_sortat),
    elimin_la_lista(Aux1_sortat, [], Final_lista).
   	% maplist(remove_helper, Aux1_sortat, Final_lista).
modificare_lista([], _) :- fail.

getter([], _, _) :- false.
getter([Curr | _], Pos, X) :-
	Pos == 0,
	X is Curr,
	!.
getter([_ | Rest], Pos, X) :-
	\+ (Pos == 0),
	NewPos is Pos - 1,
	getter(Rest, NewPos, X).

genereaza_solutie(W, [Pereche | Rest], Acc, Solutie) :-
    Pereche = (Intrebare, All),
    member(Raspuns, All),
    atom_chars(List_Raspuns, Raspuns),
    Pereche_Raspuns = (Intrebare, List_Raspuns),
    \+ member((_, List_Raspuns), Acc),
    \+ verifica_intersectie(W, Pereche_Raspuns, Acc),
    NewAcc = [Pereche_Raspuns | Acc],
    genereaza_solutie(W, Rest, NewAcc, Solutie).
genereaza_solutie(_, [], Acc, Acc).

verifica_intersectie(W, Pereche, Sol) :-
    Pereche = (Intrebare1, Raspuns1),
    atom_chars(Raspuns1, List1),
    member(Deja_sol, Sol),
    Deja_sol = (Intrebare2, Raspuns2),
    intersectie(W, Intrebare1, Pos1, Intrebare2, Pos2),
    nth0(Pos1, List1, X),
    atom_chars(Raspuns2, List2),
    nth0(Pos2, List2, Y),
    \+ (X == Y),
    !.
verifica_intersectie(_, _, []) :- fail.

elimin_la_lista([(_, Legit)| Rest], Acc, Solutie) :-
    NewAcc = [Legit | Acc],
    elimin_la_lista(Rest, NewAcc, Solutie).
elimin_la_lista([], Acc, Acc).

adaug_la_lista(_,[], Acc, Acc).
adaug_la_lista(W, [Head | Rest], Acc, Solutie) :-
    Head = (Intrebare, _),
    W = integ(_, _, Lista_intrebari, _),
    find_intrebare(Lista_intrebari, Intrebare, (R, C), _),
    Suma is R * C + (R + C),
    append([(Suma, Head)], Acc, NewAcc),
    adaug_la_lista(W, Rest, NewAcc, Solutie).

remove_helper([], _) :- false.
remove_helper((_, Legit1, Legit2), (Legit1, Legit2)).

compare_tuples_ascending('<', (X, _, _), (Y, _, _)) :- X < Y, !.
compare_tuples_ascending('>', _, _).

sort_tuples_ascending(Unsorted, Sorted) :-
    predsort(compare_tuples_ascending, Unsorted, Sorted).

% Credits for sorting function:
% https://stackoverflow.com/questions/47810970/sort-descending-order-in-a-list-of-tuple-in-prolog/47811774