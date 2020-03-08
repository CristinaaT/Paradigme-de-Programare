
:-ensure_loaded('probleme.pl').
:-ensure_loaded('testing.pl').

% getFields(+Context, -Days, -Times, -Rooms,
%           -Groups, -Staff, -Activities)
% Imparte contextul in field-urile din care e compus (ignora Constraints)
getFields(Context, Days, Times, Rooms, Groups, Staff, Activities) :-
	member(D, Context), D = days(Days),
	member(T, Context), T = times(Times),
	member(R, Context), R = rooms(Rooms),
	member(G, Context), G = groups(Groups),
	member(S, Context), S = staff(Staff),
	member(A, Context), A = activities(Activities), !.


% chooseActivity(+Activities, -Activity, -ActivitiesLeft)
% Alege o activitate din lista de activitati si creeaza o noua lista
% de activitati, cu numarul de aparitii ale activitatii alese mai mic cu 1
chooseActivity([(Activity, 1) | T], Activity, T).
chooseActivity([(Activity, N) | T], Activity, [(Activity, M) | T]) :-
	N \= 0, M is N - 1.


% generateActivityForEachGroup(+Activity, +Groups, +Days, +Times, +Rooms,
%                              +Staff, ?Acc, -Slots).
% Programeaza o activitate pentru toate grupele, pastrand constrangerile fizice
generateActivityForEachGroup(_, [], _, _, _, _, Acc, Acc).
generateActivityForEachGroup(Activity, [Group | Tail], Days, Times, Rooms,
                             Staff, Acc, Slots) :-
	member(D, Days), member(T, Times), member(R, Rooms),
	member((P, L), Staff), member(Activity, L), % Alegem profesorul
	\+ member(slot(_, Group, D, T, _, _), Acc), % Grupa nu are alta activitate
	\+ member(slot(_, _, D, T, R, _), Acc), % Sala nu mai e rezervata atunci
	\+ member(slot(_, _, D, T, _, P), Acc), % Profesorul nu mai are ore
	Slot = slot(Activity, Group, D, T, R, P), % Construim slotul
	generateActivityForEachGroup(Activity, Tail, Days, Times, Rooms, Staff,
	                             [Slot | Acc], Slots).


% generate(+Days, +Times, +Rooms, +Groups, +Staff, +Activities, ?Acc, -Slots)
% Genereaza toate posibilitatile de orare ce satisfac constrangerile "fizice"
generate(_, _, _, _, _, [], Acc, Acc).
generate(Days, Times, Rooms, Groups, Staff, Activities, Acc, Slots) :-
	nth0(0, Activities, _), % Fail, daca nu mai exista activitati
	chooseActivity(Activities, Activity, ActivitiesLeft), !, % Alegem materia
	generateActivityForEachGroup(Activity, Groups, Days, Times, Rooms, Staff,
	                             Acc, NewAcc),
	generate(Days, Times, Rooms, Groups, Staff, ActivitiesLeft,
	         NewAcc, Slots).


% schedule(+Context, -Sol)
% pentru contextul descris, întoarce o soluție care respectă constrângerile
% fizice
schedule(Context, (Context, Slots)) :-
	getFields(Context, Days, Times, Rooms, Groups, Staff, Activities),
	generate(Days, Times, Rooms, Groups, Staff, Activities, [], Slots).


% cost(+Sol, -Cost)
% pentru soluția dată, întoarce costul implicat de constrângerile de
% preferință care au fost încălcate.
cost(_, _) :- fail.


% schedule_best(+Context, -Sol, -Cost)
% pentru contextul descris, întoarce soluția validă cu cel mai bun (cel
% mai mic) cost (sau una dintre ele, dacă există mai multe cu același
% cost)
schedule_best(_, _, _) :- fail.













