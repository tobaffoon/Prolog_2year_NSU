expert("Bioinformatics", "Barbara Green").
expert("Bioinformatics", "Ben Smith").
expert("AI", "Adam Walker").
expert("AI", "Barbara Green").
expert("AI", "Anna Crawley").
expert("Database", "Dan Rogers").
expert("Database", "Adam Walker").

session_time(morning).
session_time(afternoon).

session(Time, Topic, E1, E2) :-
    session_time(Time),
    expert(Topic, E1),
    expert(Topic, E2),
    dif(E1, E2).

no_overlap(Tm1, _, _, Tm2, _, _) :- dif(Tm1, Tm2).
no_overlap(T, E11, E12, T, E21, E22) :-
    dif(E11, E21), dif(E12, E21),
    dif(E11, E22), dif(E12, E22).

schedule_meeteing(
    s(Tm1, Tp1, E11, E12),
    s(Tm2, Tp2, E21, E22),
    s(Tm3, Tp3, E31, E32)
) :-
    session(Tm1, Tp1, E11, E12),
    session(Tm2, Tp2, E21, E22),
    session(Tm3, Tp3, E31, E32),
    dif(Tp1, Tp2), dif(Tp1, Tp3), dif(Tp3, Tp2),
    no_overlap(Tm1, E11, E12, Tm2, E21, E22),
    no_overlap(Tm1, E11, E12, Tm3, E31, E32),
    no_overlap(Tm3, E31, E32, Tm2, E21, E22).