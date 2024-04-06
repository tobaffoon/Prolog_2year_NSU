% decomposition f(a,b) =.. [f,a,b].

isa(bird, animal).
isa(albatross, bird).
isa(kiwi, bird).
isa('Albert', albatross).
isa('Ross', albatross).
isa('Kim', kiwi).

active(bird, daylight).
active(kiwi, night).

moving(bird, fly).
moving(kiwi, walk).

color(albatross, 'B&W').
color(kiwi, 'brown').

info(Predicate, Obj, Value) :-
    Request =.. [Predicate, Obj, Value],
    (
        Request, !;
        (
            isa(Obj, Super),
            info(Predicate, Super, Value)
        )
    ).

    