isa(bird, animal).
isa(albatross, bird).
isa(kiwi, bird).
isa('Albert', albatross).
isa('Ross', albatross).
isa('Kim', kiwi).

active(bird, daylight).
active(kiwi, night).

active(Species, Time) :-
    isa(Species, Super),  
    active(Super, Time).

% This is bad 'cause it depends on order, and we can forget to "add :- !"
moving(bird, fly) :- !.
moving(kiwi, walk) :- !.

moving(Species, Time) :-
    isa(Species, Super),  
    moving(Super, Time).

color(albatross, 'B&W').
color(kiwi, 'brown').