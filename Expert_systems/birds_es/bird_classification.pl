:- op(800, fx, if).
:- op(700, xfx, then).
:- op(300, xfy, or).
:- op(200, xfy, and).

:- dynamic(derived/1).
:- dynamic(asked/1).

% Rules



% Askable
askable(wings).
askable(bill).
askable(live).
askable(feed).
askable(color).
askable(head).
askable(size).
askable(flight_profile).
askable(eats).

true(Prop) :- derived(Prop).
true(P1 and P2) :-
    true(P1),
    true(P2).

true(Prop) :-
    if Cond then Prop,
    true(Cond).

true(Prop) :-
    askable(Prop),
    \+derived(Prop),
    \+asked(Prop),
    ask(Prop).

ask(Prop) :-
    format("Describe ~w: ", [Prop]).
    read_line_to_string(user_input, Answer),
    split_string(Answer, " ", "", Values).
    process(Values, Prop).

check_answer([], _).
check_answer([Value | T], Prop) :-
    Check =.. [Prop, Value],
    Check,
    check_answer(T, Prop).

process([Value | T], Prop) :-
    Check =.. [Prop, Value],
    Check,
    asserta(derived(Check)),
    asserta(Prop), !.

process(_, Prop) :-
    Check =.. [Prop, Answer],
    Check, !,