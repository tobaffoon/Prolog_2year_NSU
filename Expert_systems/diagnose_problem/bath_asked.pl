% Backward infer - from facts to result

:- op(800, fx, if).
:- op(700, xfx, then).
:- op(300, xfy, or).
:- op(200, xfy, and).
:- op(800, xfx, <==).

:- dynamic(derived/1).
:- dynamic(asked/1).

% Rules
if 
    kitchen_dry and hall_wet
then
    leak_in_bathroom.

if
    hall_wet and bathroom_dry
then 
    problem_in_kitchen.

if
    window_closed or no_rain
then 
    no_water_from_outside.

if 
    problem_in_kitchen and no_water_from_outside
then 
    leak_in_kitchen.

askable(hall_wet).
askable(kitchen_dry).
askable(bathroom_dry).
askable(window_closed).
askable(no_rain).

true(Prop, Prop, _) :- derived(Prop).
true(P1 and P2, T1 and T2, Trace) :-
    true(P1, T1, Trace),
    true(P2, T2, Trace).
true(P1 or P2, T, Trace) :-
    true(P1, T, Trace);
    true(P2, T, Trace).

true(Prop, Prop <== CondTree, Trace) :-
    if Cond then Prop,
    true(Cond, CondTree, [if Cond then Prop | Trace]).
true(Prop, Proof, Trace) :-
    askable(Prop),
    \+derived(Prop),
    \+asked(Prop),
    ask(Prop, Proof, Trace).

ask(Prop, Proof, Trace) :-
    format('\nIs it true that ~w? Please answer: \'yes\',\'no\',\'why\'.\n', [Prop]),
    read_string(user_input, "\n", "\r\t", _, Answer),
    process(Answer, Prop, Proof, Trace).

process("yes", P, P <== was_told, _) :- !,
    asserta(derived(P)),
    asserta(asked(P)).

process("no", P, _, _) :- !,
    asserta(asked(P)),
    fail.
    
process("why", P, Proof, Trace) :- !,
    show_reasoning_chain(Trace, 0),
    ask(P, Proof, Trace).

process(_, P, Proof, Trace) :-
    format('\nPlease answer: \'yes\',\'no\',\'why\'.\n'),
    read_string(user_input, "\n", "\r\t", _, Answer),
    process(Answer, P, Proof, Trace).

show_reasoning_chain([], _).
show_reasoning_chain([if Cond then Concl | Re], Indent) :-
    tab(Indent),
    format("\nTo derive that ~w, using rule `if ~w then ~w`", [Concl, Cond, Concl]),
    NextIndent is Indent + 1,
    show_reasoning_chain(Re, NextIndent).

true(What, How) :-
    retractall(derived(_)),
    retractall(asked(_)),
    true(What, How, []).