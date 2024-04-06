% Backward infer - from facts to result

:- op(800, fx, if).
:- op(700, xfx, then).
:- op(300, xfy, or).
:- op(200, xfy, and).

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

% Data
known(hall_wet).
known(bathroom_dry).
known(window_closed).

% Inference engine
true(Proposition) :- known(Proposition).
true(P1 and P2) :- true(P1), true(P2).
true(P1 or P2) :- true(P1); true(P2).

true(Prod) :-
    if Cond then Prod,
    true(Cond).

:- dynamic derived/1.
:- dynamic rejected/1.

fact(Proposition) :- derived(Proposition).
fact(P1 and P2) :- fact(P1), fact(P2).
fact(P1 or P2) :- fact(P1); fact(P2).

% does it mean that Prop maybe can be inferred?
supposed(Prop) :-
    derived(Gt),
    (
        Cond = Gt;
        Cond = _ and Gt;
        Cond = Gt and _;
        Cond = Gt or _;
        Cond = _ or Gt
    ),
    if Cond then Prop,
    \+ derived(Prop),
    \+ rejected(Prop).


hc_interface :-
    supposed(Prop),
    !,
    (
        true(Prop) ->
        format('Infered that ~w is true;\n', [Prop]),
        assertz(derived(Prop))
        ;
        assertz(rejected(Prop))
    ),
    hc_interface,
    write('--------------------------------').

hc :-
    retractall(derived(_)),
    retractall(rejected(_)),
    asserta(derived(P) :- known(P)),
    hc_interface.

derivable(Prod) :-
    if Cond then Prod,
    \+ derived(Prod),
    fact(Cond).

fc_interface :-
    derivable(Proposition),
    !,
    format('Infered that ~w is true;\n', [Proposition]),
    assertz(derived(Proposition)),
    fc_interface;
    write('--------------------------------').

fc :-
    retractall(derived(_)),
    assertz(derived(P) :- known(P)),
    fc_interface.