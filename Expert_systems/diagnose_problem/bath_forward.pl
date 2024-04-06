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
:- dynamic derived/1.

fact(Proposition) :- derived(Proposition).
fact(P1 and P2) :- fact(P1), fact(P2).
fact(P1 or P2) :- fact(P1); fact(P2).

derivable(Prod) :-
    if Cond then Prod,
    \+ derived(Prod),
    fact(Cond).

fc_interface :-
    derivable(Proposition),
    !,
    format('Infered that ~w is true;\n', [Proposition]),
    assertz(derived(Proposition)),
    write('--------------------------------'),
    fc_interface.

fc :-
    retractall(derived(_)),
    assertz(derived(P) :- known(P)),
    fc_interface.