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