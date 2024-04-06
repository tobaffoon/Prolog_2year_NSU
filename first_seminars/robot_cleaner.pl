% state(Robot, Basket, Rubbish)
% Robot and Basket = point(X,Y)
% Rubbish = floor(point(X,Y)) or held or in_basket
% action(CurState, Action, NextState)
% Action = pickup / drop / push (backet) / go

% state(R, B, Rb).
:- use_module(library(clpfd)).

% pickup
action(state(R, B, floor(R)), pickup, state(R, B, held)).

% drop
action(state(B, B, held), drop, state(B, B, in_basket)).
%   action(state(R, B, held), drop, state(R, B, floor(R))) :- R \= B.

% push 
action(state(B, B, Rb), push(B, NewLoc), state(NewLoc, NewLoc, Rb)).

% go
action(state(R, B, Rb), go(R, NewLoc), state(NewLoc, B, Rb)).

% protocol(Start, Final, Protocol).
protocol(Final, Final, []).

protocol(Curr, Final, [Act | Tail]) :- 
    action(Curr, Act, Next),
    protocol(Next, Final, Tail).

% doesn't work, should return schedules not longer than N
protocolN(Curr, Final, Plan, N) :- 
    gen(Plan, N),
    protocol(Curr, Final, Plan).

gen([], 0).
gen([_:L], N) :- N #> 0, N1 #= N - 1, gen(L, N1).