:- use_module(library(clpr)).

cnv(C, F) :-
    {C = (F - 32) * 5 / 9}.

% test1 :- {X >= 5, X <= 10}, sup(X).
test2 :- {X > 5, X < 10}, maximize(X).
test3 :- {X > 5, X < 10}, sup(X, S).

% given processes a,b,c,d composing system f evaluate minimum time of completing f
% time of completing each subtask and their dependce on each other is given
solve(Ta, Tb, Tc, Td, Tf) :-
    {Ta >= 0,
    Tb >= Ta + 2,
    Tc >= Ta + 2,
    Td >= Tb + 3,
    Tf >= Td + 5,
    Tf >= Tc + 4}, minimize(Tf).
    
% A = 0.0,
% B = 2.0,
% D = 5.0,
% F = 10.0,
% {_= -4.0+_A, _A=<4.0, _A>=0.0, C=2.0+_A}. 
% Last means that C can start at 2.0 + _A, _A>=0.0, _A=<4.0 

% on a coordinate system rectangles have pos(X, Y) and d(W, H) - coordinates, width and height
block(b1, d(5,3)).
block(b2, d(2,6)).
block(b3, d(1,2.4)).
block(b4, d(1,5)).

box(box1, d(6,6)).
box(box2, d(7,5)).
box(box3, d(6,5)).

rot(rect(Pos, Dim), rect(Pos, Dim)).
rot(rect(Pos, d(W, H)), rect(Pos, d(H, W))).

place(Name, rect(Pos, Dim)) :-
    block(Name, BDim),
    rot(rect(Pos, Bdim), rect(Pos, Dim)).

:- op(400, xfx, inside).
:- op(400, xfx, no_overlap).
% xf, fx, xfx - x must have less priority than f
% fy, yf, yfx - x same, y can have equal priority to f (i.e. be f)
% numbers have priority 0

% check if one rectangle is inside another
inside(rect(pos(X1, Y1), d(W1, H1)), rect(pos(X2, Y2), d(W2, H2))) :-
    {
        X1 >= X2,
        X1 + W1 =< X2 + W2,
        Y1 >= Y2,
        Y1 + H1 =< Y2 + H2
    }.

no_overlap(rect(pos(X1, Y1), d(W1, H1)), rect(pos(X2, Y2), d(W2, H2))) :-
    {
        X1 + W1 =< X2;
        X2 + W2 =< X1;
        Y1 + H1 =< Y2;
        Y2 + H2 =< Y1        
    }.

arrange(BoxName, B1, B2, B3, B4) :-
    box(BoxName, BoxDim),
    Box = rect(pos(0,0), BoxDim),
    place(b1, B1), B1 inside Box,
    place(b2, B2), B2 inside Box,
    place(b3, B3), B3 inside Box,
    place(b4, B4), B4 inside Box,
    B1 no_overlap B2,
    B1 no_overlap B3,
    B1 no_overlap B4,
    B2 no_overlap B3,
    B2 no_overlap B4,
    B3 no_overlap B4.