:- use_module(library(clpfd)).
:- use_module('area-map').

:- dynamic came_from/2.
% man(Goal, Node, Dist).
%
% Calculates Manhatten distance between two cells (|x1-x2| + |y1-y2|)
man(Goal, Node, Dist) :-
    grid_cell(Node, coords(X1, Y1), _),
    grid_cell(Goal, coords(X2, Y2), _),
    abs(X1 - X2, Xabs),
    abs(Y1 - Y2, Yabs),
    Dist #= Xabs + Yabs.

% insert(Elem, List, Upd)
%
% Adds Elem in List with insertion_sort, Upd is new list
insert(X, [], [X]).
insert((X, Mx), [(Y, My) | Ys], [(X, Mx), (Y, My) | Ys]) :-
    Mx #< My,
    !.

insert(X, [Y | Ys0], [Y | Ys1]) :-
    insert(X, Ys0, Ys1).

% update(Ns, Front, Upd).
update([], Front, Front).
update([N | Ns], Front, Upd) :-
    insert(N, Front, Buff),
    update(Ns, Buff, Upd).

greed_search(Start, Goal, Path) :-
    retractall(came_from(_,_)),
    asserta(came_from(Start, -1)),
    gs_traverse(Goal, [(Start, 0)], Path).
    
gs_traverse(Goal, [(Goal, _) | _], Path) :- unroll(Goal, [], Path), !.
gs_traverse(Goal, [(Current, _) | Rest], Path) :-
    findall(
        (Next, Mnext),
        (neighbor(Current, Next), \+came_from(Next, _), man(Next, Goal, Mnext)),
        Ns
    ),
    assert_visited(Ns, Current),
    update(Ns, Rest, Extended),
    gs_traverse(Goal, Extended, Path).

greed_search1(Start, Goal, Path) :-
    retractall(came_from(_,_)),
    asserta(came_from(Start, -1)),
    gs_traverse1(Goal, [(Start, 0)], Path).
    
gs_traverse1(Goal, [(Goal, _) | _], [Goal]) :- !.
gs_traverse1(Goal, [(Current, _) | Rest], [Current | Path]) :-
    findall(
        (Next, Mnext),
        (neighbor(Current, Next), \+came_from(Next, _), man(Next, Goal, Mnext)),
        Ns
    ),
    assert_visited(Ns, Current),
    update(Ns, Rest, Extended),
    gs_traverse1(Goal, Extended, Path).

assert_visited([], _).
assert_visited([(Node, _) | Ns], From) :-
    asserta(came_from(Node, From)),
    assert_visited(Ns, From).

unroll(Current, Buffer, [Current | Buffer]) :- came_from(Current, From), From #< 0, !.
unroll(Current, Buffer, Path) :- came_from(Current, From), From #>= 0, unroll(From, [Current | Buffer], Path).

:- dynamic cost_so_far/2.

% estimate(+CurrentCost, +Next, -NextCost)
% CurrentCost - Цена в Next
% Next - куда хотим добраться за один шаг
% NextCost - сколько это будет нам стоить
estimate(CurrentCost, Next, NextCost):-
    grid_cell(Next, _, CellCost),
    NextCost #= CurrentCost + CellCost.

% unified search cost
usc_rec([], _).
usc_rec([(Next, NextCost) | Ns], From) :-
    asserta(came_from(Next, From)),
    asserta(cost_so_far(Next, NextCost)),
    usc_rec(Ns, From).

% usc_traverse(+Goal, +Frontier, -Path, -Cost)
usc_traverse(Goal, [(Goal, Cost) | _], Path, Cost) :- unroll(Goal, [], Path), !.
usc_traverse(Goal, [(Current, CurrentCost) | Rest], Path, Cost) :-
    findall(
        (Next, NextCost),
        (neighbor(Current, Next), \+came_from(Next, _), estimate(CurrentCost, Next, NextCost)),
        Ns
    ),
    usc_rec(Ns, Current),
    update(Ns, Rest, Extended),
    usc_traverse(Goal, Extended, Path, Cost).

usc_traverse1(Goal, [(Goal, Cost) | _], [Goal], Cost) :- !.
usc_traverse1(Goal, [(Current, CurrentCost) | Rest], [Current | Path], Cost) :-
    findall(
        (Next, NextCost),
        (neighbor(Current, Next), \+came_from(Next, _), estimate(CurrentCost, Next, NextCost)),
        Ns
    ),
    usc_rec(Ns, Current),
    update(Ns, Rest, Extended),
    usc_traverse(Goal, Extended, Path, Cost).

usc(Start, Goal, Path, Cost) :-
    retractall(cost_so_far(_,_)),
    retractall(came_from(_,_)),
    asserta(cost_so_far(Start, 0)),
    asserta(came_from(Start, -1)),
    usc_traverse(Goal, [(Start, 0)], Path, Cost).

usc1(Start, Goal, Path, Cost) :-
    retractall(cost_so_far(_,_)),
    retractall(came_from(_,_)),
    asserta(cost_so_far(Start, 0)),
    asserta(came_from(Start, -1)),
    usc_traverse1(Goal, [(Start, 0)], Path, Cost).