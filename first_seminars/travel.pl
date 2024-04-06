:- use_module(library(clpfd)).
tr(auckland, hamilton, car).
tr(hamilton, raglam, car).
tr(valmont, saarbrueken, car).
tr(valmont, metz, car).
tr(metz, frankfurt, train).
tr(saarbrueken, frankfurt, train).
tr(metz, paris, train).
tr(saarbrueken, paris, train).
tr(frankfurt, bangkok, plane).
tr(frankfurt, singapore, plane).
tr(paris, losAngeles, plane).
tr(bangkok, auckland, plane).
tr(losAngeles, auckland, plane).

dc(A, B, T) :- tr(A, B, T); tr(B, A, T).

route(A, B, [A|R]) :- routeNoRep(A, B, R, [A]).

routeNoRep(A, B, [T, B], Visited) :- dc(A, B, T), \+member(B, Visited).
routeNoRep(A, B, [T, C|R], Visited) :- dc(A, C, T),
											\+member(C, Visited),
											 routeNoRep(C, B, R, [C|Visited]).

gen([], 0).
gen([_:L], N) :- N #> 0, N1 #= N - 1, gen(L, N1).