parent(pamela, robert).
parent(tom, robert).
parent(tom, jessica).
parent(robert, ann).
parent(robert, kate).
parent(kate, joseph).

male(tom).
male(robert).
male(joseph).
female(pamela).
female(jessica).
female(ann).
female(kate).

grandparent(Gp,Gc) :- parent(Gp, P), parent(P, Gc).
mother(M, C) :- parent(M, C), female(M).
father(F, C) :- parent(F, C), male(F).
sister(S, P) :- parent(Par, S), parent(Par, P), female(S), dif(S, P).
brother(B, P) :- parent(Par, B), parent(Par, P), male(B), dif(B, P).
aunt(A, N) :- parent(P, N), sister(A, P).

ancestor(A, D) :- parent(A,D); (parent(A, Aa), ancestor(Aa, D)).
