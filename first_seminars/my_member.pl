% mem(E, L) :- E is member of L
mem(E, [H|L]) :- (E = H); mem(E, L).

% cat(L1, L2, L3) :- L3 = L1+L2
cat([], L, L3) :- L3 = L.
cat([H|T], L, [H|Tres]) :- cat(T, L, Tres).

% rev(L, R) :- R is L reversed
rev([], R) :- R = [].
rev([H|T], R) :- cat(rev(T, R), [H], R).

% shift(L, S) :- [a,b,c] => [b,c,a]
shift([], S) :- S = [].
shift([H|T], S) :- cat(T, [H], S).
