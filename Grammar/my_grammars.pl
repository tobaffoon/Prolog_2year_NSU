% 1. a*
star   --> [].
star   --> [a], star.

rec(G, W) :-
    string_chars(W, Chars),
    call(G, Chars, []).

% 2. a^n b^n
two_symb    --> [].
two_symb    --> [a], two_symb, [b].

% 3. a^n b^2n
two_symb2   --> [].
two_symb2   --> [a], two_symb2, [b, b].

% 4. a^n b^2m c^2m d^n
four_symb   --> [].
four_symb   --> [a], four_symb, [d].
four_symb   --> four_symb2.
four_symb2  --> [].
four_symb2  --> [b, b], four_symb2, [c, c].

% 5. wcw^r (w^r - reverse), w from {a,b}*
odd_polyndrom   --> [c].
odd_polyndrom   --> [a], odd_polyndrom, [a].
odd_polyndrom   --> [b], odd_polyndrom, [b].

:- use_module(library(clpfd)).

% 6 a^n b^n c^n
s --> as(N), bs(N), cs(N).

as(0) --> [].
as(N) -->
    { N #> 0, N #= M + 1 }, % без фигурных транслятор --> добавит разностыне списки ко всем подфункциям
    [a], as(M).             % В том числе и #> и #=

bs(0) --> [].
bs(N) -->
    { N #> 0, N #= M + 1 }, 
    [b], bs(M).             

cs(0) --> [].
cs(N) -->
    { N #> 0, N #= M + 1 }, 
    [c], cs(M).             