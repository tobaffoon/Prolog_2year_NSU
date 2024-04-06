sentence(S, D) :-
    noun_phrase(S, SWithoutNP), % SWithoutNP = VP + D
    verb_phrase(SWithoutNP, D).

noun_phrase(NP, D) :-
    article(NP, NPWithoutA),
    noun(NPWithoutA, D).

verb_phrase(VP, D) :-
    verb(VP, VPWithoutVerb),
    noun_phrase(VPWithoutVerb, D).

article(["a" | D], D).
article(["the" | D], D).

noun(["cat" | D], D).
noun(["mouse" | D], D).

verb(["scares" | D], D).
verb(["hates" | D], D).

sentence(S) :-
    noun_phrase(NP),
    verb_phrase(VP),
    append(NP, VP, S).

noun_phrase(NP) :-
    article(A),
    noun(N),
    append(A, N, NP).

verb_phrase(VP) :-
    verb(V),
    noun_phrase(NP),
    append(V, NP, VP).

article(["a"]).
article(["the"]).

noun(["cat"]).
noun(["mouse"]).

verb(["scares"]).
verb(["hates"]).