% Automatic (syntax sugar) translation to difference lists
% :- set_prolog_flag(double_quotes, chars). % all symbols in double_quotes are traslated as lists of chars

% ?- listing(noun).
% noun(singular, ["mouse"|A], A).
% noun(singular, ["cat"|A], A).
% noun(plural, ["mices"|A], A).
% noun(plural, ["cats"|A], A).

sentence(Number)    --> noun_phrase(Number), verb_phrase(Number).
noun_phrase(Number) --> article(Number), noun(Number).
verb_phrase(Number) --> verb(Number), noun_phrase(_).

article(singular)       --> ["a"].
article(_)              --> ["the"].

noun(singular)          --> ["mouse"].
noun(singular)          --> ["cat"].
noun(plural)            --> ["mices"].
noun(plural)            --> ["cats"].

verb(singular)          --> ["scares"].
verb(singular)          --> ["hates"].
verb(plural)            --> ["scare"].
verb(plural)            --> ["hate"].