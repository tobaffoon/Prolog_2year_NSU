% Automatic (syntax sugar) translation to difference lists
% :- set_prolog_flag(double_quotes, chars). % all symbols in double_quotes are traslated as lists of chars

sentence    --> noun_phrase, verb_phrase.
noun_phrase --> article, noun.
verb_phrase --> verb, noun_phrase.

article     --> ["a"].
article     --> ["the"].
noun        --> ["mouse"].
noun        --> ["cat"].
verb        --> ["scares"].
verb        --> ["hates"].