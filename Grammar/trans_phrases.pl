sentence        --> noun_phrase, verb_phrase.

noun_phrase     --> proper_noun.
noun_phrase     --> det, noun, rel_clause.

verb_phrase     --> intrans_verb.
verb_phrase     --> trans_verb, noun_phrase.

rel_clause      --> [].
rel_clause      --> ["that"], verb_phrase.

det             --> ["a"].
det             --> ["every"].

noun            --> ["man"].
noun            --> ["woman"].

proper_noun     --> ["John"].
proper_noun     --> ["Annie"].
proper_noun     --> ["Monet"].

trans_verb      --> ["likes"].
trans_verb      --> ["admires"].

intrans_verb    --> ["paints"].