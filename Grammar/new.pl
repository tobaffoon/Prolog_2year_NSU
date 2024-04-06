:- op(100, xfy, and).
:- op(150, xfy, ==>).

sentence(Meaning) --> noun_phrase(Subj, Assertion, Meaning), verb_phrase(Subj, Assertion).

noun_phrase(Subj, Assertion, Assertion)  --> proper_noun(Subj). % if noun doesn't have det Meaning is from VP
noun_phrase(Subj, Assertion, Meaning)  --> 
    det(Subj, Prop, Assertion, Meaning),    % det gives and assertion a meaning putting it in exists or all
    noun(Subj, P1),
    rel_clause(Subj, P1, Prop). % rel clause gets first property of noun and returns new more complex 

verb_phrase(Subj, Assertion)   --> intrans_verb(Subj, Assertion).
verb_phrase(Subj, Assertion)   --> 
    trans_verb(Subj, Obj, A1),  % A1 is just an assertion it may be wrapped with exists or wrap after NP
    noun_phrase(Obj, A1, Assertion).   

rel_clause(_, P, P)  --> [].    % if no "that", then inital property is the meaning
rel_clause(Subj, Prop1, Prop1 and Prop2)    -->
    ["that"],
    verb_phrase(Subj, Prop2).

det(X, Prop, Assertion, exists(X, Prop and Assertion)) --> ["a"].
det(X, Prop, Assertion, all(X, Prop ==> Assertion)) --> ["every"].

noun(X, man(X))     --> ["man"].
noun(X, woman(X))     --> ["woman"].

proper_noun("John") --> ["John"].
proper_noun("Annie") --> ["Annie"].
proper_noun("Monet") --> ["Monet"].

intrans_verb(X, paints(X))    --> ["paints"].

trans_verb(Subj, Obj, likes(Subj, Obj))      --> ["likes"].
trans_verb(Subj, Obj, admires(Subj, Obj))      --> ["admires"].

meaning(Sentence, Meaning) :-
    split_string(Sentence, " ", "", List),
    phrase(sentence(Meaning), List).
    
    