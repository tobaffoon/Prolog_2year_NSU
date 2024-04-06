:- prolog_load_context(directory,Dir), atom_concat(Dir, './wordnet', WNDB_), absolute_file_name(WNDB_, WNDB), asserta(user:file_search_path(wndb, WNDB)).
:- use_module(wnload/prolog/wn).
:- use_module(library(clpfd)).

% related_words(mouse/PoS1/Sense1/Syn1, squirrel/PoS2/Sense2/Syn2, 5, Connection).

% MAIN FUNCTION
% Generate WordNet connection between two words
related_words(W1/P1/S1/Syn1, W2/P2/S2/Syn2, MaxDist, Connection) :-
    wn_s(Syn1, _, W1, P1, S1, _), 
    wn_s(Syn2, _, W2, P2, S2, _),
    between(1, MaxDist, MaxDists),    
    length(SynsetConnection, MaxDists),
    synset_connection(Syn1, Syn2, [Syn1], SynsetConnection),
    map_to_words_boarders(W1/P1/S1, SynsetConnection, W2/P2/S2, Connection).

% Get synset relation as term
synset_relation(Syn1, Syn2, r(Syn1, hyp, Syn2)) :- wn_hyp(Syn1, Syn2).
synset_relation(Syn1, Syn2, r(Syn1, mm, Syn2)) :- wn_mm(Syn1, Syn2).
synset_relation(Syn1, Syn2, r(Syn1, mp, Syn2)) :- wn_mp(Syn1, Syn2).

% Map relation from synset to word in any way possible
map_relation(W1/P1/S1, r(Syn1, Rel, Syn2), r(W1/P1/S1, Rel, W2/P2/S2)) :-
    wn_s(Syn1, _, W1, P1, S1, _),
    wn_s(Syn2, _, W2, P2, S2, _).
map_relation(W1/P1/S1, r(Syn1, Rel, Syn2), r(W2/P2/S2, Rel, W1/P1/S1)) :-
    wn_s(Syn2, _, W1, P1, S1, _),
    wn_s(Syn1, _, W2, P2, S2, _).

% Check if last relation contains last word
check_ends_with(ContextEnd, [r(W1/P1/S1, _, W2/P2/S2) | WordConnection]) :-
    WordConnection \= [];
    (ContextEnd = W1/P1/S1;
    ContextEnd = W2/P2/S2).

% Map relation from synset to word asserting last word
map_to_words_ends_with([], _, []).
map_to_words_ends_with([r(Syn1, Rel, Syn2) | SynConnection], ContextEnd, [r(W1/P1/S1, Rel, W2/P2/S2) | WordConnection]) :-
    wn_s(Syn1, _ , W1, P1, S1, _),
    wn_s(Syn2, _ , W2, P2, S2, _),
    map_to_words_ends_with(SynConnection, ContextEnd, WordConnection),
    check_ends_with(ContextEnd, [r(W1/P1/S1, Rel, W2/P2/S2) | WordConnection]).

% Map relation from synset to word asserting first word
map_to_words_boarders(ContextStart, [SynRel | SynConnection], ContextEnd, [WordRel | WordConnection]) :-
    map_relation(ContextStart, SynRel, WordRel),
    map_to_words_ends_with(SynConnection, ContextEnd, WordConnection).

% Get relation between two synsets
related_synset(Syn1, Syn2, Relation) :-
    synset_relation(Syn1, Syn2, Relation);
    synset_relation(Syn2, Syn1, Relation).

% Get connection between two synsets
synset_connection(Syn, Syn, _, []).
synset_connection(Syn1, Syn2, Used, [Relation | Connection]) :-
    related_synset(Syn1, Syn3, Relation),
    \+member(Syn3, Used),
    synset_connection(Syn3, Syn2, [Syn3 | Used], Connection).