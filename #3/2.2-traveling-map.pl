/*
 * A simple route calculator.
 */

% There are the following places we want to travel between
% place(  'Auckland'  ).
% place(  'Hamilton'  ).
% place(   'Raglan'   ).
% place(  'Valmont'   ).
% place('Saarbruecken').
% place(    'Metz'    ).
% place(  'Frankfurt' ).
% place(   'Paris'    ).
% place(  'Bangkok'   ).
% place( 'Singapore'  ).
% place( 'LosAngeles' ).

trip('Auckland'    , 'Hamilton'    , car  ). % Traveling from Aucklang to Hamilton by car
trip('Hamilton'    , 'Raglan'      , car  ). % Traveling from Hamilton to Raglan bt car
trip('Valmont'     , 'Saarbruecken', car  ). % Traveling from Valmont to Saarbruecken by car
trip('Valmont'     , 'Metz'        , car  ). % Traveling from Valmont to Metz by car
trip('Metz'        , 'Frankfurt'   , train). % Traveling from Metz to Frankfurt by train
trip('Saarbruecken', 'Frankfurt'   , train). % Traveling from Saarbruecken to Frankruth by train
trip('Metz'        , 'Paris'       , train). % Traveling from Metz to Paris by train
trip('Saarbruecken', 'Paris'       , train). % Traveling from Saarbruecken to Paris by train
trip('Frankfurt'   , 'Bangkok'     , plane). % Traveling from Frankfurt to Bangkok by plain
trip('Frankfurt'   , 'Singapore'   , plane). % Traveling from Frankfurt to Singapore by plain
trip('Paris'       , 'LosAngeles'  , plane). % Traveling from Paris to Los Angeles by plain
trip('Bangkok'     , 'Auckland'    , plane). % Traveling from Bangkok to Aucklang by plain
trip('LosAngeles'  , 'Auckland'    , plane). % Traveling from Los Angeles to Aucklang by plain

% If you can travel from place A to place B
% by some T then you can do vice versa and
% travel from B to A by the same means
directly_connected(A, B, T) :- trip(A, B, T) ; trip(B, A, T).


% A sample recursive solution

% from_to(+A, ?B, +Passed, -Route)
% Holds if you can travel from A to B by route Route
% A: starting place
% B: required place
% Passed: a list of places we already visited (i.e. they are in route)
% Route: required route as a list [A, T1, A1, T2, A2, ..., Tn, B].

% If there is a direct route from A to B by T then stop and add T and B
% to the Route
from_to(A, B, P, [T,B]) :- directly_connected(A, B, T), \+member(B, P).
from_to(A, B, Passed, [T,Next|Route]) :-
    directly_connected(A, Next, T),         % look for some place Next, A connected with by T
    \+ member(Next, Passed),                % where Next is not visited yet
    from_to(Next, B, [Next|Passed], Route).

route(A, B, Route) :- from_to(A, B, [A], R), Route = [A|R].


% More sophisticated solution using DCGs

from_to(A, B, P) --> { directly_connected(A, B, T), \+member(B, P) }, [T, B].
from_to(A, B, Passed) -->
    {
        directly_connected(A, Next, T),
        \+ member(Next, Passed)
    },
    [T, Next],
    from_to(Next, B, [Next|Passed]).

dcg_route(A, B, Route) :- phrase(from_to(A, B, [A]), R), Route = [A|R].


% Produce generic lists with length counter
% Using this predicate we can limit the length
% of routes, posing queries like the following:
%
% ?- list_te(R, 5), route(A, B, R).
% It produces all routes of length 5 between some places A and B
list_te([], 0).
list_te([_|Ls], N) :- N #> 0, N0 #= N - 1, list_te(Ls, N0).