/*
 * There is a finite collection of jugs of known integer capacities.
 * Initially each jug contains a known integer volume of water, not
 * necessaryly equal to its capacity. There also is a pool in which any
 * jug can be filled to full capacity or emtptied to. It is stated that
 * the jugs are irregularly shaped and unmarked, so that it is
 * impossible to accurately measure any quality of water that does not
 * completely fill a jug. Write a program that answers how many steps of
 * pouring water from one jug to another (until either one jug becomes
 * empty or the other becomes full), filling jugs from the pool or
 * emptying them are required to reach a goal state. The goal state
 * specified in terms of the volume of water that must be present in
 * some jug or jugs. Test the program on the task when jugs volumes are
 * 3 and 5 litres, and the goal state is 4 litres of water in the larger
 * jug, assuming that initially both jugs are empty.
 */

:- use_module(library(clpfd)).

% State successor relation.
% s(?CurrState, ?NextState)
% holds if NextState is a successor of the CurrState.
% We define state as a list of pairs of a form (Ji,Ci)
% where Ji is a current volume of water i-th jug contains,
% and Ci is a capacity of the i-th jug.
% Therefore a state is a list [(J1,C1),(J2,C2),...,(Jn,Cn)].
% Next is successor of a current state if there is a pair of
% jugs Ji and Jj and we can pour some water from one to another, or we
% can choose a jug Ji and either fill it from the pool or empty it.

empty_jug((OldVolume, Capacity), (0, Capacity)) :-
    OldVolume #> 0.

fill_jug((OldVolume, Capacity), (Capacity, Capacity)) :-
    OldVolume #< Capacity.

min(N1, N2, Min) :-
    (
        N1 #< N2,
        Min = N1, !
    );
    Min = N2.

pour_from_jug_to_jug((FromVolume, FromCapacity), (ToVolume, ToCapacity), (NewVolume1, FromCapacity), (NewVolume2, ToCapacity)) :-
    FromVolume #> 0,
    ToVolume #< ToCapacity,
    PourLimit #= ToCapacity - ToVolume,
    min(FromVolume, PourLimit, Difference),
    NewVolume1 #= FromVolume - Difference,
    NewVolume2 #= ToVolume + Difference.

s([], []).
s(CurrState, NextState) :-
    select(OldJug, CurrState, NewJug, NextState),
    (
        empty_jug(OldJug, NewJug);
        fill_jug(OldJug, NewJug)
    ).

s(CurrState, NextState) :-
    nth0(Index, CurrState, SourceJug, StateWithoutSourceJug),                   % take random SourceJug and delete it to get StateWithoutSourceJug
    select(DistJug, StateWithoutSourceJug, NewDistJug, StateWithNewDistJug),    % take random DistJug from StateWithoutSourceJug, replace it with NewDistJug
    pour_from_jug_to_jug(SourceJug, DistJug, NewSourceJug, NewDistJug),         % pour
    nth0(Index, NextState, NewSourceJug, StateWithNewDistJug).                  % insert NewSourceJug in StateWithNewDistJug to get NextState

% Goal state is specified as a template of required form.
% For example, consider we have two jugs of capacity 3 and 5.
% Start state could be [(0,3),(0,5)] if both of them are empty.
% If we want the jugs to contain 1 and 4 litres correspondingly,
% we can specify G = [(1,_),(4,_)].

goal(G, G).

bf_search(_, Goal, [Sol|_], Sol) :-
    Sol = [G|_],
    call(Goal, G).
bf_search(Succ, Goal, [Curr|Rest], Sol) :-
    update(Succ, Curr, ExtendedCurr),
    append(Rest, ExtendedCurr, Frontier),           % We will build a next step for any lists of lists of states (path to solving a task)
    bf_search(Succ, Goal, Frontier, Sol).           % as path with next step is appended (at the end), we first construct N+1 length solutions, only than N+2 len

update(Succ, P, Ext) :-
    P = [N|Ns],
    findall(                                        % construct a list of states, where we can go from N
        [Next,N|Ns],                                % Next goes directly after N, thus the order
        (call(Succ, N, Next), \+memberchk(Next,P)), % Next is a successor of N 
        Ext).

solve(Start, Goal, Solution) :-
    bf_search(s, goal(Goal), [[Start]], Solution).


% One of the main drawbacks is the inefficiency of the append/3.
% This can be rectified by using the difference lists instead of
% ordinary lists. Frontier would then be represented by a pair of
% lists Frontier-T.

bf_search_d(_, Goal, [Sol|_]-_, Sol) :-
    Sol = [G|_],
    call(Goal, G).
bf_search_d(Succ, Goal, [Curr|Rest]-T, Sol) :-
    update(Succ, Curr, ExtendedCurr),
    append_d(Rest-T, ExtendedCurr-E, Frontier),
    bf_search_d(Succ, Goal, Frontier-E, Sol).

% Consider we have 3 jugs of capacity 8, 5 and 3 litres,
% and in the goal state they should be filled with 4,4 and 0
% litres.
% Compare solve/3 and solve_d/3 on this task.
%
% ?- solve([(8,8),(0,5),(0,3)], [(4,_),(4,_),(0,_)], Sol).
% ?- solve_d([(8,8),(0,5),(0,3)], [(4,_),(4,_),(0,_)], Sol).
%
% How the execution time changes when using difference lists?
solve_d(Start, Goal, Solution) :-
    bf_search_d(s, goal(Goal), [[Start]|T]-T, Solution).

% Действия с кувшином: V - объём воды, C - вместимость
% 1) Если он не пуст - опустошить его. V > 0: (V, C) -> (0, C)
% 2) Если не полон - дополнить. V < C: (V, C) -> (C, C)
% 3) Имея второй сосуд - перелить туда. 

% Используем nth0, чтобы убрать кувшин из списка кувшинов, если хотим перелить, так как в самого себя нельзя перелить

append_d(List-Tail, Tail, List).