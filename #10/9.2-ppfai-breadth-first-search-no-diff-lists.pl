/*
 * Breadth-first search in a state space.
 * Let us have the same state spaces as we did in
 * the depth-first search program.
 * The breadth-first search is not so easy to program
 * because we have to maintain a set of alternative
 * candidates, not just one as in depth-first search.
 * However, even this set of nodes is not sufficient
 * if we also want to extract a solution path from
 * the search process.
 * Therefore, we maintain a set of candidate paths.
 * Look definition of bf_search/4 for details.
 */

% ts(?Parent, ?Child) - tree_successor
% It holds if Child is child node of a Parent.
% Child is a successor of Parent.
% For example b and c are children of a.
ts(a, b).
ts(a, c).
ts(b, d).
ts(b, e).
ts(c, f).
ts(c, g).
ts(d, h).
ts(e, i).
ts(e, j).
ts(f, k).

% Nodes f and j are goals in the tree.
t_goal(j).
t_goal(f).


% For more complex state spaces representing the successor
% relation as a set of facts would be impractical or even
% impossible. Therefore, it could be defined implicitly
% by stating the rules for computing successor nodes of a
% selected node.
%
% ss(?Curr, ?Next) - state successor.
% It holds iff Next is a successor state of the Curr.
% For the stacks of blocks the Next is a successor of Curr
% if there are 2 stacks, S1 and S2 in Curr, and the top block
% of S1 can be moved to S2.
% A state is represented as a list of stacks currently on
% the table. To make it more relevant to real world problem
% let us limit the maximum number of stacks allower on the table.
% Empty stacks are represented by empty lists.

ss(Curr, [S1Next, [TopS1|S2]|Rest]) :-
    S1 = [TopS1|S1Next],      % TopS1 was the top block of S1
    remove(S1, Curr, Buff),   % Remove S1 and S2 from the States
    remove(S2, Buff, Rest).

% Standard predicate delete/3 does not help here
% (try it and see it for yourself =) ), so we need
% a hand-make delete relation.
% remove(+Elem, +List, -L1).
% L1 is a List with Elem removed from it.
remove(B, [B|Bs], Bs).
remove(B, [B1|Bs], [B1|Bs1]) :- dif(B,B1), remove(B, Bs, Bs1).

% A goal state is any arrangement with the ordered stack
% of required form.
s_goal(S) :- member([a,b,c],S).


% bf_search(+SuccRel, +GoalRel, +Frontier, -Solution)
% It holds iff some paths from a candidate set called
% Frontier can be extended to one of the goal nodes.
% Solution is such an extended path.
% Same as for DF-predicates SuccRel and GoalRel are
% relations to define successors and goals.
%
% If the head of the first path in the frontier is a goal
% node then this path is a solution.
% Otherwise remove the first path from the frontier and
% make a list of all possible one-step extensions of this
% path, add this list at the end of the frontier, and
% proceed recursively on this updated frontier.
bf_search(_, Goal, [Sol|_], Sol) :-
    Sol = [G|_],
    call(Goal, G).
bf_search(Succ, Goal, [Curr|Rest], Sol) :-
    update(Succ, Curr, ExtendedCurr),
    append(Rest, ExtendedCurr, Frontier),
    bf_search(Succ, Goal, Frontier, Sol).

update(Succ, P, Ext) :-
    P = [N|Ns],
    findall(
        [Next,N|Ns],
        (call(Succ, N, Next), \+memberchk(Next,P)),
        Ext).

solve_tree_bf(Start, Sol) :-
    bf_search(ts, t_goal, [[Start]], Sol).

solve_stacks_bf(Start, Sol) :-
    bf_search(ss, s_goal, [[Start]], Sol).
