/*
 * Depth-first search in a state space.
 * Let us consider two problems. The first problem is
 * to find a path in a tree.
 *         a
 *       /   \
 *      b     c
 *    /  \    | \
 *   d    e   g  f
 *  /    / \      \
 * h    i   j      k
 *
 * Let j and f be a goal nodes. We want to find
 * a path from a selected node to the one of the
 * goal ones.
 *
 * The second problem is similar to hanoi tower.
 * Consider the followin example:
 *
 * c                      a
 * a            ----->    b
 * b                      c
 * __________             _______________
 *
 * We have a 3 blocks a, b and c lying on a table in stack,
 * and we want them rearranged as the figure shows.
 * This problem is to find a plan for a robot to rearrange
 * a stack. The robot is only allowed to move one block at
 * a time. A block can be grasped only when its top is clear,
 * i.e. there are no blocks atop of it.
 * A block can be put on the table or on some other block.
 * A plan is a sequence of moves transforming left state to
 * the right.
 */

:- use_module(library(clpfd)).
:- use_module('../include_all').

% First let us describe a tree using the following relation.
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

% Depth-first search.
% df_search(+SuccRel, +GoalRel, +Seen, +Start, -Solution)
% SuccRel - successor relation (for example ts or ss)
% GoalRel - goal relation (t_goal or s_goal)
% Seen    - list of states we already been in
% Start   - Starting node
% Solution- required sequence of state-to-state moves
%
% Note that the predicate constructs solution in a
% straight order, i.e. solution is a list starting
% from the Start and ending with a goal.
% Some other DF-search strategies represent solutions
% in the inverse order.
df_search(_, Goal, _, G, [G]) :- call(Goal, G).
df_search(Succ, Goal, Seen, N, [N|Path]) :-
    call(Succ, N, Sn),
    \+ memberchk(Sn, Seen), % preventing beeing at the same state more than once
    df_search(Succ, Goal, [Sn|Seen], Sn, Path).


% Find solutions of our two problems.
% For example:
% ?- solve_tree(a, Sol).
% ?- solve_stacks([[c,a,b],[],[]], Sol).
solve_tree(N, Sol) :-
    df_search(ts, t_goal, [], N, Sol).

solve_stacks(N, Sol) :-
    df_search(ss, s_goal, [], N, Sol).


% Ok, we have our solution, and it even has a cycle-detection.
% There are, however, state spaces where one or more branches
% are infinite, and the depth-first search can easily miss a goal
% node, proceesing along on of them, never producing any result.
% To avoid aimless non-cyclic but infinite branches, we can limit
% the depth of the search.
% A problem here is that we have to guess a suitable limit in advance.
% So, instead of specifying it directly, let us execute the DF-search
% iteratively, varying the depth limit: starting with a low depth limit
% and gradually increase it until the solution is found.
% Such technique is called iterative deepening.
%
% Consider an example of a predicate that does just that: produces
% all paths of increasing length starting from node N. Path is in
% reversed order.
%
% id_path(+SuccRel, +Node, ?Goal, -Path)
%
% ?- id_path(ts, a, G, Path).
% G = a, P = [a] ;
% G = b, P = [b, a] ;
% G = c, P = [c, a] ;
% ...
id_path(_, N, N, [N]).
id_path(Succ, N, G, [G|Path]) :-
    id_path(Succ, N, BeforeGoal, Path),
    call(Succ, BeforeGoal, G),
    \+ memberchk(G, Path).

%id_path(_, N, N, [N]).
%id_path(Succ, N, G, [G|Path]) :-
%    id_path(Succ, N, BeforeGoal, Path),
%    (call(Succ, BeforeGoal, G) -> \+ memberchk(G, Path) ; !).

solve_tree_id_(Start, Sol) :-
    id_path(ts, Start, Goal, Sol),
    t_goal(Goal).

solve_stacks_id_(Start, Sol) :-
    id_path(ss, Start, Goal, Sol),
    s_goal(Goal).

% The solution above is not good.
% id_path/4 is not tail-recursive, and loops infinitely
% after producing all solutions.
%
% Let's do something about it, and make our solution more
% general and efficient.
%
% id_path_(+SuccRel, +CurrLen, +Node, ?Goal, -Path)
%
% ?- id_path(ts, 1, a, G, P).
% G = a, P = [a] ;
% G = b, P = [a, b] ;
% G = c, P = [a, c] ;
% ...
% G = k, P = [a, c, f, k] ;
% false.
%
% That's it - the predicate does not loop.
% id_path_/4 is an auxilliary predicate calling id_path_/5
% with an initial Len = 1.

id_path_(Succ, N, G, P) :- id_path_(Succ, 1, N, G, P).

id_path_(Succ, Len, N, G, P) :-
    list_te(P, Len),
    path(Succ, N, G, [], P) ;
    list_te(T, Len),
    path(Succ, N, _, [], T),
    !,
    IncLen #= Len + 1,
    id_path_(Succ, IncLen, N, G, P).

%id_path_(Succ, Len, Node, Goal, Path) :-
%    list_te(Path, Len),
%    path(Succ, Node, Goal, [], Path);
%    IncLen #= Len + 1,
%    id_path_(Succ, IncLen, Node, Goal, Path).


path(_, N, N, _, [N]).
path(Succ, N, G, Buff, [N|Path]) :-
    call(Succ, N, Next),
    \+ memberchk(Next, Buff),
    path(Succ, Next, G, [Next|Buff], Path).

% Solve tree problem.
% ?- solve_tree_id(a, Sol).
% Sol = [a, c, f] ,
% Sol = [a, b, e, j].
% The shortest solution is found first.
solve_tree_id(Start, Sol) :-
    id_path_(ts, Start, Goal, Sol),
    t_goal(Goal).

solve_stacks_id(Start, Sol) :-
    id_path_(ss, Start, Goal, Sol),
    s_goal(Goal).
