/* a ->
 *      b ->
 *          d ->
 *              h
 *          e ->
 *              i
 *              (j)
 *      c ->
 *          g
 *          (f) ->
 *                k
 * 
 * (vertex) - terminus
 */

:- use_module(library(clpfd)).

list_te([], 0).
list_te([_ | T], Len) :-
    Len #> 0,
    NextLen #= Len - 1,
    list_te(T, NextLen).

% Tree successor

ts(a,b).
ts(a,c).
ts(b,d).
ts(b,e).
ts(c,g).
ts(c,f).
ts(d,h).
ts(e,i).
ts(e,j).
ts(f,k).

% Tree goal
tg(f).
tg(j).

% Stacks successor
ss(Current, [S1Next, [TopS1 | S2] | Rest]) :-
    S1 = [TopS1 | S1Next],
    remove(S1, Current, Buff),
    remove(S2, Buff, Rest).

remove(B, [B|Bs], Bs).
remove(B, [B1|Bs], [B1|Bs1]) :- dif(B,B1), remove(B, Bs, Bs1).

% Stacks goal
sg(S) :- (member([a,b,c], S)).

% df_search(SuccRule, GoalRule, Memory, Node, Solution)
% Mem - to avoid cycles
df_search(_, GR, _, G, [G]) :- call(GR, G).
df_search(SR, GR, Mem, Node, [Node|Ns]) :-
    call(SR, Node, Next),
    \+ memberchk(Next, Mem),
    df_search(SR, GR, [Next | Mem], Next, Ns).
    
solve_tree(Start, Sol) :- df_search(ts, tg, [], Start, Sol).
solve_stacks(Start, Sol) :- df_search(ss, sg, [], Start, Sol).

% path(SuccRule, Node, End, Mem, Path).
% Same but without last step 
path(_, Node, Node, _, [Node]).
path(SR, Node, End, Mem, [Node|Path]) :-
    call(SR, Node, Next),
    \+ memberchk(Next, Mem),
    path(SR, Next, End, [Next|Mem], Path).

id_search(SR, Len, Node, End, Path) :-
    list_te(Path, Len),
    path(SR, Node, End, [], Path);
    IncLen #= Len + 1,
    id_search(SR, IncLen, Node, End, Path).

id_search(SR, Node, End, Path) :- id_search(SR, 0, Node, End, Path).

solve_id_tree(Start, Sol) :- id_search(ts, Start, End, Sol), tg(End).
solve_id_stacks(Start, Sol) :- id_search(ss, Start, End, Sol), sg(End).

% bf_search(SuccRule, GoalRule, Froniter, Solution).
bf_search(_, GR, [Sol | _], Sol) :-
    Sol = [G | _],
    call(GR, G).
bf_search(SR, GR, [Current | Rest], Sol) :-
    update(SR, Current, Extended),
    append(Rest, Extended, Front),
    bf_search(SR, GR, Front, Sol).

% get new Frontier (children of current Frontier)
update(SR, Path, Ext) :-
    Path = [N|Ns],
    findall([Nx,N|Ns], (call(SR, N, Nx), \+ memberchk(Nx, Path)), Ext).

solve_bf_tree(Start, Sol) :- bf_search(ts, tg, [[Start]], Sol).
    
/* b - степень ветвление
 * d - высота
 * N(b,d)
 */