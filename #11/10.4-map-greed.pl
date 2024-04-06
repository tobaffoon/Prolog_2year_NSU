/*
 * Greed search on a map. Also called heuristic search.
 */

:- use_module(library(clpfd)).
:- use_module('10.1-area-map').

% came_from(?Where, ?From)
%
% The relation holds if and only if we have come to the cell 'Where'
% from the cell 'From' while traversing the map.
:- dynamic came_from/2.

% greed_search(?Start, ?Goal, -Path)
%	Start:	the cell we start from
%	Goal:	the cell we want to come to
%	Path:	path from Start to Goal as a list of cells
%
%	Traverse the map taking directions shown by the heuristic function.
%	Stops when Goal is reached.

greed_search(Start, Goal, Path) :-
	retractall(came_from(_,_)),		% clean previous data if present
	asserta(came_from(Start, -1)),	% we did not come to the starting point from anywhere
	gs_traverse(Goal, [(Start, 0)], Path).


% gs_traverse(+Goal, +Frontier, -Path)
%	Goal:		cell we want to be in
%	Frontier:	a list of cells we want to traverse; each element of the frontier
%				is a pair of a form (CellID, CellPriority) where CellPriority is a
%				value of the heuristic function on the CellID. Frontier is sorted on
%				in ascending order of the CellPriority.
%	Path:		a path we want to construct
%
% Explore frontier. On each step pick the most promising cell from the frontier,
% which is always the first element of the frontier since the frontier is sorted by priority.
% Then get all the neighbors of the cell picked, and add them to the frontier keeping
% its sorted state.

gs_traverse(Goal, [(Goal, _) | _], Path) :-	% stop when the next node to traverse is the Goal
	unroll(Goal, [], Path),
	!.
gs_traverse(Goal, [(Current, _) | Rest], Path) :-
	findall(
		(Next, H),                         % Next is a neighbor of the Current
		(                                  % where H = h(Goal, Next) - heurisric function value
			neighbor(Current, Next),       % for Next
			\+ came_from(Next, _),         % <- this is checking for loops - we take Next only if
			h(Goal, Next, H)               % we have not traverse it yet
		),
		NeighborsOfCurrentNode             % list of neighbors, where each element is a pair of a form
	),                                     % (NeighborID, NeighborHeuristicValue)
	rec(NeighborsOfCurrentNode, Current),  % save information that we have come to the neighbors from the Current
	update(NeighborsOfCurrentNode, Rest, ExtendedFrontier),
	gs_traverse(Goal, ExtendedFrontier, Path).


% rec(+Neighbors, +Node)
%
% add facts of a form came_from(N, Node) where N is a neighbor of Node

rec([], _).
rec([(Neighbor, _) | Ns], Node) :-
	asserta(came_from(Neighbor, Node)),
	rec(Ns, Node).


% h(Goal, Node, H)
%
% Heuristic function. In this case heuristic is a manhattan distance from Node to Goal
% Let P1 (x1, y1) and P2 (x2, y2) be points in 2d space. Manhattan distance between P1
% and P2 is computed as follows:
%
% M = |x1 - x2| + |y1 - y2|

h(Goal, Node, H) :-
	grid_cell(Goal, coords(_gx, _gy), _),   % (_gx, _gy) is the Goal's coordinates
	grid_cell(Node, coords(_nx, _ny), _),   % (_nx, _ny) is the Node's coordinates
	abs(_gx - _nx, XDiff),                  % XDiff = | _gx - _nx |
	abs(_gy - _ny, YDiff),                  % YDiff = | _gy - _ny |
	H #= XDiff + YDiff.


% update(+NewNodes, +Frontier, -ExtendedFrontier)
%
% Extend frontier with new nodes.

update([], F, F).                    % If there is nothing to add then ExtendedFrontier = Frontier
update([Node | Ns], Frontier, Extended) :-
	insert(Node, Frontier, Buff),    % add one node to the frontier resulting in Buff
	update(Ns, Buff, Extended).      % add remaining nodes to the Buff


% insert(+Elem, +Sorted, -ExtendedSorted)
%
% Add an element of a form (Node, H) to a sorted list of similar pairs.
% 'Sorted' is sorted by the second member of a pair ascending.

insert(X, [], [X]).
insert((X, Hx), [(Y, Hy) | Rest], [(X, Hx), (Y, Hy) | Rest]) :- % if Hx < Hy then insert X before Y
	Hx #< Hy,
	!.
insert(X, [Y | Rest], [Y | RestWithXInserted]) :- % otherwise skip Y and insert X into the tail
	insert(X, Rest, RestWithXInserted).


% unroll(+CurrentNode, +PartialPath, -Path)
%
% Go from Goal to Start, making a path. At each step we add a current node to a path
% and go to the previous node via the came_from/2 relation.
%
% Consider the query:
%
% ?- unroll(Goal, [], Path)
%
% We start at the goal node with an empty partial path.
% Let us assume that there is a node Nk such that came_from(Goal, Nk), meaning we came
% to the Goal from Nk. Then we add Goal to the partial path and proceed from Nk:
% unroll(Nk, [Goal], Path). Let's now assume there is Nk-1 such that came_from(Nk, Nk-1).
% Now we have unroll(Nk-1, [Nk, Goal], Path). And so on until we find outselves
% at the starting node.
% unroll(Start, [N1, N2, ..., Nk-1, Nk, Goal], Path). Here we fall into the first rule
% and make Path = [Start, N1, N2, ..., Nk-1, Nk, Goal]

unroll(Start, PartialPath, [Start | PartialPath]) :- % if From < 0, it means From = -1 and we reached Start
	came_from(Start, From),                          % then we stop and say Path = [Start | PartialPath]
	From #< 0.
unroll(Current, PartialPath, Path) :-
	came_from(Current, From),
	From #>= 0,
	unroll(From, [Current | PartialPath], Path).
