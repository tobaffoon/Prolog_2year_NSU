/*
 * Decribing a cleaning robot protocol.
 *
 * State of the robot's world described with state/3
 *
 * state(RobotXY, BasketXY, RubbishLocation)
 *
 * Action produces NextState from a CurrentState
 *
 * action(CurrentState, Action, NextState)
 *
 * We assume robot never drops rubbish to floor,
 * and never pushes rubbish around.
 *
 * Positions are described with predicate point/2,
 * point(X, Y).
 * If position Pos of the rubbish is in the floor it is denoted as
 * floor(Pos). There are two additional positions where rubbish can be: 'held' and 'in_basket'.
 * The predicate floor/1 is needed, because we do not want robot
 * picking the rubbish up when it is already in the basket.
 *
 * Assuming that at the starting time the robot stands at (0,0),
 * basket is at (21,33) and the rubbish is on the floor at
 * position (4,15) we can pose the following query.
 *
 * ?- protocol(state(point(0,0),point(21,33),floor(point(4,15))), state(_, _, in_basket), P).
 * P = P = [go(point(0, 0), point(4, 15)), pickup, go(point(4, 15), point(21, 33)), drop] ;
 * P = [go(point(0, 0), point(4, 15)), pickup, go(point(4, 15), point(21, 33)), drop, push(point(21, 33), _)] ;
 * P = [go(point(0, 0), point(4, 15)), pickup, go(point(4, 15), point(21, 33)), drop, push(point(21, 33), _A), push(_A, _)]
 *
 * Note that after dropping rubbish into the basket robot keeps pushing the basket around
 * making more and more redundand and useless solutions. Let us restrict solution length,
 * once again using our not-actually-breadth-first-search feature with the predicate template/2.
 *
 * The predicate protocol/4 generates protocols of required length.
 *
 * ?- protocol(state(point(0,0),point(21,33),floor(point(4,15))), state(_, _, in_basket), P, 4).
 * P = [go(point(0, 0), point(4, 15)), pickup, go(point(4, 15), point(21, 33)), drop] ;
 * P = [go(point(0, 0), point(21, 33)), push(point(21, 33), point(4, 15)), pickup, drop] ;
 * false.
 * 
 * Here we want to get solutions of length 4, and there are 2 of them.
 * Robot could go to the rubbish, pick it up, then go to the basket
 * location holding rubbish and finally drop it into the basket.
 * The alternative way to achieve the required state with 4 steps is going
 * straight to the basket, push it to the rubbish location, pick up the rubbish
 * and immediately drop it into the basket.
 *
 * We also can use the predicate protocol/4 not knowing exactly how many steps
 * we want the robot do, but we can limit the number of steps, posing a constraint.
 *
 * ?- N #< 5, protocol(state(point(0,0),point(21,33),floor(point(4,15))), state(_, _, in_basket), P, N).
 * N = 4,
 * P = [go(point(0, 0), point(4, 15)), pickup, go(point(4, 15), point(21, 33)), drop] ;
 * N = 4,
 * P = [go(point(0, 0), point(21, 33)), push(point(21, 33), point(4, 15)), pickup, drop] ;
 * false.
 * 
 * Here we want the protocol not be longer than 4 steps.
 */

:- use_module(library(clpfd)).

action(state(Rb, Bs, floor(Rb)),   % Robot and rubbish are at the same position, and rubbish is on the floor
       pickup,                     % Picking up rubbish
       state(Rb, Bs, held)).       % Rubbish now held by robot

action(state(Rb, Rb, held),        % Robot and basket are at the same position
       drop,                       % Dropping rubbish to basket
       state(Rb, Rb, in_basket)).  % Rubbish now in basket

action(state(Rb, Rb, Rs),          % Robot and basket are at the same position
       push(Rb, Next),             % Pushing basket to NextPos
       state(Next, Next, Rs)).     % Now robot and basket are both at NextPos

action(state(Rb, Bs, Rs),          % Going from Rpos to Next
       go(Rb, Next),               % without affecting either
       state(Next, Bs, Rs)).       % basket or rubbish


% Planning a sequence of actions,
% which takes us from starting to
% a desired state

% To achieve State from State itself, do nothing
protocol(State, State, []).
protocol(Curr, Goal, [Action | Ps]) :-
    action(Curr, Action, Next),
    protocol(Next, Goal, Ps).

% Let's copy template/3 here to be able to
% produce generic protocols of required length.
template([], 0).
template([_|Ls], N) :- N #> 0, N0 #= N - 1, template(Ls, N0).

protocol(Start, Goal, Protocol, Len) :-
    template(Protocol, Len),
    protocol(Start, Goal, Protocol).
