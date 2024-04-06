/*
 * Consider there are a set of rectangular blocks. It could
 * be parcels or pallets. The question is do they fit into a
 * given container. And if yes, how can they be placed into
 * the container so they fit?
 *
 * Let us be in planar, 2-dimensional space for now.
 *
 * We will represent the dimensions of an object in the program
 * by the predicate d(_x, _y).
 */

:- use_module(library(clpr)).

:- op(400, xfx, inside).
:- op(400, xfx, no_overlap).

% Placing blocks into a box
%
% block(+BlockName, +BlockDim).
% BlockDim = d(_x, _y) - block dimensions (size).
%
%  ___________________
% |                   |
% |                   |
% |                   |
%  -------------------
% y
% ^
% |
% ---> x
%
% There are 4 predefined blocks.

block(b1, d(5.0, 3.0)).
block(b2, d(2.0, 6.0)).
block(b3, d(1.0, 2.4)).
block(b4, d(1.0, 5.0)).

% Also there are 3 different boxes

box(box1, d(6.0, 6.0)).
box(box2, d(7.0, 5.0)).
box(box3, d(6.0, 5.0)).

% Representation of rectangular spaces blocks occupy
% rect(+Pos, +Dim)
% Pos = pos(Rx, Ry)
% Dim = d(_x, _y)
% rect/2 represents a rectangle of size _x, _y at
% position (Rx, Ry).
% The position of a rectangle in a box is determined
% by the coordinates of the left-bottom corner of a
% rectangle, relative to the left-bottom corner of
% the box.


% rot(?Rect, ?RotatedRect)
% Rotation of rectangle in the 2-dimensional space

rot(rect(Pos, Dim), rect(Pos, Dim)). % Zero rotation
rot(rect(Pos, d(_x, _y)), rect(Pos, d(_y, _x))). % Rotated by 90 degrees

% Place block into a box, which means assign a minimal rectangle
% that accomodates a block.
% place_block(+BlockName, -Place).
% Place = rect(Pos, Dim).

place_block(BlockName, rect(Pos, Dim)) :-
    % Get a block
    block(BlockName, BDim),
    % Block could be rotated to fit the position
    % Note that it could be a zero rotation
    rot(rect(Pos, BDim), rect(Pos, Dim)).

% inside(+Rect1, +Rect2)
% The predicate holds iff Rect1 is completely inside Rect2


inside(rect(pos(Rx1, Ry1), d(_x1, _y1)), rect(pos(Rx2, Ry2), d(_x2, _y2))) :-
    {Rx1 >= Rx2,               % Rx1 must be on the right-hand side of Rx2
     Ry1 >= Ry2,
     Rx1 + _x1 =< Rx2 + _x2,   % Rect1 must not go beyond Rect2
     Ry1 + _y1 =< Ry2 + _y2}.


% no_overlap(+Rect1, +Rect2)
% Holds iff Rect1 and Rect2 do not overlap

no_overlap(rect(pos(Rx1, Ry1), d(_x1, _y1)), rect(pos(Rx2, Ry2), d(_x2, _y2))) :-
    { Rx1 + _x1 =< Rx2 ; % Rectangles are left or right of each other
      Rx2 + _x2 =< Rx1 ;
      Ry1 + _y1 =< Ry2 ; % Rectangles are above or below of each other
      Ry2 + _y2 =< Ry1}.

% arrange(+BoxName, -B1, -B2, -B3, -B4)
% Fit 4 blocks into a Box. B1, B2, B3 and B4 are
% rectangles that fit into a box.

arrange(BoxName, B1, B2, B3, B4) :-
    box(BoxName, BoxDim),
    Box = rect(pos(0.0, 0.0), BoxDim),
    place_block(b1, B1), B1 inside Box,
    place_block(b2, B2), B2 inside Box,
    place_block(b3, B3), B3 inside Box,
    place_block(b4, B4), B4 inside Box,
    B1 no_overlap B2,
    B1 no_overlap B3,
    B1 no_overlap B4,
    B2 no_overlap B3,
    B2 no_overlap B4,
    B3 no_overlap B4.
