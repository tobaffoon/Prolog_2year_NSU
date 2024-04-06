:- module('area-map', [neighbor/2, grid_cell/3]).
%
% Area map with 100 cells, from 0 to 99.
% The top left cell is (0,0), the bottom right cell is (9,9).
% Cells enclosed in parentheses are those with cost greater than 1.
% The second scheme shows cost of every cell on the map.
%

:- use_module(library(clpfd)).

% ---> X
% |      0   1    2     3    4   5   6   7   8   9
% |     10  11   12    13   14  15  16  17  18  19
% V     20  21  (22)  (23)  24  25  26  27  28  29
%       30  31  (32)  (33)  34  35  36  37  38  39
% Y     40  41  (42)  (43)  44  45  46  47  48  49
%       50  51  (52)  (53)  54  55  56  57  58  59
%       60  61  (62)   63   64  65  66  67  68  69
%       70  71   72    73   74  75  76  77  78  79
%       80  81   82    83   84  85  86  87  88  89
%       90  91   92    93   94  95  96  97  98  99

% -> X| 0    1   2   3   4   5   6   7   8   9
%-----|----------------------------------------
% |  0| 1    1   1   1   1   1   1   1   1   1
% |  1| 1    1   1   1   1   1   1   1   1   1
% V  2| 1    1  10  12   1   1   1   1   1   1
%    3| 1    1  12  10   1   1   1   1   1   1
% Y  4| 1    1  12  11   1   1   1   1   1   1
%    5| 1    1  14  15   1   1   1   1   1   1
%    6| 1    1  18   1   1   1   1   1   1   1
%    7| 1    1   1   1   1   1   1   1   1   1
%    8| 1    1   1   1   1   1   1   1   1   1
%    9| 1    1   1   1   1   1   1   1   1   1


% neighbor(?N1, ?N2)
% 		N1: id of the first cell
%		N2: id of the second cell
%
%	Predicate holds iff N1 and N2 are neighbors on the map grid. NO DIAGONAL NEIGHBORHOOD
neighbor(N1, N2) :-
    grid_cell(N1, coords(X1,Y1), _),
    grid_cell(N2, coords(X2,Y2), _),
    (
        (
            Y2 #= Y1,
            (X2 #= X1 + 1; X2 #= X1 - 1)
        );
        (
            X2 #= X1,
            (Y2 #= Y1 + 1; Y2 #= Y1 - 1)
        ) 
    ).

% grid_cell(?ID, ?Coordinates, ?Cost)
% 		ID: cell identifier
%		Coordinates: cell coondinates on the grid
%		Cost: cost of traversing the cell
%
%		Definition of the map grid.
grid_cell(0, coords(0,0), 1).
grid_cell(1, coords(1,0), 1).
grid_cell(2, coords(2,0), 1).
grid_cell(3, coords(3,0), 1).
grid_cell(4, coords(4,0), 1).
grid_cell(5, coords(5,0), 1).
grid_cell(6, coords(6,0), 1).
grid_cell(7, coords(7,0), 1).
grid_cell(8, coords(8,0), 1).
grid_cell(9, coords(9,0), 1).

grid_cell(10, coords(0,1), 1).
grid_cell(11, coords(1,1), 1).
grid_cell(12, coords(2,1), 1).
grid_cell(13, coords(3,1), 1).
grid_cell(14, coords(4,1), 1).
grid_cell(15, coords(5,1), 1).
grid_cell(16, coords(6,1), 1).
grid_cell(17, coords(7,1), 1).
grid_cell(18, coords(8,1), 1).
grid_cell(19, coords(9,1), 1).

grid_cell(20, coords(0,2), 1).
grid_cell(21, coords(1,2), 1).
grid_cell(22, coords(2,2), 10).
grid_cell(23, coords(3,2), 12).
grid_cell(24, coords(4,2), 1).
grid_cell(25, coords(5,2), 1).
grid_cell(26, coords(6,2), 1).
grid_cell(27, coords(7,2), 1).
grid_cell(28, coords(8,2), 1).
grid_cell(29, coords(9,2), 1).

grid_cell(30, coords(0,3), 1).
grid_cell(31, coords(1,3), 1).
grid_cell(32, coords(2,3), 12).
grid_cell(33, coords(3,3), 10).
grid_cell(34, coords(4,3), 1).
grid_cell(35, coords(5,3), 1).
grid_cell(36, coords(6,3), 1).
grid_cell(37, coords(7,3), 1).
grid_cell(38, coords(8,3), 1).
grid_cell(39, coords(9,3), 1).

grid_cell(40, coords(0,4), 1).
grid_cell(41, coords(1,4), 1).
grid_cell(42, coords(2,4), 12).
grid_cell(43, coords(3,4), 11).
grid_cell(44, coords(4,4), 1).
grid_cell(45, coords(5,4), 1).
grid_cell(46, coords(6,4), 1).
grid_cell(47, coords(7,4), 1).
grid_cell(48, coords(8,4), 1).
grid_cell(49, coords(9,4), 1).

grid_cell(50, coords(0,5), 1).
grid_cell(51, coords(1,5), 1).
grid_cell(52, coords(2,5), 14).
grid_cell(53, coords(3,5), 15).
grid_cell(54, coords(4,5), 1).
grid_cell(55, coords(5,5), 1).
grid_cell(56, coords(6,5), 1).
grid_cell(57, coords(7,5), 1).
grid_cell(58, coords(8,5), 1).
grid_cell(59, coords(9,5), 1).

grid_cell(60, coords(0,6), 1).
grid_cell(61, coords(1,6), 1).
grid_cell(62, coords(2,6), 18).
grid_cell(63, coords(3,6), 1).
grid_cell(64, coords(4,6), 1).
grid_cell(65, coords(5,6), 1).
grid_cell(66, coords(6,6), 1).
grid_cell(67, coords(7,6), 1).
grid_cell(68, coords(8,6), 1).
grid_cell(69, coords(9,6), 1).

grid_cell(70, coords(0,7), 1).
grid_cell(71, coords(1,7), 1).
grid_cell(72, coords(2,7), 1).
grid_cell(73, coords(3,7), 1).
grid_cell(74, coords(4,7), 1).
grid_cell(75, coords(5,7), 1).
grid_cell(76, coords(6,7), 1).
grid_cell(77, coords(7,7), 1).
grid_cell(78, coords(8,7), 1).
grid_cell(79, coords(9,7), 1).

grid_cell(80, coords(0,8), 1).
grid_cell(81, coords(1,8), 1).
grid_cell(82, coords(2,8), 1).
grid_cell(83, coords(3,8), 1).
grid_cell(84, coords(4,8), 1).
grid_cell(85, coords(5,8), 1).
grid_cell(86, coords(6,8), 1).
grid_cell(87, coords(7,8), 1).
grid_cell(88, coords(8,8), 1).
grid_cell(89, coords(9,8), 1).

grid_cell(90, coords(0,9), 1).
grid_cell(91, coords(1,9), 1).
grid_cell(92, coords(2,9), 1).
grid_cell(93, coords(3,9), 1).
grid_cell(94, coords(4,9), 1).
grid_cell(95, coords(5,9), 1).
grid_cell(96, coords(6,9), 1).
grid_cell(97, coords(7,9), 1).
grid_cell(98, coords(8,9), 1).
grid_cell(99, coords(9,9), 1).