/*
 * Trip planning task.
 *
 * There are five towns on the shore of the lake Lago di Garda:
 * Riva del Garda, Nago Torbole, Limone sul Garda, Campione
 * del Garda and Malcesine.
 * The towns are connected with boat routes. We would like
 * to plan a trip according to map and the timetable of boat
 * transfers.
 *
 * From Riva
 * Riva       8:00  9:10  9:45  11:45  13:10  14:05  15:00  16:20  18:05
 * Torbole     --   9:25 10:00  12:00    -      -      -    16:35  18:20
 * Limone     8:35  9:55 10:30  12:30  13:32  14:40  15:36  17:05  18:50
 * Malcesine  8:55 10:15 10:50  12:50  13:45  15:00  15:57  17:25  19:10
 * Campione    --    -     -      -      -      -    16:13    -      -
 *
 * To Riva
 * Campione    --    -     -    12:55    -      -      -      -      -
 * Malcesine  9:00 10:25 11:25  13:12  13:45  15:05  16:30  18:15  19:15
 * Limone     9:20 10:50 11:45  13:34  13:59  15:25  16:50  18:35  19:35
 * Torbole    9:50 11:20   -      -      -      -    17:20  19:05  20:05
 * Riva      10:05 11:35 12:20  14:10  14:20  16:00  17:35  19:20  20:20
 *
 * Now we want to define a predicate schedule/3 the following way:
 * 
 * schedule(Place1 at Time1, Place2 at Time2, Schedule).
 * 
 * which holds iff Schedule is a list of transfers and stops
 * between Place1 and Place2 at given times, that are
 * possible according to the timetable.
 *
 */

:- use_module(library(clpfd)).

:- op(50,  xfx, : ).     % Colon operator to denote time
:- op(100, xfx, at).     % Relation between towns and the timetable
:- op(400, xfx, before). % Time1 before Time2, example: 15:20 before 16:00


:- op(300, yfx, .-).     % Time difference relation
:- op(350, xfx, .=).     % Compute time difference

.-(H1:M1, H2:M2) :-
    0  #<  H1,
    H1 #<  24,
    0  #=< M1,
    M1 #<  60,
    0  #<  H2,
    H2 #<  24,
    0  #=< M2,
    M2 #<  60.
.=(D, H1:M1 .- H2:M2) :- (H1:M1 .- H2:M2), D #= 60 * (H1 - H2) + M1 - M2.
before(T1, T2) :- D .= T2 .- T1, D #> 0.


% Timetable between the Northern towns of Lago di Garda

% Start from Riva, land in Limone at 8:35 and immediately
% depart to Malcesine arriving at 8:55
tt(['Riva' at 8:00 , 'Limone'  at 8:35 , 'Malcesine' at 8:55]).
tt(['Riva' at 9:10 , 'Torbole' at 9:25 , 'Malcesine' at 10:15]).
tt(['Riva' at 9:45 , 'Torbole' at 10:00, 'Limone'    at 10:30, 'Malcesine' at 10:50]).
tt(['Riva' at 11:45, 'Torbole' at 12:00, 'Limone'    at 12:30, 'Malcesine' at 12:50]).
tt(['Riva' at 13:10, 'Limone'  at 13:32, 'Malcesine' at 13:45]).
tt(['Riva' at 14:05, 'Limone'  at 14:40, 'Malcesine' at 15:00]).
tt(['Riva' at 15:00, 'Limone'  at 15:36, 'Malcesine' at 15:57, 'Campione'  at 16:13]).
tt(['Riva' at 16:20, 'Torbole' at 16:35, 'Limone'    at 17:05, 'Malcesine' at 17:25]).
tt(['Riva' at 18:05, 'Torbole' at 18:20, 'Limone'    at 18:50, 'Malcesine' at 19:10]).


tt(['Malcesine' at 9:00 , 'Limone'    at 9:20 , 'Torbole' at 9:50 , 'Riva' at 10:05]).
tt(['Malcesine' at 10:25, 'Limone'    at 10:50, 'Torbole' at 11:20, 'Riva' at 11:35]).
tt(['Malcesine' at 11:25, 'Limone'    at 11:45, 'Riva'    at 12:20]).
tt(['Campione'  at 12:55, 'Malcesine' at 13:12, 'Limone'  at 13:34, 'Riva' at 14:10]).
tt(['Malcesine' at 13:45, 'Limone'    at 13:59, 'Riva'    at 14:20]).
tt(['Malcesine' at 15:05, 'Limone'    at 15:25, 'Riva'    at 16:00]).
tt(['Malcesine' at 16:30, 'Limone'    at 16:50, 'Torbole' at 17:20, 'Riva' at 17:35]).
tt(['Malcesine' at 18:15, 'Limone'    at 18:35, 'Torbole' at 19:05, 'Riva' at 19:20]).
tt(['Malcesine' at 19:15, 'Limone'    at 19:35, 'Torbole' at 20:05, 'Riva' at 20:20]).

% my trip function
tripp(PlaceTime1, PlaceTime2) :- tt(Path), trip1(PlaceTime1, PlaceTime2, Path).

trip1(PlaceTime1, PlaceTime2, [PlaceTime1, PlaceTime2 | _]).
trip1(PlaceTime1, PlaceTime2, [_, Stop | Tail]) :- trip1(PlaceTime1, PlaceTime2, [Stop | Tail]).

% seminerian trip function
trip(PlaceTime1, PlaceTime2) :- tt(Path), append(_, [PlaceTime1, PlaceTime2 | _], Path).

% from_to(Start, Dest, Schedule).
% Schedule may include waiting
from_to(Dest, Dest, []).
from_to(Curr, Dest, [arrive(Next) | Path]) :-
    trip(Curr, Next),
    from_to(Next, Dest, Path).
from_to(CurrPlace at CurrTime, Dest, [stay(CurrPlace, CurrTime, TillTime) | Path]) :-
    trip(CurrPlace at TillTime, _),
    CurrTime before TillTime,
    schedule(CurrPlace at TillTime, Dest, Path).


schedule(Start, Dest, [depart(Start), arrive(Next) | Path]) :-
    trip(Start, Next),
    from_to(Next, Dest, Path).