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
 * Sample queries:
 *
 * (a) Depart from Riva at 9:10 with destination Malcesine.
 * ?- schedule('Riva' at 9:10, 'Malcesine' at T, S).
 * T = 10:15,
 * S = [depart('Riva'at 9:10), arrive('Torbole'at 9:25),arrive('Malcesine'at 10:15)] ;
 * etc.
 *
 * (b) Can we travel from Campione to Riva, stay at Riva
 *     for at least 45 minutes, and return to Campione
 *     the same day?
 * ?- schedule('Campione' at Start, 'Campione' at End, S),
 *    member(stay('Riva', T1, T2), S),
 *    D .= T2 .- T1, D #>= 45.
 *    S = [
 *         depart('Campione' at 12:55),
 *         arrive('Malcesine' at 13:12),
 *         arrive('Limone' at 13:34),
 *         arrive('Riva' at 14:10),
 *         stay('Riva', 14:10, 15:0),
 *         depart('Riva' at 15:0),
 *         arrive('Limone' at 15:36),
 *         arrive('Malcesine' at 15:57),
 *         arrive('Campione' at 16:13)];
 * etc.
 * 
 * (c) Visit Campione from Riva and return to Riva on the same day.
 * According to the timetable it is not possible, so out query should
 * be answered with false. But since we did not bother to forbid
 * infinite loops we will not get any result, because Prolog will
 * indefinitely keep trying longer and longer schedules, not knowing
 * that it is impossible to construct the right one. We can however
 * limit prevent infinite loops by limiting the allowed length of
 * the schedule. It could be done manually by predefining Schedule as
 * a list of the length not greater than some N, or by calling
 * schedule/4. Look examples below.
 * 
 * ?- schedule('Riva' at Start, 'Riva' at End, Schedule),
 *    member(stay('Campione', F, T), Schedule),
 *    D .= T .- F,
 *    D #> 45.
 * loops indefinitely...
 * 
 * ?- N #< 10, 
 *    schedule('Riva' at Start, 'Riva' at End, Schedule, N),
 *    member(stay('Campione',F,T), Schedule),
 *    D .= T .- F,
 *    D #> 45.
 * false.
 *
 * (d) Start from Riva after 9 am, stay at Limone for lunch for at least
 *     1.5 hours beginning after 11:30 and before 1 pm and finishing
 *     before 2:30 pm, stay at Malcesine for at least 1.5 hours, return
 *     to Riva by 18:30. The trip also must visit Torbole.
 *
 * ?- schedule('Riva' at Start, 'Riva' at End, S),
 *    member(stay('Limone', TL1, TL2), S),
 *    member(stay('Malcesine', TM1, TM2), S),
 *    member(arrive('Torbole' at _), S),
 *    9:00 before Start,
 *    End before 18:30,
 *    11:30 before TL1,
 *    TL1 before 13:00,
 *    DL .= TL2 .- TL1, DL #>= 90,
 *    DM .= TM2 .- TM1, DM #>= 90.
 * S = [
 *       depart('Riva'at 11:45),
 *       arrive('Torbole'at 12:0),
 *       arrive('Limone'at 12:30),
 *       stay('Limone', 12:30, 14:40),
 *       depart('Limone'at 14:40),
 *       arrive('Malcesine'at 15:0),
 *       stay('Malcesine', 15:0, 16:30),
 *       depart('Malcesine'at 16:30),
 *       arrive('Limone'at 16:50),
 *       arrive('Torbole'at 17:20),
 *       arrive('Riva'at 17:35)]
 *
 * It should be admitted that this program is not very efficient.
 * Mostly because of the way that we have been phrasing the queries
 * about suitable schedules. It is much better to check constraints
 * as soon as possible to avoid futile generation of alternatives.
 */

:- use_module(library(clpfd)).

:- op(50,  xfx, : ).     % Colon operator to denote time
:- op(100, xfx, at).     % Relation between towns and the timetable
:- op(400, xfx, before). % Time1 before Time2, example: 15:20 before 16:00


:- op(300, yfx, .-).     % Time difference relation
:- op(350, xfx, .=).     % Compute time difference

.-(H1:M1, H2:M2) :-      % Check if H1:M1 and H2:M2 are correct
    0  #=< H1,           % because we have no interest to compute
    H1 #<  24,           % difference between arguments that are
    0  #=< M1,           % not correct timestamps
    M1 #<  60,
    0  #=< H2,
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


% trip(+PlaceTime1, +PlaceTime2)
% Holds iff Place1 and Place2 are directly connected,
% departing Place1 at Time1 and arriving at Place2
% at Time2. Arguments PlaceTime1 and PlaceTime2 are
% compound terms of a form (Place1 at Time1) and
% (Place2 at Time2) (look operator 'at').
trip(PlaceTime1, PlaceTime2) :- tt(Route), append(_, [PlaceTime1, PlaceTime2|_], Route).

% from_to(+Place1, +Place2, -Schedule)
% Holds if we can sail from Place1 to Place2,
% arriving, departing and staying at places
% we want to, according to the timetable.
from_to(Place, Place, []).      % Already at our destination - do nothing
from_to(Curr, Dest, [arrive(Next)|Route]) :-
    trip(Curr, Next),           % Stay onboard to immediately depart to Next
    from_to(Next, Dest, Route).
from_to(CurrPlace at CurrTime, Dest, [stay(CurrPlace, CurrTime, NextTime)|Route]) :-
    % Stay at a current place from current time to some other time
    trip(CurrPlace at NextTime, _),
    CurrTime before NextTime,
    schedule(CurrPlace at NextTime, Dest, Route).

template([], 0).
template([_|Ls], N) :- N #> 0, N0 #= N - 1, template(Ls, N0).

% schedule(Place1 at Time1, Place2 at Time2, Schedule).
% Holds iff Schedule is a list of transfers and stops
% between Place1 and Place2 at given times, that are
% possible according to the timetable.
% Schedule always starts with a depart, followed by
% an arrive, cause we do not allow schedule to begin
% with 'stay'. The template constructs general routes
% of increasing length, which ensures that short
% schedules are constructed before longer ones.
% There must be a trip in the timetable at start time
% to some other place Next.
schedule(Start, Dest, [depart(Start),arrive(Next)|Route]) :-
    template(Route,_),
    trip(Start,Next),
    from_to(Next,Dest,Route).

% The same as previous with an additional argument N
% allowing to limit the length of the schedule.
schedule(Start, Dest, [depart(Start),arrive(Next)|Route], N) :-
    template(Route,N),
    trip(Start,Next),
    from_to(Next,Dest,Route).
