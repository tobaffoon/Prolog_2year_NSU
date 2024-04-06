:- use_module(library(clpr)).
:- use_module(large_data).
:- use_module(utils).
:- use_module(schedule_rules).
:- use_module(print_utils).

% schedule is [day], where day is a schedule for certain date
%   day = node(dayNumber, [room_info]), where room_info is useful information about certain room for certain day
%       room_info(RID, Exams, AvailableFrom, AvailableTo)
%           RID = room ID: ID
%           Exams = list of exams ID: list[ExamID]
%           AvailableFrom = time from which room is available at certain day: int
%           AvailableTo = time to which room is available at certain day: int   
% 
%       Exams = [], and time contraints equal to classroom_available info, when room wasn't used that day.

% Main function, prints the best possible schedule
create_schedule() :-
    set_prolog_flag(stack_limit, 8294967296),               % 4 Gb
    findall(Exam, exam(Exam, _), Exams), prepare_env(Exams),
    new_rooms(1, InitRooms),
    schedule_traverse([([day(1, InitRooms)], 0)]).

% schedule_traverse(+Frontier)
%       +Frontier:  a list of schedules that we want to test
%                   to test means to check if all exams are scheduled
%                   or added another exam to the schedule
%                   list consist of pairs of schedules and their costs
%                   Initiated with schedule of only first day with no exams
%
% Rest partial schedules, get all derivative schedules and sort them 
% by how much they break soft rules. Finish when all exams are 
% scheduled, because at this point frontier is sorted and the top 
% element is defenitely the best schedule
schedule_traverse([(CompleteSchedule, CompleteCost) | _]) :-	% stop when the next node to traverse is the Goal
    complete(CompleteSchedule),
	print_result(CompleteSchedule, CompleteCost),
	!.
schedule_traverse([(Current, _) | Rest]) :-
	findall(
		(Next, NextCost),                         
		(                                 
			next(Current, Next),
			expanded_cost(Next, NextCost)
		),
		ExpandedSchedules            
	),
	update(ExpandedSchedules, Rest, ExtendedFrontier),
	schedule_traverse(ExtendedFrontier).

% update(+NewNodes, +Frontier, -ExtendedFrontier)
%
% Extend frontier with new nodes.

update([], F, F).                    % If there is nothing to add then ExtendedFrontier = Frontier
update([Node | Ns], Frontier, Extended) :-
	insert(Node, Frontier, Buff),    % add one node to the frontier resulting in Buff
	update(Ns, Buff, Extended).      % add remaining nodes to the Buff


% insert(+Elem, +Sorted, -ExtendedSorted)
%
% Add an element of a form (Node, Cost) to a sorted list of similar pairs.
% 'Sorted' is sorted by the second member of a pair ascending.

insert(X, [], [X]).
insert((X, CostX), [(Y, CostY) | Rest], [(X, CostX), (Y, CostY) | Rest]) :- 
    CostX < CostY,
	!.
insert(X, [Y | Rest], [Y | RestWithXInserted]) :- % otherwise skip Y and insert X into the tail
	insert(X, Rest, RestWithXInserted).