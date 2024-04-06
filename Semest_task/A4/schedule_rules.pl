:- module(schedule_rules, [
          complete/1,
          next/2,
          new_rooms/2,
          expanded_cost/2]).

:- use_module(large_data).
:- use_module(utils).
:- use_module(cost).

% complete(+Schedule)
%           +Schedule: a schedule to check if it includes all exams
%
% Checks if all exams have been scheduled
% It simply compares number of scheduled events to number of all events
complete(Schedule) :-
    get_scheduled_events(Schedule, ScheduledEvents),
    length(ScheduledEvents, ScheduledEventsNumber),
    findall(1, exam(_, _), Exams),
    length(Exams, ScheduledEventsNumber).

% wraps 'day' and 'room_info' semantics around 'cost' function
expanded_cost(Schedule, Cost) :-
    get_scheduled_events(Schedule, ScheduledEvents),
    cost(schedule(ScheduledEvents), Cost).


% next(+InitSchedule, -NewSchedule)
%       +InitSchedule: a schedule that we want to expand
%       -NewSchedule:  a derivative from InitSchedule
%                      with new exam added
%
% Chooses arbitrary exam, room and time, then checks the strict limitations
%
% NOTE: The room's availability is viewed as one segment that decreases every time a new event is added.
% The left boarder shrinks to the End Hour of a new event. This way program WILL NEVER add new event in between existing ones
% (because the earliest "free" point in time is the end of last event).

% schedule new exam for the current day
next(InitSchedule, NewSchedule) :-
    InitSchedule = [day(CurrentDay, CurrentDayRooms) | RestSchedule],
    get_new_exam(InitSchedule, ExamID),
    choose_classroom_and_time(ExamID, CurrentDayRooms, RoomID, NewStart),
    exam_duration(ExamID, Duration),
    NewFromLimit is NewStart + Duration,                                                                    % Evaluate new time limits (end of new event)
    select(
        room_info(RoomID, PreviousExams, _, ToLimit),                                                    % Find an entry with chosen Room to update event list and AvailableTo
        CurrentDayRooms, 
        room_info(RoomID, [event(ExamID, RoomID, CurrentDay, NewStart) | PreviousExams], NewFromLimit, ToLimit),    % add new info with appended exam list and new time limit
        NewCurrentDayRooms                                                                                          % new schedule for current day
    ),
    NewSchedule = [day(CurrentDay, NewCurrentDayRooms) | RestSchedule].  

% schedule new exam for the next day
next(InitSchedule, NewSchedule) :-
    InitSchedule = [day(CurrentDay, _) | _],
    NextDay is CurrentDay + 1,
    in_exam_season(NextDay),                                    % check that next day is still exam season
    new_rooms(NextDay, NextDayRooms),                           % get empty room_info list
    next([day(NextDay, NextDayRooms) | InitSchedule], NewSchedule).    % get new exam or continue skipping days

% get unscheduled exam
get_new_exam(Schedule, NewEvent) :-
    exam(NewEvent, _),                                                             % choose a class, then findall events with same ID in initial events
    get_scheduled_events(Schedule, ScheduledEvents),
    findall(1,                                                                  % get some events from list of lists of scheduled events
            memberchk(event(NewEvent,_,_,_), ScheduledEvents),                  % and try to find chosen event there
            []).                                                                % If the result is empty, then exam wasn't chosen before

% get all events from schedule
get_scheduled_events(Schedule, ScheduledEvents) :-
    findall(SomeExam,                                                               
            (
                member(day(_, CertainDayRooms), Schedule),                          % get info about rooms for certain day
                member(room_info(_, CertainRoomExams, _, _), CertainDayRooms),      % get exams at certain room
                member(SomeExam, CertainRoomExams)                                  % get an exam that happens at certain day in certain roo,
            ),     
            ScheduledEvents).


% get an available room at suitable time
choose_classroom_and_time(EventID, Rooms_info, ChosenRoom, ChosenTime) :-
    select(room_info(ChosenRoom, _, AvailableFrom, AvailableTo), Rooms_info, Other_Rooms),   % choose a room and info about it
    st_group(EventID, Students),
    check_capacity(Students, ChosenRoom),
    get_start_time(EventID, AvailableFrom, AvailableTo, ChosenTime),
    check_concurrent_events(event(EventID, ChosenRoom, _, ChosenTime), Other_Rooms).

% check if Students fit in Chosen Room
check_capacity(Students, ChosenRoom) :-
    length(Students, StudentsNumber),
    classroom_capacity(ChosenRoom, ChosenRoomCapacity),
    between(0, ChosenRoomCapacity, StudentsNumber).                     % Quantity of students should be lower than room's capacity

% get time when an exam will start
get_start_time(EventID, AvailableFrom, AvailableTo, ChosenTime) :-
    exam_duration(EventID, Duration),
    UpperTimeLimit is AvailableTo - Duration,                           % calculate the latest hour, when exam can begin today
    between(AvailableFrom, UpperTimeLimit, ChosenTime).                 % choose any time in reasonable bounds

% check if scheduled event dosn't have intersecting students with other events at the same time
check_concurrent_events(event(EventID, _, _, ChosenTime), Other_Rooms) :-
    findall(1,
            (
                member(room_info(_, CertainRoomExams, _, _), Other_Rooms),  % get events for certain room 
                member(event(OtherExam, _, _, OtherStartingTime), CertainRoomExams),   % get an exam from another room
                (
                    student_follows_both_classes(EventID, OtherExam);                   % check if events can intersect
                    teacher_teaches_both_classes(EventID, OtherExam)
                ),
                same_time_events(event(EventID, _, _, ChosenTime), event(OtherExam, _, _, OtherStartingTime)) % check events for concurrency
            ),
            []).

% check if two events have intersecting time intervals
same_time_events(event(EventID1, _, _, Start1), event(EventID2, _, _, Start2)) :-
    exam_duration(EventID1, Duration1),
    End1 is Start1 + Duration1,             % There are only 4 options when time intervals intersect:
    exam_duration(EventID2, Duration2),     % [ ]   |   [ ]    |    [  ]    |   []
    End2 is Start2 + Duration2,             %  [ ]  |  [ ]     |     []     |  [  ]
    (                                       % We only need to check when any left boarder is between other boarders
        between(Start1, End1, Start2), dif(Start2, End1);      % also make 'between' predicate half-strict by 'dif' predicate
        between(Start2, End2, Start1), dif(Start1, End2)       % to allow exam to start right at the end of another exam
    ).

% check if day is within exam season
in_exam_season(Day) :-
    ex_season_starts(Start),
    ex_season_ends(End),
    between(Start, End, Day).

% create empty room_info list for empty schedule
new_rooms(Day, Rooms) :-
    findall(room_info(RoomID, [], AvailableFrom, AvailableTo),                      
                (
                    classroom(RoomID, _), 
                    classroom_available(RoomID, Day, AvailableFrom, AvailableTo)
                ), 
                Rooms).

% findall(Exam, exam(Exam, _), Exams), prepare_env(Exams).  
% ====================
% next([day(3, [room_info(200, [event("5fbb4f7e86c54afb4b70370d", 200, 3, 10)], 12, 15), room_info(100, [event("5fbb4f91248c761433523323", 100, 3, 10)], 12, 15)]), day(2, []), day(1, [room_info(100, [event("5fbb4f89498d05787f0b1e63", 100, 1, 10)], 12, 12), room_info(200, [], 10, 12)])], New).
% 
% New = [day(3, [room_info(200, [event("5fbb4f2ea40616944a8e6b73", 200, 3, 12), event("5fbb4f7e86c54afb4b70370d", 200, 3, 10)], 14, 15), room_info(100, [event("5fbb4f91248c761433523323", 100, 3, 10)], 12, 15)]), day(2, []), day(1, [room_info(100, [event("5fbb4f89498d05787f0b1e63", 100, 1, 10)], 12, 12), room_info(200, [], 10, 12)])] ;
% New = [day(3, [room_info(200, [event("5fbb4f2ea40616944a8e6b73", 200, 3, 13), event("5fbb4f7e86c54afb4b70370d", 200, 3, 10)], 15, 15), room_info(100, [event("5fbb4f91248c761433523323", 100, 3, 10)], 12, 15)]), day(2, []), day(1, [room_info(100, [event("5fbb4f89498d05787f0b1e63", 100, 1, 10)], 12, 12), room_info(200, [], 10, 12)])] ;
% New = [day(3, [room_info(200, [event("5fbb4f9ac118ca4b72c8973f", 200, 3, 12), event("5fbb4f7e86c54afb4b70370d", 200, 3, 10)], 14, 15), room_info(100, [event("5fbb4f91248c761433523323", 100, 3, 10)], 12, 15)]), day(2, []), day(1, [room_info(100, [event("5fbb4f89498d05787f0b1e63", 100, 1, 10)], 12, 12), room_info(200, [], 10, 12)])] ;
% New = [day(3, [room_info(200, [event("5fbb4f9ac118ca4b72c8973f", 200, 3, 13), event("5fbb4f7e86c54afb4b70370d", 200, 3, 10)], 15, 15), room_info(100, [event("5fbb4f91248c761433523323", 100, 3, 10)], 12, 15)]), day(2, []), day(1, [room_info(100, [event("5fbb4f89498d05787f0b1e63", 100, 1, 10)], 12, 12), room_info(200, [], 10, 12)])] ;
%   (schedule Math or English for 3rd day at 12 or 13)
%
% New = [day(4, [room_info(100, [], 10, 12), room_info(200, [event("5fbb4f2ea40616944a8e6b73", 200, 4, 10)], 12, 12)]), day(3, [room_info(200, [event("5fbb4f7e86c54afb4b70370d", 200, 3, 10)], 12, 15), room_info(100, [event("5fbb4f91248c761433523323", 100, 3, 10)], 12, 15)]), day(2, []), day(1, [room_info(100, [event("5fbb4f89498d05787f0b1e63", 100, 1, 10)], 12, 12), room_info(200, [], 10, 12)])] ;
% New = [day(4, [room_info(100, [], 10, 12), room_info(200, [event("5fbb4f9ac118ca4b72c8973f", 200, 4, 10)], 12, 12)]), day(3, [room_info(200, [event("5fbb4f7e86c54afb4b70370d", 200, 3, 10)], 12, 15), room_info(100, [event("5fbb4f91248c761433523323", 100, 3, 10)], 12, 15)]), day(2, []), day(1, [room_info(100, [event("5fbb4f89498d05787f0b1e63", 100, 1, 10)], 12, 12), room_info(200, [], 10, 12)])] ;
% New = [day(5, [room_info(100, [], 10, 12), room_info(200, [event("5fbb4f2ea40616944a8e6b73", 200, 5, 10)], 12, 12)]), day(4, [room_info(100, [], 10, 12), room_info(200, [], 10, 12)]), day(3, [room_info(200, [event("5fbb4f7e86c54afb4b70370d", 200, 3, 10)], 12, 15), room_info(100, [event("5fbb4f91248c761433523323", 100, 3, 10)], 12, 15)]), day(2, []), day(1, [room_info(100, [event("5fbb4f89498d05787f0b1e63", 100, 1, 10)], 12, 12), room_info(200, [], 10, 12)])] ;
% New = [day(5, [room_info(100, [], 10, 12), room_info(200, [event("5fbb4f9ac118ca4b72c8973f", 200, 5, 10)], 12, 12)]), day(4, [room_info(100, [], 10, 12), room_info(200, [], 10, 12)]), day(3, [room_info(200, [event("5fbb4f7e86c54afb4b70370d", 200, 3, 10)], 12, 15), room_info(100, [event("5fbb4f91248c761433523323", 100, 3, 10)], 12, 15)]), day(2, []), day(1, [room_info(100, [event("5fbb4f89498d05787f0b1e63", 100, 1, 10)], 12, 12), room_info(200, [], 10, 12)])] ;
%   (or schedule for the next days)
% ====================
% complete([day(3, [room_info(200, [event("5fbb4f9ac118ca4b72c8973f", 200, 3, 12), event("5fbb4f7e86c54afb4b70370d", 200, 3, 10)], 14, 15), room_info(100, [event("5fbb4f91248c761433523323", 100, 3, 10)], 12, 15)]), day(2, []), day(1, [room_info(100, [event("5fbb4f89498d05787f0b1e63", 100, 1, 10)], 12, 12), room_info(200, [], 10, 12)])]).
%
% false.
% (Math isn't scheduled)
%
% complete([day(4, [room_info(100, [], 10, 12), room_info(200, [event("5fbb4f2ea40616944a8e6b73", 200, 4, 10)], 12, 12)]), day(3, [room_info(200, [event("5fbb4f9ac118ca4b72c8973f", 200, 3, 12), event("5fbb4f7e86c54afb4b70370d", 200, 3, 10)], 14, 15), room_info(100, [event("5fbb4f91248c761433523323", 100, 3, 10)], 12, 15)]), day(2, []), day(1, [room_info(100, [event("5fbb4f89498d05787f0b1e63", 100, 1, 10)], 12, 12), room_info(200, [], 10, 12)])]).
% 
% true.
% (now Math is scheduled)
% ====================
% expanded_cost([day(3, [room_info(200, [event("5fbb4f2ea40616944a8e6b73", 200, 3, 12), event("5fbb4f7e86c54afb4b70370d", 200, 3, 10)], 14, 15), room_info(100, [event("5fbb4f91248c761433523323", 100, 3, 10)], 12, 15)]), day(2, []), day(1, [room_info(100, [event("5fbb4f89498d05787f0b1e63", 100, 1, 10)], 12, 12), room_info(200, [], 10, 12)])], Cost).     
%
% Cost = 10.625.
% ====================