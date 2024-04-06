:- module(print_utils, [
          print_result/2]).

:- use_module(large_data).

% print_result(+Schedule, +Cost)
%               +Schedule: list of subschedules for each day of the found schedule
%               +Cost: the penalty value of Schedule
%
% Prints out the schedule and its penalty in correct order
print_result(Schedule, Cost) :-
    format('Total penalty: ~f\n\n', [Cost]),
    print_schedule(Schedule).

print_schedule([]).
print_schedule([day(_, Rooms) | RestScedule]) :- 
    findall(1,                                          % if no exams scheduled for the day - print next ones
            member(room_info(_, [_|_], _, _), Rooms),   % no exams => no not empty lists of events. Not empty is [_|_]
            []),
    print_schedule(RestScedule), !.   
print_schedule([day(LastDay, Rooms) | RestScedule]) :-
    print_schedule(RestScedule),                                                % days are sorted in descendind order, calling recursion at the start will reverse the order
    format('\t*** DAY ~d ***\n\n', [LastDay]),
    print_rooms(Rooms).

print_rooms([]).
print_rooms([room_info(_, [], _, _) | Other_Rooms]) :- print_rooms(Other_Rooms), !.     % absolutely the same logic as print_schedule algorithm
print_rooms([room_info(RoomID, RoomEvents, _, _) | Other_Rooms]) :-
    print_rooms(Other_Rooms),
    format('Room ~d:\n', [RoomID]),
    print_events(RoomEvents), nl.

print_events([]).
print_events([event(EventID, _, _, Start) | RestEvents]) :-
    print_events(RestEvents),                                                           % events are also sorted in descending order by time, so we reverse them
    class(EventID, ExamName),
    exam_duration(EventID, Duration),
    End is Start + Duration,
    teaches(TeacherID, EventID),
    teacher(TeacherID, TeacherName),
    format('\t~d:00 - ~d:00 --- ~s (~s)\n', [Start, End, ExamName, TeacherName]).
    
    
% print_schedule([day(3, [room_info(200, [event("5fbb4f2ea40616944a8e6b73", 200, 3, 13), event("5fbb4f7e86c54afb4b70370d", 200, 3, 10)], 15, 15), room_info(100, [event("5fbb4f91248c761433523323", 100, 3, 10)], 12, 15)]), day(2, []), day(1, [room_info(100, [event("5fbb4f89498d05787f0b1e63", 100, 1, 10)], 12, 12), room_info(200, [], 10, 12)])]).
%         *** DAY 1 ***
% 
% Room 100:
%         10:00 - 12:00 --- Philosophy (Mr Francis)
% 
%         *** DAY 3 ***
% 
% Room 100:
%         10:00 - 12:00 --- Religion (Mr Josef)
% 
% Room 200:
%         10:00 - 12:00 --- Science & Technology (Mr John)
%         13:00 - 15:00 --- Math (Mr John)
% 
% Prints out a schedule presented in list of schedules for days