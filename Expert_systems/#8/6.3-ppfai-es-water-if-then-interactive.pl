/*
 * A major practical disadvantage of the inference procedures we implemented before
 * is that the user has to state all the relevant information as facts in advance,
 * before the reasoning process is started. So the user may state too much or too
 * little. Therefore it would be better for the information to be provided by the
 * user interactively in a dialogue when it is needed.
 * 
 * An expert system not only simulates a resoning process of a human exper, but
 * must be able to explain decisions it made. Two usual types of explanation are
 * called 'how' and 'why' explanation.
 * When the system comes up with an answer, the user may ask: How did you find
 * this answer? The typical explanation consists of presenting the user with the
 * trace of how the answer was derived. Suppose the system has just found that
 * there is a leak in the kitchen and the user is asking 'How?'. The explanation
 * can be along the following line:
 *
 * Because
 * (a) there is a problem in the kitchen, which was concluded from hall wet and bathroom dry,
 *     AND
 * (b) no water came from outside, which was concluded from window closed.
 *
 * Such an explanation is a proof tree of how the final conclusion follows from
 * rules and known pieces of evidence.
 *
 * We can choose to represent the proof tree of a proposition P in one of the
 * following forms depending on the case:
 *
 * (1) If P is a fact then the whole proof tree is just P itself;
 * (2) If P was derived using a rule (IF Cond THEN P) then the proof tree
 *     is P <== ProofOfCond
 * (3) Let P1 and P2 be propositions whose proof trees are PT1 and PT2. If P is
 *     P1 AND P2 then the proof tree is PT1 AND PT2. If P is P1 OR P2 then the
 *     proof tree is either PT1 or PT2.
 */


:- dynamic derived/1.
:- dynamic asked/1.

:- op(800, xfx, <==).
:- op(800, fx, if).
:- op(700, xfx, then).
:- op(300, xfy, or).
:- op(200, xfy, and).

:- retractall(derived(_)), retractall(asked(_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         KNOWLEDGE BASE                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if
    hall_wet and kitchen_dry
then
    leak_in_bathroom.


if
    hall_wet and bathroom_dry
then
    problem_in_kitchen.


if
    window_closed or no_rain
then
    no_water_from_outside.


if
    problem_in_kitchen and no_water_from_outside
then
    leak_in_kitchen.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         KNOWLEDGE BASE                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Facts the system can ask about
askable(hall_wet).
askable(kitchen_dry).
askable(bathroom_dry).
askable(window_closed).
askable(no_rain).


true(Statement, Proof) :- 
                            retractall(derived(_)),
                            retractall(asked(_)),
                            true(Statement, Proof, []).

true(Statement, Statement, _) :- derived(Statement).
true(S1 and S2, P1 and P2, Trace) :-
                                    true(S1, P1, Trace),
                                    true(S2, P2, Trace).
true(S1 or S2, P, Trace) :-
                                    true(S1, P, Trace) ;
                                    true(S2, P, Trace).
true(Conclusion, Conclusion <== ConditionProof, Trace) :-
                                                        if Condition then Conclusion,
                                                        true(Condition, ConditionProof, [if Condition then Conclusion | Trace]).
true(Statement, Proof, Trace) :-
                                    askable(Statement),
                                    \+ derived(Statement),
                                    \+ asked(Statement),
                                    ask(Statement, Proof, Trace).


ask(Statement, Proof, Trace) :-
                                format('\nIs it true that ~w ? Please answer \'yes\', \'no\' or \'why\'.\n',[Statement]),
                                read_string(user_input, "\n", "\r\t", _, Answer),
                                process(Answer, Statement, Proof, Trace).


process("yes", S, S <== was_told, _) :- !,
                                        asserta(derived(S)),
                                        asserta(asked(S)).
process("no", S, _, _) :-   !,
                            asserta(asked(S)),
                            fail.
process("why", Statement, Proof, Trace) :-  !,
                                            show_reasoning_chain(Trace, 0), nl,
                                            ask(Statement, Proof, Trace).
process(_, Statement, Proof, Trace) :-
                        write('Please answer \'yes\', \'no\' or \'why\'!\n'),
                        read_string(user_input, "\n", "\r\t", _, Answer),
                        process(Answer, Statement, Proof, Trace).


show_reasoning_chain([], _).
show_reasoning_chain([if Cond then Concl | Rules], _) :-
                                                        format('\n   To infer ~w, using rule\n   (if ~w then ~w)', [Concl, Cond, Concl]),
                                                        show_reasoning_chain(Rules, _).