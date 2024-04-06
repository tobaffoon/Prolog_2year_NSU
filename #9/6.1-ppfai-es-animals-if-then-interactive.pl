/*
 * Let us now try to develop an interactive expert system utilizing hybrid chaining approach.
 * It means we start with no information at all about what user wants to know, and should
 * somehow pose hypothesis and get an information to work with.
 *
 * We also want our rules work not only with binary answers like 'yes' or 'no', but take
 * some values from user. The knowledge base we are about to implement consists of the
 * following IF-THEN rules:
 *
 * 1. If nostrils are external tubular and live is at sea and bill is hooked then order is tubenose.
 * 2. If feet are curved talons and bill is sharp hooked then order is falconiforms.
 * 3. if order is tubenose and size is large and wings are long narrow then family is albatross.
 * 4. If order is falconiforms and feed is scavange and wings are broad then family is vulture.
 * 5. If order is falconiforms and wings are long pointed and head is large and tail is narrow at tip then family is falcon.
 * 6. If family is albatross and color is white then bird is laysan albatross.
 * 7. If family is albatross and color is dark then bird is black footed albatross.
 * 8. If order is tubenose and size is medium and flight be flap glide then bird is fulmar.
 * 9. If family is vulture and flight profile is v_shaped then bird is turkey vulture.
 *10. If family is vulture and flight profile is flat then bird is california condor.
 *11. If family is falcon and eats is insects then bird is sparrow hawk.
 *12. If family is falcon and eats is birds then bird is peregrine falcon.
 *
 * As we see, some properties have binary values ('nostrils' are expected to be external tubular or not),
 * and some aren't of binary domain. For example, bird size could either be large, medium, small or plump.
 * When asking user to provide a property's value we should at the same time show the corresponding domain.
 * For example if asking about bill:
 *
 * Of what type bill is ? Please choose one of the below:
 *  1. hooked
 *  2. flat
 *  3. sharp_hooked
 *  4. short
 *
 * We expect user to enter an integer from 1 to 4.
 *
 * Example:
 *
 * ?- start.
 * Hi!
 * 
 * Is it true that nostrils is external_tubular ? Please answer 'yes', 'no' or 'why'.
 * |: yes
 * 
 * Is it true that live is at_sea ? Please answer 'yes', 'no' or 'why'.
 * yes
 * 
 * Of what type bill is? Type an integer or 'why'.
 * 1: hooked
 * 2: flat
 * 3: sharp_hooked
 * 4: short
 * 1
 * 
 * Of what type size is? Type an integer or 'why'.
 * 1: large
 * 2: medium
 * 3: small
 * 4: plump
 * 2
 * 
 * Of what type flight is? Type an integer or 'why'.
 * 1: ponderous
 * 2: powerful
 * 3: agile
 * 4: flap_glide
 * why
 * 
 *    To infer bird be fulmar, using rule
 *    (if order be tubenose and size be medium and flight be flap_glide then bird be fulmar)
 * 
 * Of what type flight is? Type an integer or 'why'.
 * 1: ponderous
 * 2: powerful
 * 3: agile
 * 4: flap_glide
 * 4
 * Conclusion: bird be fulmar
 * Explanation: bird be fulmar<==(bird be fulmar<==(order be tubenose<==nostrils be external_tubular and live be at_sea and bill be hooked)and size be medium and (flight be flap_glide<==was_told))
 * true.
 */

:- use_module(library(clpfd)).

% As before we define dynamic procedures for data already derived, for questions already asked
% and for hypothesis already rejected.
:- dynamic derived/1.
:- dynamic asked/1.
:- dynamic rejected/1.

% Syntax of rules and proof trees.
:- op(800, xfx, <==).
:- op(800, fx, if).
:- op(700, xfx, then).
:- op(300, xfy, or).
:- op(200, xfy, and).

% Clear dynamic database on program start up.
:- 
    retractall(derived(_)),
    retractall(rejected(_)),
    retractall(asked(_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         KNOWLEDGE BASE                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% It would be handy to use binary predicate 'is' to denote properties' values, but,
% alas, 'is'/2 is a built-in ISO predicate, and it cannot be redefined. So we are forced
% to find an alternative. For example we can define a predicate 'be'/2. Using 'be' in rules
% makes them sound like being pronounced by some illiterate barbarians, but nothing better
% comes in mind.
:- op(100, xfx, be).

% Properties we can ask about. Each provided with its' domain. If domain consists of
% a single value then property is a binary one, otherwise we should show a menu.
askable(nostrils, [external_tubular]).                          % Nostrils shape
askable(live, [at_sea]).                                        % Whether lives at sea
askable(bill, [hooked, flat, sharp_hooked, short]).             % Bill shape
askable(eats, [flying_insects, insects, birds]).                % Food type
askable(feet, [webbed, curved_talons, one_long_backward_toe]).  % Feet shape
askable(size, [large, medium, small, plump]).                   % Bird size
askable(wings, [long_narrow, broad, long_pointed]).             % Wings shape
askable(feed, [scavange]).                                      % Feeding type
askable(head, [large, black, green]).                           % Head size
askable(tail, [narrow_at_tip, forked, long_rusty, square]).     % Tail shape
askable(color, [white, dark, mottled_brown]).                   % Bird color
askable(flight, [ponderous, powerful, agile, flap_glide]).      % Flight type
askable(flight_profile, [v_shaped, flat]).                      % Flight profile

% Production rules
if
    nostrils be external_tubular and live be at_sea and bill be hooked
then
    order be tubenose.

if
    feet be curved_talons and bill be sharp_hooked
then
    order be falconiforms.

if
    order be tubenose and size be large and wings be long_narrow
then
    family be albatross.

if
    order be falconiforms and feed be scavange and wings be broad
then
    family be vulture.

if
    order be falconiforms and wings be long_pointed and head be large and tail be narrow_at_tip
then
    family be falcon.

if
    family be albatross and color be white
then
    bird be laysan_albatross.

if
    family be albatross and color be dark
then
    bird be black_footed_albatross.

if
    order be tubenose and size be medium and flight be flap_glide
then
    bird be fulmar.

if
    family be vulture and flight_profile be  v_shaped
then
    bird be turkey_vulture.

if
    family be vulture and flight_profile be flat
then
    bird be california_condor.

if
    family be falcon and eats be insects
then
    bird be sparrow_hawk.

if
    family be falcon and eats be birds
then
    bird be peregrine_falcon.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         KNOWLEDGE BASE                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Backward chain inference engine. Looks familiar with what we already did. Note that here we
% work non only with binary rules, so we need to develop two ways of asking questions and processing
% answers.
true(Statement, Proof) :- 
                            retractall(derived(_)),
                            retractall(asked(_)),
                            retractall(rejected(_)),
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
                                    Statement = Subject be _,
                                    askable(Subject, Menu),
                                    \+ derived(Statement),
                                    \+ asked(Subject),
                                    ask(Statement, Subject, Proof, Trace, Menu).

% Ask question in case of binary property. A property is detected to be binary if its' domain has only one value.
%
% ask(+Statement, +Subject, -Proof, -Trace, +Menu)
% Statement: current proposition we are trying to derive, asking a question. For example: whether nostrils are external_tubular ?.
% Subject  : subject of the proposition we ask about. For example if we are asking whether nostrils are external tubular, the subject
%            would be 'nostrils'. When asking about a bill, bill is the subject.
% Proof    : Current proof tree.
% Trace    : Trace of rules we use to answer the WHY-question.
% Menu     : Subject's domain as a list. Domains are defined in predicate askable/2.
ask(Statement, Subject, Proof, Trace, [Val]) :-
                                format('\nIs it true that ~w is ~w ? Please answer \'yes\', \'no\' or \'why\'.\n',[Subject, Val]),
                                read_string(user_input, "\n", "\r\t", _, Answer),
                                process_yes_no(Answer, Statement, Subject, Proof, Trace, [Val]).

% Same as previous but asking question about non-binary property. It requires showing menu.
ask(Statement, Subject, Proof, Trace, [V,V1|Vs]) :-
                                Menu = [V,V1|Vs],
                                format('\nOf what type ~w is? Type an integer or \'why\'.\n', [Subject]),
                                show_menu(1, Menu),
                                read_string(user_input, "\n", "\r\t", _, Answer),
                                process(Answer, Statement, Subject, Proof, Trace, Menu).

% Shows menu as a enumerated list of values.
% For example:
%
% 1. Val1
% 2. Val2
% 3. Val3
show_menu(_, []).
show_menu(Counter, [V|Vs]) :-
                            format('~d: ~w\n', [Counter, V]),
                            Next #= Counter + 1,
                            show_menu(Next, Vs).

% Process non-binary answer.
%
% process(+Answer, +Statement, +Subject, -Proof, -Trace, +Menu)
% Answer   : an answer got from a user. It could be 'why' or some integer
% Statement: statement we are trying to derive
% Subject  : property we are asking about (nostrils, feet, wings etc).
% Proof    : Statement's proof tree
% Trace    : list of rules we are about to use to derive the Statement
% Menu     : Domain
process("why", St, S, Proof, Trace, Menu) :- !,
                                        show_reasoning_chain(Trace, 0), nl,
                                        ask(St, S, Proof, Trace, Menu).

% Note the last expression in the predicate. We should check if we derived what we are trying to derive.
% Let us suppose we want to confirm a hypothesis that order is tubenose and to do so we ask of what type
% bill is. In order to derive that order is tubenose we expect bill to be hooked, meaning that the
% Statement here is (bill be hooked). But user can choose any value from the menu. Let it be, for example,
% (bill be short). So, the Proposition we have just derived is (bill is short), and Statement we wanted to
% derive is (bill is hooked).
process(StrInd, St, S, Proof <== is_stated, _, Menu) :-
                                            atom_number(StrInd, Index),     % convert string to integer I
                                            nth1(Index, Menu, Answer),      % get I-th element from the domain if it exists
                                            !,
                                            Proposition = S be Answer,      % Make a proposition from user's answer
                                            Proof = Proposition,            % Proof of the proposition is proposition itself
                                            asserta(derived(Proposition)),  % Save the information that the proposition is derived
                                            asserta(asked(S)),              % Save the information that the question was asked
                                            St == Proposition.              % Check if we derived what we want to derive.

% Process incorrect answers.
process(_, Statement, Subject, Proof, Trace, Menu) :-
                                            write('Incorrect answer! Try again, please\n'),
                                            ask(Statement, Subject, Proof, Trace, Menu).

% Process binary answers. Nothing new here. It is the same as we did before.
process_yes_no("yes", S, Subj, S <== is_stated, _, _) :- !,
                                        asserta(derived(S)),
                                        asserta(asked(Subj)).
process_yes_no("no", _, S, _, _, _) :-   !,
                            asserta(asked(S)),
                            fail.
process_yes_no("why", Statement, Subject, Proof, Trace, Menu) :-  !,
                                            show_reasoning_chain(Trace, 0), nl,
                                            ask(Statement, Subject, Proof, Trace, Menu).
process_yes_no(_, Statement, Subject, Proof, Trace, Menu) :-
                        write('Please answer \'yes\', \'no\' or \'why\'!\n'),
                        read_string(user_input, "\n", "\r\t", _, Answer),
                        process_yes_no(Answer, Statement, Subject, Proof, Trace, Menu).


show_reasoning_chain([], _).
show_reasoning_chain([if Cond then Concl | Rules], _) :-
                                                        format('\n   To infer ~w, using rule\n   (if ~w then ~w)', [Concl, Cond, Concl]),
                                                        show_reasoning_chain(Rules, _).


% User interaction section.

% Check if some Conclusion is derivable.
%
% derivable(?Condition, -Conclusion, -ConclusionProofTree)
% Condition          : A piece of currently known information (could be unbound)
% Conclusion         : A hypothesis we want to confirm or reject, based on currently known information
% ConclusionProofTree: If hypothesis is confirmed ConclusionProofTree is a assigned with the corresponding proof tree
derivable(CondPart, Concl, Concl <== How) :-
                                if Cond then Concl,             % looking for a rule to apply
                                contains_term(CondPart, Cond),  % check if Condition occurs in the IF-path of the rule
                                \+ derived(Concl),
                                \+ rejected(Concl),
                                % If Concl is true then the hypothesis is confirmed, otherwise it is rejected
                                (
                                    true(Concl, How, []) -> !, asserta(derived(Concl)); asserta(rejected(Concl)), fail
                                ).

% infer(+CurrentCondition, -Hypothesis, +CurrentExplanation, -NextExplanation)
% CurrentCondition    : A piece of information we use to make a hypothesis
% Hypothesis          : A hypothesis we make based on the CurrentCondition
% CurrentExplanation  : Current proof tree
% NextExplanation     : A proof tree we will make in case the hypothesis is confirmed
infer(Cond, Concl, Prev, Expl) :-
                            derivable(Cond, Concl1, Trace),     % check if hypothesis is derivable
                            !,
                            infer(Concl1, Concl, Trace, Expl) ; % if so, use it as a new condition
                            Expl = Prev,                        % otherwise the final proof tree is equal to the current one
                            Concl = Cond.                       % and return the current Condition as the last information we were able to derive.


infer(Conclusion, Proof) :-
                            infer(_, Conclusion, 'Can\'t infer something from the information provided.' , Proof), !.



start :-
            retractall(derived(_)),
            retractall(rejected(_)),
            retractall(asked(_)),
            write('Hi!\n'),
            infer(Conclusion, How),
            format('Conclusion: ~w\nExplanation: ~w', [Conclusion, How]).


clear :- retractall(derived(_)), retractall(rejected(_)), retractall(asked(_)).
