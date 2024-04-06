/* To initiate Expert System use "start."
 *
 *
 * This expert system focuses on determining the proper Java container. Data is stored utilizing IF-THEN rules listed below.
 * Interface / Class:
 *  1. Map
 *      a. LinkedHashMap
 *      b. TreeMap
 *      b. HashTable
 *      d. HashMap
 *      e. IdentityHashMap
 *  2. Set
 *      a. HashSet
 *      b. LinkedHashSet
 *      c. TreeSet
 *  3. List
 *      a. ArrayList
 *      b. LinkedList
 *  4. Queue
 *      a. Stack
 *      b. Vector
 * 
 * 1. If stores pairs then interface is Map
 * 2. If stores values only and doesn't contains duplicates then interface is Set
 * 3. If stores values only and contains duplicates and synchronized then interface is Queue
 * 4. If stores values only and contains duplicates and unsynchronized then interface is List
 * 5. If interface is Map and ordered and insertion order then class is LinkedHashMap
 * 6. If interface is Map and ordered and sorted by key and synchronized then class is TreeMap
 * 7. If interface is Map and ordered and sorted by key and unsynchronized then class is HashTable
 * 8. If interface is Map and unordered and uses equals() then class is HashMap
 * 9. If interface is Map and unordered and uses "==" then class is IdentityHashMap
 *10. If interface is Set and unordered then class is HashSet
 *10. If interface is Set and ordered and insertion order then class is LinkedHashSet
 *11. If interface is Set and ordered and sorted by value then class is TreeSet
 *12. If interface is Queue and is LIFO then class is Stack
 *14. If interface is Queue and isn't LIFO then class is Vector
 *15. If interface is List and fast random access then class is ArrayList
 *16. If interface is List and fast sequential access then class is LinkedList
 */

:- use_module(library(clpfd)).

% As before we define dynamic procedures for data already derived, for questions already asked
% and for hypothesis already rejected.
:- dynamic derived/1.
:- dynamic asked/1.
:- dynamic rejected/1.

% Syntax of rules and proof trees.
:- op(100, xfx, <==).
:- op(800, fx, if).
:- op(650, xfx, then).
:- op(300, xfy, or).
:- op(200, xfy, and).
:- op(50, fx, non).

% Clear dynamic database on program start up.
:- 
    retractall(derived(_)),
    retractall(rejected(_)),
    retractall(asked(_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         KNOWLEDGE BASE                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- op(100, xfx, be).

% Properties we can ask about. 

% Some are provided with domain.
askable(stored_value, [pairs, values_only]).
askable(sort_type, [insertion, value_sorted, key_sorted]).
askable(comparator, [equals, "=="]).

% Binary properties are provided as is.
askable(containing_duplicates).
askable(synchronized).
askable(ordered).
askable(is_LIFO).
askable(fast_random_access).
askable(fast_sequential_access).

% Production rules

if
    stored_value be pairs
then
    interface be map.

if
    stored_value be values_only and non containing_duplicates
then
    interface be set.

if
    stored_value be values_only and containing_duplicates and synchronized
then
    interface be queue.

if
    stored_value be values_only and containing_duplicates and non synchronized
then
    interface be list.

if
    interface be map and ordered and sort_type be insertion
then 
    class be linkedHashMap.

if
    interface be map and ordered and sort_type be key_sorted and synchronized
then 
    class be treeMap.

if
    interface be map and ordered and sort_type be key_sorted and non synchronized
then 
    class be hashTable.

if
    interface be map and non ordered and comparator be equals
then 
    class be hashMap.

if
    interface be map and non ordered and comparator be "=="
then 
    class be identityHashMap.

if
    interface be set and non ordered
then 
    class be hashSet.

if
    interface be set and ordered and sort_type be insertion
then 
    class be linkedHashSet.

if
    interface be set and ordered and sort_type be value_sorted
then 
    class be treeSet.

if
    interface be queue and is_LIFO
then 
    class be stack.

if
    interface be queue and non is_LIFO
then 
    class be vector.

if
    interface be list and fast_random_access
then 
    class be arrayList.

if
    interface be list and fast_sequential_access
then 
    class be linkedList.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                         KNOWLEDGE BASE                               %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

true(Statement, Proof) :- true(Statement, Proof, []).

true(Statement, Statement <== is_stated, _) :- derived(Statement).
true(S1 and S2, P1 and P2, Trace) :-
                                    true(S1, P1, Trace),
                                    true(S2, P2, Trace).
true(S1 or S2, P, Trace) :-
                                    true(S1, P, Trace) ;
                                    true(S2, P, Trace).

true(non S, non S <== is_stated, Trace) :-
                        \+true(S, _, [if S be false then non S | Trace]).

true(Conclusion, Conclusion <== ConditionProof, Trace) :-
                                                        if Condition then Conclusion,
                                                        true(Condition, ConditionProof, [if Condition then Conclusion | Trace]).
true(Statement, Proof, Trace) :-    Statement = Subject be _,
                                    askable(Subject, Menu),
                                    \+ derived(Statement),
                                    \+ asked(Subject),
                                    ask(Statement, Subject, Proof, Trace, Menu).
% Binary property case
true(Property, Proof, Trace) :-     Property \= _ be _,
                                    askable(Property),
                                    \+ derived(Property),
                                    \+ asked(Property),
                                    ask_bin(Property, Proof, Trace).

% Ask if binary Property is true

ask_bin(Property, Proof, Trace) :-
    format('\nIs it true that collection is ~w? Please answer: \'yes\',\'no\',\'why\'.\n', [Property]),
    read_string(user_input, "\n", "\r\t", _, Answer),
    process_yes_no(Answer, Property, Proof, Trace).

% Ask for properties value chosen from the Menu
%
% ask(+Statement, +Subject, -Proof, -Trace, +Menu)
% Statement: current proposition we are trying to derive, asking a question. For example: whether nostrils are external_tubular ?.
% Subject  : subject of the proposition we ask about. For example if we are asking whether nostrils are external tubular, the subject
%            would be 'nostrils'. When asking about a bill, bill is the subject.
% Proof    : Current proof tree.
% Trace    : Trace of rules we use to answer the WHY-question.
% Menu     : Subject's domain as a list. Domains are defined in predicate askable/2.
ask(Statement, Subject, Proof, Trace, Menu) :-
    format('\nCan you choose option that describes ~w. Please answer with a number or \'why\':\n', [Subject]),
    show_menu(1, Menu),
    read_string(user_input, "\n", "\r\t", _, Answer),
    process(Answer, Statement, Subject, Proof, Trace, Menu).

% show_menu(+Counter, +Alternatives)
% Counter      : number of a current alternative
% Alternatives : list of alternative values to choose from
%
% Shows menu as a enumerated list of values.
% For example:
%
% 1. Val1
% 2. Val2
% 3. Val3

show_menu(_, []).
show_menu(Cnt, [Alt | Tail]) :-
    format("~d. ~w\n", [Cnt, Alt]),
    CntNext #= Cnt + 1,
    show_menu(CntNext, Tail).

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
    show_reasoning_chain(Trace, 0),
    ask(St, S, Proof, Trace, Menu).

process(StrInd, St, S, St <== is_stated, _, Menu) :- 
    atom_number(StrInd, IntInd),
    nth1(IntInd, Menu, Value), !,
    assertz(derived(S be Value)),
    assertz(asked(S)),
    (St = S be Value).                  % Assert that we inferred desired statement 

% Process incorrect answers.
process(_, Statement, Subject, Proof, Trace, Menu) :-
    format('\nPlease answer with a correct number or \'why\':\n'),
    read_string(user_input, "\n", "\r\t", _, Answer),
    process(Answer, Statement, Subject, Proof, Trace, Menu).

% Process binary answers. 
process_yes_no("yes", Prop, Prop <== is_stated, _) :- !,
    assertz(derived(Prop)),
    assertz(asked(Prop)).
process_yes_no("no", Prop, non Prop <== is_stated, _) :- !,
    assertz(derived(non Prop)),             % new code, should be obvious
    assertz(asked(Prop)),
    fail.
process_yes_no("why", Prop, Proof, Trace) :- !,
    show_reasoning_chain(Trace, 0),
    ask_bin(Prop, Proof, Trace).
process_yes_no(_, Prop, Proof, Trace) :-
    format('\nPlease answer: \'yes\',\'no\',\'why\'.\n'),
    read_string(user_input, "\n", "\r\t", _, Answer),
    process_yes_no(Answer, Prop, Proof, Trace).

show_reasoning_chain([], _) :- nl.
show_reasoning_chain([if Cond then Concl | Rules], Indent) :- nl,
                                                        tab(Indent),
                                                        format('To infer `~w`, using rule (if ~w then ~w)', [Concl, Cond, Concl]),
                                                        I1 is Indent + 1,
                                                        show_reasoning_chain(Rules, I1).

derivable(CondPart, Concl, How) :-
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
            clear,
            write('Hi!\n'),
            infer(Conclusion, How),
            format('Conclusion: ~w\nExplanation: \n', [Conclusion]),
            print_explanation(How, 0).

demo :-
        clear,
        assertz(derived(stored_value be pairs)),
        assertz(derived(ordered)),
        assertz(derived(sort_type be insertion)),
        print_derived,
        infer(class be linkedHashMap, How),
        format('Conclusion: ~w\nExplanation:\n', [class be hashMap]),
        print_explanation(How, 0).


print_derived :- forall(derived(Prop), format('Fact: ~w\n', [Prop])).

print_explanation(is_stated, Ind) :- 
                                    tab(Ind),
                                    format('is_stated'), nl.
print_explanation(C1 <== Tail1 and C2, Ind) :-  print_explanation(C1 <== Tail1, Ind),
                                                tab(Ind),
                                                format('and'), nl,
                                                print_explanation(C2, Ind).

print_explanation(Concl <== Tail, Ind) :-   NextInd #= Ind + 1,
                                            tab(Ind),
                                            format('(\n '),
                                            format('~w <== \n', [Concl]),
                                            print_explanation(Tail, NextInd),
                                            tab(Ind),
                                            format(')\n').


clear :- retractall(derived(_)), retractall(rejected(_)), retractall(asked(_)).