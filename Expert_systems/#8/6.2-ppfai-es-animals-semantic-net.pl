/*
 * We can represent knowledge not only with IF-THEN rules. Another popular knowledge
 * representation framework are semantic networks. A semantic network consists of
 * entities and relations between the entities. It is customary to represent a semantic
 * netword as a graph with nodes corresponding to entities, and relations shown as links
 * labelled by the names of relations.
 *
 * Let us define a network representing the following facts:
 * - A bird is a king of an animal
 * - Birds are normally active at daylight
 * - Flying is the normal moving method of birds
 * - An albatross is a bird
 * - An albatross is black and white in color
 * - Kiwi is a bird
 * - Kiwi active at night
 * - Walking is the moving method of kiwis
 * - Albert is an albatross, and so is Ross
 * - Kim is a kiwi.
 * 
 *                                animal
 *                                  |\
 *                                  | isa 
 *                                  |
 *    daylight <-----active_at-----bird-------moving--------> fly
 *                                  |
 *                   -------isa------------isa--------
 *                   |\                              |\
 *                   |                               |
 *               albatross      night<-active_at----kiwi--color---> brown
 *                   |                               |
 *    ------------------------------         -----------------
 *    |              |\            |\        |               |\
 *  color           isa           isa      moving           isa
 *    |              |             |         |               |\
 *    V              |             |         V               |
 * black_and_white  Albert        Ross      walk            Kim
 *
 * Note that 'isa' sometimes relates a class of objects with a superclass of the class,
 * (like 'animal' is a superclass of 'bird', meaning that a bird is a kind of animal), and
 * sometimes an instace of a class with the class itself (Albert is an albatross).
 */

isa(bird, animal).
isa(albatross, bird).
isa(kiwi, bird).
isa('Albert', albatross).
isa('Ross', albatross).
isa('Kim', kiwi).

active_at(bird, daylight).
active_at(kiwi, night).

moving(bird, fly).
moving(kiwi, walk).

color(albatross, black_and_white).
color(kiwi, brown).

% In addition to the facts, which are explicitly stated, some other facts can be
% inferred from the network. Ways of inferring facts are built into a semantic network.
% A typical built-in principle of inference is inheritance. For example, the fact that
% albatross flies should be inherited from the fact that birds fly. Similarly, through
% inheritance we should be able to infer that Ross and Albert fly, and Kim walks.
%
% Of cause we can state that the method of moving is inherited as:
%
% moving(Entity, Method) :-
%                           isa(Entity, Super),
%                           moving(Super, Method).
%
% It works, but it is a little awkward to state a separate inheritance rules for each
% relation that can be inherited. Therefore, it would be better to state a more general
% rule about facts. Facts can be either stated explicitly in the network or inherited.
%
% With the rule below we can ask about any inheritable relation. For example:
%
% ?- fact(moving('Albert', M)).
% M = fly.
%
% ?- fact(active_at('Kim', A)).
% A = night.
%


fact(Prop) :- call(Prop), !.                   % Fact stated explicitly
fact(Prop) :-
            Prop =.. [R, A1, A2],              % Otherwise look for the fact
            isa(A1, SuperClass),               % in a superclass
            SuperFact =.. [R, SuperClass, A2],
            fact(SuperFact).