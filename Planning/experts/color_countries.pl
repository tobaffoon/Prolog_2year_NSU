ngb(portugal, [spain]).
ngb(spain, [portugal, france]).
ngb(france, [spain, switzerland, belgium, germany, italy]).
ngb(belgium, [france, germany, netherlands]).
ngb(netherlands, [belgium, germany]).
ngb(germany, [belgium, netherlands, france, switzerland, austria, denmark]).
ngb(switzerland, [france, germany, austria, italy]).
ngb(austria, [switzerland, germany, italy]).
ngb(italy, [france, switzerland, austria]).
ngb(denmark, [germany]).

color(red).
color(blue).
color(green).
color(yellow).

append_d(L1-L2, L2-T, L1-T).

make_order(Order) :-
    findall((Len, Country), (ngb(Country, Ns), length(Ns, Len)), All),
    max_member((_, MaxNsCount), All),
    order_starting_with([MaxNsCount], [], Order).

make_order1(Order) :-
    findall((Len, Country), (ngb(Country, Ns), length(Ns, Len)), All),
    max_member((_, MaxNsCount), All),
    order_starting_with1([MaxNsCount], [], Order).

order_starting_with([], Order, Order).
order_starting_with([C|Cs], Acc, Order) :-
    memberchk(C/_, Acc), !,
    order_starting_with(Cs, Acc, Order).
order_starting_with([C|Cs], Acc, Order) :-
    ngb(C, Ns),
    append(Ns, Cs, NextCs),
    order_starting_with(NextCs, [C/_|Acc], Order).

order_starting_with1([], _, []).
order_starting_with1([C|Cs], Acc, Order) :-
    memberchk(C/_, Acc), !,
    order_starting_with1(Cs, Acc, Order).
order_starting_with1([C|Cs], Acc, [C/_|Order]) :-
    ngb(C, Ns),
    append(Cs, Ns, NextCs),
    order_starting_with1(NextCs, [C/_|Acc], Order).

coloring([]).
coloring([Country/Color|Cs]) :-
    coloring(Cs),
    color(Color),
    \+ (ngb(Country, Ns), member(C1, Ns), member(C1/Color, Cs)).

assign_colors(Map) :- make_order(Map), coloring(Map).
assign_colors1(Map) :- make_order1(Map), coloring(Map).