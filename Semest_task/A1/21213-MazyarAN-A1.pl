%Away in Arles       -> away
%Call of Duty        -> call
%Dessert Oasis       -> dessert
%Wendalissa          -> wendalissa
%Alexis Olson        -> olson
%Danien Hansen       -> hansen
%Roscoe Jones        -> jones
%Zoey Stout          -> stout

:- use_module(library(clpfd)).

books([away, call, dessert, wendalissa]).
authors([olson, hansen, jones, stout]).
copies([12, 19, 26, 33]).

% Из книг Dessert Oasis и Wendalissa какая-то одна была продана 
% в количестве 19 копий, а другая написана автором Alexis Olson.
hint1(Res) :-
    (booksBought(wendalissa, 19, Res),
    writtenBy(dessert, olson, Res));
    (booksBought(dessert, 19, Res),
    writtenBy(wendalissa, olson, Res)).

% Из книг, проданных количеством 33 и 19 копий, какая-то 
% одна написана автором Roscoe Jones, а другая - это Away in Arles.
hint2(Res) :-
    (authorSold(jones, 19, Res), booksBought(away, 33, Res)); 
    (authorSold(jones, 33, Res), booksBought(away, 19, Res)).

% Количество проданных копий книги Wendalissa на 7 больше, 
% чем книги, написанной Zoey Stout.
hint3(Res) :-
    authorSold(stout, N1, Res),
    booksBought(wendalissa, N, Res),
    N #= N1 + 7.

% Проверяет отношение автор-копии
authorSold(A, N, Res) :-
    books(LBooks), copies(LCopies), authors(LAuthors), 
    member(B, LBooks), member(N, LCopies), member(A, LAuthors),
    member(N-B-A, Res).

% Проверяет отношение книга-автор
writtenBy(B, A, Res) :- 
    books(LBooks), copies(LCopies), authors(LAuthors), 
    member(B, LBooks), member(N, LCopies), member(A, LAuthors),
    member(N-B-A, Res).

% Проверяет отношение книга-копии
booksBought(B, N, Res) :- 
    books(LBooks), copies(LCopies), authors(LAuthors),
    member(B, LBooks), member(N, LCopies), member(A, LAuthors),
    member(N-B-A, Res).

% Проверяет отношение копии-книга-автор
bookInfo(N-B-A, Res) :-
    books(LBooks), copies(LCopies), authors(LAuthors),
    member(B, LBooks), member(N, LCopies), member(A, LAuthors),
    member(N-B-A, Res).

% Основная функция
sol(S) :- 
    books(LBooks), copies(LCopies), authors(LAuthors),
    length(S, Int), length(LBooks, Int),
    sol1(LBooks, LAuthors, LCopies, S).

% Служебная функция
sol1([],[],[],_).                           
sol1(Books, Authors, Copies, Res) :-
    member(N, Copies),
    member(B, Books),
    member(A, Authors),
    bookInfo(N-B-A, Res),
    delete(Copies, N, Copies1),
    delete(Books, B, Books1),
    delete(Authors, A, Authors1),
    sol1(Books1, Authors1, Copies1, Res),
    hint1(Res),
    hint2(Res),
    hint3(Res).