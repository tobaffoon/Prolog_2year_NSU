/*
 * Exercises involving arithmetic operations
 */


:- use_module(library(clpfd)).

% Factorial of N
% There is a redundant backtrack after the solution is found
% because of the current goal matching heads of both rules.
% n_factorial/2 does not backtrack because there are no
% rules but the first one mathing the current goal.


factorial(0, 1).
factorial(N, F) :-
    N  #> 0,
    N1 #= N - 1,
    F  #= N * F0,
    factorial(N1, F0).


n_factorial(N, F) :- zcompare(Cmp, N, 0), n_factorial(Cmp, N, F).
n_factorial(=, _, 1).
n_factorial(>, N, F) :-
    N1 #= N - 1,
    F  #= N * F0,
    n_factorial(N1,F0).

% Classic tail recursive factorial with a counter and a buffer
n_fact(0, 1).
n_fact(N, F) :- N #> 0, n_fact(N, 1, 1, F).
n_fact(N, N, F, F).
n_fact(N, C, R, F) :-
    C  #< N,
    Cn #= C + 1,
    Rn #= R * Cn,
    n_fact(N, Cn, Rn, F).

% Fibonacci numbers

% A straightforward solution using CLP(FD) library
% It works well on queries where N is bound or on
% the most general queries like fibonacci(N,F), but
% does not terminate on queries when F is bound and
% N is unbound due to multiple constraints accumulating
% by recursive calls
fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(N, Fn) :-
    N  #> 1,
    Fn #> 0,
    N  #= N1 + 1,
    N  #= N2 + 2,
    Fn #= F0 + F1,
    fibonacci(N1, F0),
    fibonacci(N2, F1).


% More sophisticated solution where there aren't
% multiple recursive calls in one rule.
% It works well enough on queries of all types

n_fib(0, 0).                    % 0th Fibonacci number is 0
n_fib(1, 1).                    % 1st Fibonacci number is 1
n_fib(N, Fn) :-                 % Otherwise N must be greater than 1
    N #> 1,
    n_fib(N, 1, 0, 1, Fn).
% Auxiliary predicate.
% n_fib(?N, +Cnt, +F0, +F1, ?Fn)
% N   : number of Fibonacci we want to compute
% Cnt : recursive calls counter, must be from 1 to N
% F0  : Fibonacci number we got two steps before
% F1  : Fibonacci number we got a step before
% Fn  : N-th Fibonacci number
n_fib(N, N, _, Fn, Fn).         % Stop when Cnt = N, and current F1 is a required Fn
n_fib(N, C, F0, F1, Fn) :-
    C  #< N,                    % prevent backtracking
    Cn #= C + 1,                % next value of a counter is Cnt + 1
    F2 #= F0 + F1,              % current Fibonacci number is F0 + F1
    Fn #>= F2,                  % another check to prevent backtracking (matters in case N is unbound)
    n_fib(N, Cn, F1, F2, Fn).   % proceed further with Cn = C + 1, F0n = F1 and F1n = F0 + F1
