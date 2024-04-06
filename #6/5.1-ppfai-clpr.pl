/*
 * Samples of using clpr library to solve
 * various constraint satisfaction problems.
 *
 */


:- use_module(library(clpr)).


/*
 * Let there be 4 tasks a, b, c and d, whose
 * durations are 2, 3, 5 and 4 hours correspondingly.
 * Let there be these precedence constraints among the
 * tasks: task a has to precede tasks b and c, and b has
 * to precede d.
 *
 *    --> b ----> d
 *  /
 * a
 *  \
 *    --> c
 *
 * The problem is to find the start times Ta, Tb, Tc and Td
 * of the corresponding tasks so that the finishing time Tf
 * of the schedule is minimal. We assume the earliest start
 * time is 0.
 */
solve(Ta, Tb, Tc, Td, Tf) :-
    {Ta >= 0,
     Ta + 2 =< Tb,
     Ta + 2 =< Tc,
     Tb + 3 =< Td,
     Tc + 5 =< Tf,
     Td + 4 =< Tf},
    minimize(Tf).

/*
 * A set of constraints is inserted into a clause as
 * a goal enclosed in curly brackets. Individual
 * constraints are separated by commas and semicolons.
 * As in an regular Prolog clauses, a comma means
 * conjunction, and a semicolon means disjunction.
 * The conjunction of constraints C1, C2 and C3 is
 * written as
 *
 * {C1, C2, C3}.
 *
 * Each constraints is of the form
 *
 * E1 Operator E2
 * Both E1 and E2 are usual arithmetic expressions.
 * The operator can be one of the following:
 * =               equation
 * =\=             disequation
 * <, =<, >, >=    inequation
 *
 * There are the build-in predicates facilitating
 * a linear optimizations. This finds the extreme
 * values of a given linear expressions inside the
 * region that satisfies the given linear constraints.
 *
 * minimize(+Expr).
 * maximize(+Expr).
 * Expr is a linear expression in terms of variables
 * appearing in linear constraints.
 * It should be noted, however, that minimize and
 * maximize find the optimum within the current numerical
 * constraints, but not over alternative solutions
 * of Prolog goals.
 *
 * For example:
 *
 * ?- {0 =< X},{X =< 5; X =< 10}, maximize(X).
 * X = 5.0 ;
 * X = 10.0.
 *
 * The results are 2 maximum values for X, one for each
 * of the alternative executions of the disjunction.
 *
 * The predicates sup/2 and inf/2 find the supremum and
 * infimum of an expression. Note that they DO NOT
 * instantiate variables in Expr.
 *
 * For example:
 * {X < 5}, sup(X, Sup).
 * Sup = 5.0, {X < 5.0}.
 */



/*
 * Let us return to computing Fibonacci numbers and define a
 * predicate fib/2 that rewrites a task as a constraint
 * satisfaction problem.
 *
 * Ok, fib/2 looks nice. It can compute n-th Fibonacci numbers
 * and answer the most general queries.
 * For example:
 *
 * ?- fib(15, F).
 * F = 610.0,
 * false.
 *
 * ?- fib(N,F).
 * N = F, F = 0.0 ;
 * N = F, F = 1.0 ;
 * N = 2.0,
 * F = 1.0 ;
 * N = 3.0,
 * F = 2.0 ;
 * N = 4.0,
 * F = 3.0 ;
 * N = F, F = 5.0 ;
 * N = 6.0,
 * F = 8.0 ;
 * N = 7.0,
 * F = 13.0 ;
 * etc.
 *
 * However, as our previous solution, it does not terminate on
 * queries like fib(N, 144) where N is unbound. It happens due
 * to many successive recursions and could be healed with tail
 * recursive solution. What it also does not do is non-termination
 * in case of unsatisfiable queries:
 *
 * ?- fib(N, 4).
 *
 * The program keeps trying to find two consecutive Fibonacci
 * numbers F1 and F2 such that F1+F2 = 4. It keeps generating
 * larger and larger numbers not realizing that once their sum
 * has exceeded 4, it will only be increasing and so it can never
 * become equal to 4.
 */


fib(N, F) :-
    {N = 0, F = 0};
    {N = 1, F = 1};
    {N > 1,
     F = F1 + F2,
     N = N1 + 1,
     N = N2 + 2
    },
    fib(N1, F1),
    fib(N2, F2).

/*
 * Let's add extra constraints: F1 >= N1 and F2 >= N2.
 * Since it is obvious that for each N F_N >= N, our
 * program must always satisfy these constraints.
 *
 * ?- fib2(N, 4).
 * false.
 */

fib2(N, F) :-
    {N = 0, F = 0};
    {N = 1, F = 1};
    {N > 1,
     F = F1 + F2,
     N = N1 + 1,
     N = N2 + 2,
     F1 >= N1,
     F2 >= N2
    },
    fib2(N1, F1),
    fib2(N2, F2).


/*
 * Here is a tail-recursive solution of our CS-problem.
 */

n_fib(N, Fn) :-
    {N = 0, Fn = 0} ;
    {N = 1, Fn = 1} ;
    {N > 1},
    n_fib(N, 1, 0, 1, Fn).
n_fib(N, C, F0, F1, Fn) :-
    {C = N, F1 = Fn};
    {C < N,
     Cn = C + 1,
     F2 = F0 + F1,
     Fn >= F2},
    n_fib(N, Cn, F1, F2, Fn).
