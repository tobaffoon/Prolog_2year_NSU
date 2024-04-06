nf(1, 1).
nf(2, 1).
nf(N, F) :- N #> 2, N1 #= N-1, N2 #= N-2, nf(N1, F1), nf(N2, F2), F #= F1 + F2.

fib(1, 1).
fib(2, 1).
fib(N, F) :- N #> 2, fib(1, N, 1, 1, F).

fib(N, N, _, Acc, Acc).
fib(Ni, N, Acc, F2, F) :- 
	Ni #< N, 
	Nj #= Ni + 1, 
	Acc1 #= Acc + F2, 
	Acc1 #=< F, 
	fib(Nj, N, F2, Acc1, F).