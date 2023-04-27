:- module(power_without_assertions2,[powers/3],[assertions, nativeprops]).

:- pred powers(A,B,C) : (list(num,A), num(B), var(C)) => list(num,C).

powers(Factors,N,Powers) :-
    qsort(Factors, SFactors),
    create_pairs(SFactors,Pairs),
    first_powers(N,Pairs,Powers).

min([H], H, []).
min([H|L], M, [H|R]) :- min(L, M, R), H >= M.
min([H|L], H, [M|R]) :- min(L, M, R), H < M.

qsort([],[]).
qsort(L, [M|S]) :- min(L, M, R), qsort(R, S).

create_pairs([X|R],[(X,X)|S]) :- create_pairs_(R,S).

create_pairs_([],[]).
create_pairs_([X|R],[(X,X)|S]) :- create_pairs_(R,S).

first_powers(N,[(Power,Factor)|PFs],[Power|Powers]) :-
    N > 0, !,
    N1 is N-1,
    remove_power(Power,PFs,PFs1),
    Power1 is Power*Factor,
    sorted_insert(PFs1,(Power1,Factor),PFs2),
    first_powers(N1,PFs2,Powers).
first_powers(0,_,[]).

remove_power(_,[],[]).
remove_power(Power,[(Power1,Factor)|RestOut],[(Power1,Factor)|RestOut]) :-
    Power =\= Power1, !.
remove_power(Power,[_|RestPFsIn],PFsOut) :-
    remove_power(Power,RestPFsIn,PFsOut).

sorted_insert([], X, [X]).
sorted_insert([(X1,F1)|L1], (X,F), [(X,F), (X1,F1)|L1]):- X =< X1, !.
sorted_insert([X1|L1], X, [X1|L]):- sorted_insert(L1, X, L).

