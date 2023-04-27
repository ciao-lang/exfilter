:- module(_,[powers/3],[assertions, regtypes, nativeprops]).

:- regtype list_num(X) # "@var{X} is a list of numbers." .
list_num([]).
list_num([X|T]) :-
    num(X),
    list_num(T).

:- regtype list_num1(X) # "@var{X} is a non-empty list of numbers." .  
list_num1([X|T]) :-
    num(X),
    list_num(T).

:- pred powers(A,B,C) : (list_num1(A), num(B), var(C)) => (list_num(C)).

powers(Factors,N,Powers) :-
    qsort(Factors, SFactors),
    create_pairs(SFactors,Pairs),
    first_powers(N,Pairs,Powers).

min([H], H, []).
min([H|L], M, [H|R]) :- min(L, M, R), H >= M.
min([H|L], H, [M|R]) :- min(L, M, R), H < M.

qsort([], []).
qsort(L, [M|S]) :- min(L, M, R), qsort(R, S).

create_pairs([X|R],[(X,X)|S]) :- create_pairs_(R,S).

create_pairs_([],[]).
create_pairs_([X|R],[(X,X)|S]) :- create_pairs_(R,S).


:- regtype num_pair(P) # "@var{P} is a pair @tt{(_,_)}." .
num_pair((X, Y)):- num(X), num(Y).

:- regtype list_pair(L).
list_pair([]).
list_pair([X|Xs]):-
    num_pair(X),
    list_pair(Xs).

:- regtype list_pair1(L).
list_pair1([X|Xs]):-
   num_pair(X),
   list_pair(Xs).


:- pred first_powers(A,B,C) : (num(A), list_pair1(B),var(C)) => (list_num(C)) .

first_powers(N,[(Power,Factor)|PFs],[Power|Powers]) :-
    N > 0, !,
    N1 is N-1,
    remove_power(Power,PFs,PFs1),
    Power1 is Power*Factor,
    sorted_insert(PFs1,(Power1,Factor),PFs2),
    first_powers(N1,PFs2,Powers).
first_powers(0,_,[]).

:- pred remove_power(A,B,C) : (num(A), list_pair(B), var(C)) => list_pair(C) .

remove_power(_, [], []).
remove_power(Power, [(Power1, Factor)|RestOut], [(Power1, Factor)|RestOut]) :-
    Power =\= Power1, !.
remove_power(Power, [_|RestPFsIn], PFsOut) :-
    remove_power(Power, RestPFsIn, PFsOut).

:- pred sorted_insert(A,B,C) : (list_pair(A), num_pair(B), var(C)) => list_pair1(C) .

sorted_insert([], X, [X]).
sorted_insert([(X1,F1)|L1], (X,F), [(X,F), (X1,F1)|L1]):- X =< X1, !.
sorted_insert([X1|L1], X, [X1|L]):- sorted_insert(L1, X, L).
