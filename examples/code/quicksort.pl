:- module(_,_,[assertions,nativeprops]).
:- use_module(library(lists)).

:- prop sorted/1.
sorted([]).
sorted([_]).
sorted([X,Y|Ys]) :-
    X=<Y,
    sorted([Y|Ys]).

:- prop list_nnegint(X) + regtype 
# "Verifies that @var{X} is list of non-negative integers." .  
list_nnegint([]).
list_nnegint([X|T]) :-
       nnegint(X), list_nnegint(T).

:- pred qsort(Xs,Ys) : (list_nnegint(Xs),var(Ys)) => (list_nnegint(Ys),sorted(Ys)) + not_fails.

:- test qsort(A, B) : (A = []) => (B = []) + not_fails.
:- test qsort(A, B) : (A = [1]) => (B = [1]) + not_fails.
:- test qsort(A, B) : (A = [5, 7, 2, 4, 3]) => (B = [2, 3, 4, 5, 7]) + not_fails.

% qort with a slight mistake: it may fail when there are repeated numbers in the list
min([H], H, []).
min([H|L], M, [H|R]) :- min(L, M, R), H > M.
min([H|L], H, [M|R]) :- min(L, M, R), H < M.

qsort([], []).
qsort(L, [M|S]) :- min(L, M, R), qsort(R, S).