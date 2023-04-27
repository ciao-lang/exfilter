:- module(_, [nrev/2], [assertions,predefres(res_steps)]).

:- use_module(library(assertions/native_props)).

:- entry nrev(A,B) : (ground(A), list(A), var(B)).
:- check comp nrev(A,B) + steps_o(length(A)).

nrev([],[]).
nrev([H|L],R) :-
    nrev(L,R1),
    append(R1,[H],R).

append([],Ys,Ys).
append([X|Xs],Ys,[X|Zs]):- append(Xs,Ys,Zs).
