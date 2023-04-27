:- module(_,_,[assertions]).

:- doc(filetype, documentation).
:- doc(title, "An Introduction to Dynamic Bug Finding with CiaoPP's Testing Facilities").
:- doc(author, "Esteban Gil").

:- doc(module,"

In this tutorial we will illustrate how CiaoPP's Analyzer/Verifier,
UnitTest, and CiaoTest work together to check Ciao assertions and how
they can be used to find errors in programs. This tutorial goes over
the whole process but concentrates more on the testing (i.e., dynamic)
side of things.

@section{The approach}

First of all, we will start by running our program through CiaoPP's 
analyzer and let it try to check the assertions statically.
Then, we will instruct CiaoPP to run any unit tests that may be present in the source file and, finally,
for those assertions that could not be verified, we will show how CiaoPP can use CiaoTest to
automatically generate goals for that predicate (test cases) that satisfy the assertion 
precondition (i.e., valid call patterns for the predicate) and
execute them to either check that the assertion holds for those cases or find errors.

@section{The example program}

Let us analyze this implementation of quick-sort (predicate @pred{qsort/2}):

```ciao_runnable
:- module(_,_,[assertions,nativeprops]).
:- use_module(library(lists)).
%! \\begin{focus}
:- prop sorted/1.
sorted([]).
sorted([_]).
sorted([X,Y|Ys]) :-
    X=<Y,
    sorted([Y|Ys]).

:- prop list_nnegint(X) + regtype  .  
list_nnegint([]).
list_nnegint([X|T]) :-
       nnegint(X), list_nnegint(T).

:- pred qsort(Xs,Ys) : (list_nnegint(Xs),var(Ys)) => (list_nnegint(Ys),sorted(Ys)) + not_fails.

:- test qsort(A, B) : (A = []) => (B = []) + not_fails.
:- test qsort(A, B) : (A = [1]) => (B = [1]) + not_fails.
:- test qsort(A, B) : (A = [5, 7, 2, 4, 3]) => (B = [2, 3, 4, 5, 7]) + not_fails.

% qsort with a slight mistake: it may fail when there are repeated numbers in the list
min([H], H, []).
min([H|L], M, [H|R]) :- min(L, M, R), H > M. %  (1) should be >= (or =< below)
min([H|L], H, [M|R]) :- min(L, M, R), H < M.

qsort([], []).
qsort(L, [M|S]) :- min(L, M, R), qsort(R, S).
%! \\end{focus}
```

This predicate sorts a given list of integers from lowest to highest.
However, we have introduced an intentional bug @tt{(1 in the listing)} that causes
the program to fail when a list with repeated elements is given.

First of all we can see two user-defined properties:
@tt{list_nnegint/1} checks if the argument is a list of integers,
@tt{sorted/1} checks if the argument is a sorted list.
Properties such as these are normal predicates, but which meet certain conditions (e.g.,
termination) and are marked as such via @tt{prop/1} declarations. Other properties
like @tt{var/1} or @tt{not_fails} are builtins, defined in libraries.
These properties are important because they will be used by CiaoTest
as generators for test cases.

Then we can see a @tt{pred} assertion:

@tt{:- pred qsort(Xs,Ys) : (list_nnegint(Xs),var(Ys)) => (list_nnegint(Ys),sorted(Ys)) + not_fails.}

The assertion has a calls field (the conjunction after ‘:’), a success field (the conjunction
after ‘=>’), and a computational properties field (after ‘+’), where all these
fields are optional. It states that a valid calling mode for @tt{qsort/2} is to invoke it with
its first argument instantiated to an @tt{list_nnegint}, and that it will then return an @tt{list_nnegint} in @tt{Ys},
that this list will be sorted, and that the predicate will not fail. 
For more information about assertions see @ref{The Ciao assertion language}.

After the @tt{pred} assertion we can see three test assertions that the user has included to
check the behavior of the predicate. They cover a few different use cases: empty list, list with
only one element, and a more general case. 

@section{Setting up CiaoPP flags}

In order to carry out the three operations described in the introduction (static checking, 
running unit tests, and test generation) automatically we need to activate a few flags in CiaoPP's
menu. 
Under the @tt{Test assertions} category, we will find the @tt{Run test assertions (run_utests)} and
@tt{Generate tests from check assertions (test_gen)} flags that we need to turn on:

@image{Figs/ciaopp-flag-menu}{650}{375}

Now, when we tell CiaoPP to perform assertion checking, it will first run the usual static analysis
and checking of assertions, then it will run all unit tests present in the program. If at least one of them 
fails, then random test generation is skipped. However, if all unit tests pass, test generation is 
performed as a last step to try to find test cases that make the assertions fail, hence revealing faults 
in our code.

@section{Running assertion checking}

Now let us check the assertion with a type domain (eterms) and a sharing/freeness domain (shfr) 
and see the results.
We can do so by clicking the @image{Figs/quick-ciaoasr}{33}{30} icon in the Emacs interface.

If we were to stop the execution right after assertion checking and 
take a look into the file generated by CiaoPP, among other @tt{true} assertions,
we will find the following assertions:

@exfilter{quicksort_without_tests.pl}{V,foutput=on,filter=check_pred}

We can see that the analyzer did verify that the precondition is a valid calling mode 
(assertion 3), however, it was not able to infer anything about the success field or the 
computational properties field of the assertion (assertions 1 and 2).

CiaoPP is capable of simplifying assertions, meaning that conjunctions
of properties can be broken up and verified individually. For this we
turn on the \"Simplify check assertions\" flag.  After assertion
simplification, the three previous assertions look like this:

```ciao
:- check success qsort(Xs,Ys) %    (1)
   : ( list_nnegint(Xs), var(Ys) )
   => ( sorted(Ys) ).

:- check comp qsort(Xs,Ys) %       (2)
   : ( list_nnegint(Xs), var(Ys) )
   + not_fails.

:- checked calls qsort(Xs,Ys) %    (3)
   : ( list_nnegint(Xs), var(Ys) ).

:- checked success qsort(Xs,Ys) %  (4)
   : ( list_nnegint(Xs), var(Ys) )
   => ( list_nnegint(Ys) ).
```

Now we can see that CiaoPP was able to partially verify the post-condition. It was able
to prove that on success, @tt{Ys} is a list of integers (assertion 4), hence assertion 1
is simplified. 
Two properties still have not been checked, so here is where CiaoTest comes into play.
For the final part of this procedure, the output file of CiaoPP's analysis is going to be 
fed through CiaoTest. For more information on how to use CiaoTest please refer to the
@ref{CiaoTest Tutorial}.

@section{Running unit tests}

After assertion checking, CiaoTest runs all unit tests present in the program:

@exfilter{quicksort.pl}{V,foutput=on,ftesting=on,filter=test}

In this case all tests passed without errors, so random test generation is performed.

@section{Generating Tests}

CiaoTest will read the assertions left to be checked and generate goals for that predicate satisfying 
the assertion precondition and execute them to either check that the assertion holds for those cases 
or find errors.

Lets see what CiaoTest did:

@exfilter{quicksort.pl}{V,ftesting=on,ftest_gen=on,filter=errors}

By default CiaoTest generates 100 cases for each assertion, or stops before if it finds one
case that does not meet the assertion post-condition. 
Keep in mind that the generation is random, so do not expect to get the same
results if you try this yourself, in fact, it may very well be that none of the test cases
generated makes the program fail, so it is recommended to run CiaoTest a couple times or
increase the number of cases to be generated using the @tt{num_test_cases} option in CiaoPP's
flags menu.
For that same reason, it is also important to note that of course even if CiaoTest does not find
any cases that violate the assertion, one cannot affirm that the assertion is true.

The failed test cases that we got are valid calls to @tt{qsort/2} that did not comply with the post-condition,
in particular, they violated the computational properties field, since they where
required to not fail. Thus, they are counter-examples that prove that the remaining part of the assertion does not hold.
Now we are aware that the predicate is not behaving as it should. 
If we look at the input lists that violate the assertion, it is not too difficult too realize that
there are repeated elements in them, and that this may be the source of our problems.

If it is not apparent where the bug is through observation, a good next step
would be to debug the predicate in the interactive source debugger calling it with the
counter-examples that CiaoTest generated for us and look for the point in which the error occurs.

@section{The final output}

If we take a look into CiaoPP's output file now, we will see that some of the assertions
left to be checked after static analysis have been proven false by the counter-examples
found via test generation:

@exfilter{quicksort.pl}{V,foutput=on,ftesting=on,ftest_gen=on,filter=check_pred}

In the computation properties field of the assertions that have been marked as false, we can
see a new property @tt{by/1} that is used to indicate the source of the failure. In
our case, that source is the failed test cases, which are represented by @tt{texec} declarations, 
each identified by a different @tt{id/1}, which is what appears in the @tt{by/1} field.

@section{Summing-up}

In this tutorial we showed how given a buggy program we can follow a simple methodology
to help us spot those bugs. CiaoPP's analyzers and verifiers offer us the static analysis tools to check part of the
assertions and then we can ask it to use CiaoTest to check the remaining unchecked assertions by running the unit
tests present in the program and with assertion-based random test generation.
Depending on the properties involved, this procedure can often be fully automatic, just needing setting the relevant flags.
If failing test cases are found they can be excellent starting points for more classical debugging. 

@section{Bugs and Warnings}

The CiaoTest bundle is still under development. See the internals documentation for known bugs and limitations.
").
