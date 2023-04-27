:- module(_,[],[assertions]).
:- doc(filetype, part).

:- doc(title, "A fake tutorial").
:- doc(author, "Some CiaoPP developer").

:- doc(module, "
Lets consider the `app` module with the following code: 
@includecode{code/app.pl}
when analyzed gives the following results:
@exfilter{app.pl}{A,filter=tpred}   

Lets consider the `bugqsort` module with the following code:
@includecode{code/bugqsort.pl}
when analyzed without modes gives the following results:
@exfilter{bugqsort.pl}{A,types=eterms,modes=none,filter=tpred}   
 
Lets consider the `bugsort2` module with the following code:
@includecode{code/bugsort2.pl}
when verified gives the following results:
@exfilter{bugsort2.pl}{V,asr_not_stat_eval=error,filter=warnings}

Lets consider the `bugsort3` module with the following code:
@includecode{code/bugsort3.pl}
when verified gives the following results:
@exfilter{bugsort3.pl}{V,asr_not_stat_eval=error,filter=warnings}

Lets consider the `nrev` module with the following code:
@includecode{code/nrev.pl}
when verified gives the following results:
@exfilter{nrev.pl}{V,ctchecks_intervals=off,filter=errors}

Lets consider the `nrev2` module with the following code:
@includecode{code/nrev2.pl}
when verified gives the following results:
@exfilter{nrev2.pl}{V,ctchecks_intervals=off,filter=errors}

Lets consider the `qsort` module with the following code:
@includecode{code/qsort.pl}
when analyzed with non-failure gives the following results:
@exfilter{qsort.pl}{A,ana_nf=nf,filter=tpred_plus}
when analyzed with eterms gives the following results:
@exfilter{qsort.pl}{A,types=eterms,modes=none,filter=tpred_regtype}
when analyzed with modes gives the following results:
@exfilter{qsort.pl}{A,types=none,modes=shfr,filter=tpred}

Lets consider the `qsort2` module with the following code:
@includecode{code/qsort2.pl}
when analyzed with non-failure and cost gives the following results:
@exfilter{qsort2.pl}{A,ana_nf=nf,ana_cost=resources,filter=tpred_plus}
").

