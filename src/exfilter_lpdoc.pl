:- module(_, [], [assertions, fsyntax, doc_module]).

:- doc(title, "LPdoc plugin for exfilter").

:- doc(module, "This module defines the LPdoc @tt{exfilter} command.").

:- use_module(engine(io_basic)).
:- use_module(library(lists)).
:- use_module(library(pathnames)).
:- use_module(library(system)).
:- use_module(library(stream_utils)).
:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(classic/classic_predicates)).
:- use_module(library(process)).
:- use_module(lpdoc(autodoc_filesystem), [vpath_indir/1]).

% ---------------------------------------------------------------------------

% LPdoc command @exfilter{File}{Opts}{auto}
% @exfilter{code/app.pl}{A,filter=tpred}{auto}
doc_cmd_type(exfilter(s,s,s)) :- !. % {file}{opts}{auto}
%
doc_cmd_rw(exfilter(File, Opts, "auto"), R) :- !,
    fmt_exfilter(File, Opts, yes, R).

% LPdoc command @exfilter{File}{Opts}
% @exfilter{code/app.pl}{A,filter=tpred}
doc_cmd_type(exfilter(s,s)) :- !. % {file}{opts}
%
doc_cmd_rw(exfilter(File, Opts), R) :- !,
    fmt_exfilter(File, Opts, no, R).

% (common)
fmt_exfilter(File, Opts, Auto, R) :-
    split_file_output(File, NameFile),
    split_opts(Opts, ListOpts),
    concatenate_items(NameFile, ListOpts, Result),
    atom_codes(ResFile, Result),
    process_file_nofail(ResFile, Auto, ResString0),
    strip_blanks(ResString0, ResString),
    select_lang(ListOpts, Lang),
    R = codeblock(Lang, ResString).   
    
% process_file_nofail(+, +, -)
process_file_nofail(F0, Auto, String) :-
    % Resolve F0 path (relative to indir)
    ( path_is_absolute(F0) ->
        F = F0
    ; vpath_indir(InDir),
      path_concat(InDir, F0, F)
    ),
    % Make sure that the output directory exists
    path_split(F, Dir, _),
    ( file_exists(Dir) -> true
    ; mkpath(Dir)
    ),
    %
    ( Auto = yes -> % (filter automatically) % TODO: be aware, this can be slow!
        process_call(path('ciao-exfilter'),[F],[stdout(file(F))]), % TODO: be careful with redirections
        file_to_string(F,String)
    ; file_exists(F) ->
        file_to_string(F,String1),
        ( pending_str(String1) -> missing_str(String) % still pending, show missing
        ; String = String1 % we got it!
        )
    ; pending_str(PendingStr),
      string_to_file(PendingStr, F), % mark as pending
      missing_str(String) % show missing
    ).

pending_str("%%PENDING EXFILTER OUTPUT%%").
missing_str("WARNING: exfilter output for ... is missing, call 'exfilter' to generate it").

split_file_output(File,Name):-
    append(Name, ".pl", File), !.

concatenate_items(NameFile, ListOpts, ResultPath):-
    ListResults = [NameFile|ListOpts],
    concatenate_opts(ListResults, Result),
    append("results/", Result, Result1),
    append(Result1, ".txt", ResultPath).

concatenate_opts([Item], Item) :- !.
concatenate_opts([Item|Items], Cs) :-
    ( append(Item, "--"||Cs2, Cs) -> true
    ; Item = Cs, Cs2 = []
    ),
    concatenate_opts(Items, Cs2).

split_opts([], []) :- !.
split_opts(Cs, [Item|Items]) :-
    ( append(Item0, ","||Cs0, Cs) -> 
        ( append(_,"["||_, Item0) ->
            append(Item1,"],"||Cs1,Cs),
            Cs2 = Cs1,
            append(Item1,"]",Item)
        ; Item = Item0, Cs2 = Cs0
        )
    ; Item = Cs, Cs2 = []
    ),    
    split_opts(Cs2, Items).

select_lang(Opts,Lang):-
    ( member("V",Opts) ->
        ( member("output=on",Opts) ->
            Lang = "ciao"
        ; Lang = "ciao-inferior"
        )    
    ; Lang = "ciao"
    ).

% Strip left and right blanks and newlines from a string
strip_blanks(Str0, Str) :-
    reverse(Str0, Str1),
    strip_lblanks(Str1, Str2),
    reverse(Str2, Str3),
    strip_lblanks(Str3, Str).

strip_lblanks([], []).
strip_lblanks([C|Cs], Ds) :- blank(C), !, strip_lblanks(Cs, Ds).
strip_lblanks(Cs, Cs).
    
blank(0' ).
blank(0'\n).
blank(0'\t).

