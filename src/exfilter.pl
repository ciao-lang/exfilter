:- module(_, [], [dcg,assertions,regtypes,doccomments,fsyntax,datafacts]).

%! \title Example extraction, execution, and filter tool for Ciao/CiaoPP
%  \author The Ciao Development Team
%
%  \module
%  This is a tool to run Ciao/CiaoPP on a collection of codes and
%  extract fragments of the output (either messages or the final
%  output) suitable for inclusion in manuals and tutorials.
%
%  The output produced by CiaoPP generally contains significant amounts of information,
%  including transformations, static analysis information, assertion checking, or
%  verification counterexamples. These results are typically presented as a new version
%  of the source file annotated with (additional) assertions. The full analysis results
%  produced by CiaoPP can be quite large, and cover the whole file or program. When writing
%  the documentation it is interesting to show only a small fraction of this information
%  at a time, the particular part that helps to understand the topic or step being
%  explained. To do so, we propose a method that includes the use of filters. These
%  filters precisely match developer’s requirements and extract only the selected parts
%  of CiaoPP’s output (e.g. particular properties of a concrete predicate, particular
%  types of assertions, etc.) and ignore the rest of the output. This means that the
%  analysis information can be embedded in human-readable explanations and tailored to
%  the user’s needs.
%
%  The tool accepts a result path file name whose name encodes an
%  input file, a list of actions, options, and filters. For the given
%  parameters it runs CiaoPP and extracts and filter the results into
%  the given result path.
%
%  # Filters
%  - \bf{all} :: keep all data.
%  - \bf{tpred} :: all "true pred" assertions.
%  - \bf{tpred_plus} :: all "true pred" assertions including comp properties.
%  - \bf{tpred_regtype} :: all "true pred" assertions and all regtypes.
%  - \bf{regtype} :: only all regtypes.
%  - \bf{warnings} :: all WARNINGs.
%  - \bf{error} :: all ERRORs.
%  - \bf{check_pred} :: all check assertions.
%  - \bf{warn_error} :: all WARNINGs and all ERRORs.
%  - \bf{all_message} :: all top-level messages
%  - \bf{test} :: all tests.
%
%  ##  More options
%  Include these options with one of the filters for more precise results.
%  - \bf{name = Pred} :: results of a specific predicate, with `Pred` being the predicate.
%  - \bf{assertion = [Terms]} :: assertions that contain a series of terms, where `Terms` is the list of terms to be matched.
%  - \bf{comments = on} :: If we add this option together with the filter check_pred then the comments of the pred assertions will be added as well.
%  - \bf{absdomain = AD} :: `AD` can be `types` or can be `modes`. We have to add this option together with filter tpred, tpred_regtype or tpred_plus to obtain the assertions related to the types or modes.
%
%  # Examples
%  Note that the collected examples need to be in `code/` directory.
%  - If we want to execute CiaoPP with options `A`, `types=eterms`, `modes=none`, filter
%  true assertions, and generate the result in the specified result path the command line to use 
%  should be:
%  ```
%  ciao-exfilter results/bugqsort--A--types=eterms--modes=none--filter=tpred.txt
%  ```
%  - But, if we want filter a specific true assertion, for example qsort, the command line to use should be:
%  ```
%  ciao-exfilter results/bugqsort--A--name=qsort--filter=tpred.txt
%  ```
%  - If we need the assertions that contain the word cost and the word ub, then we need to add the option assertion=[cost,ub]:
%  ```
%  ciao-exfilter results/bugqsort--A--name=qsort--assertion=[cost,ub]--filter=tpred_plus.txt
%  ```
%  - The following call will look automatically in the results/ directory
%  ```
%  ciao-exfilter 
%  ```

:- use_module(library(system_extra), [mkpath/1]).
:- use_module(library(stream_utils)).
:- use_module(library(streams)).
:- use_module(library(write)).
:- use_module(library(format), [format/2]).
:- use_module(library(lists), [member/2, append/3, select/3]).
:- use_module(library(process)).
:- use_module(library(process/process_channel)).
:- use_module(library(system), [file_exists/1]).
:- use_module(library(pathnames), [path_split/3, path_concat/3, path_splitext/3]).
:- use_module(library(read_from_string)).
:- use_module(library(llists), [flatten/2]).
:- use_module(library(regexp/regexp_code)).
:- use_module(library(classic/classic_predicates)).

:- use_module(engine(runtime_control), [set_prolog_flag/2]).

% ----- (loaded dynamically in ciaoppcl_common)
%P% :- use_module(library(compiler/p_unit/p_asr), []).
:- use_module(ciaopp(analyze_driver), []).
:- use_module(ciaopp(transform_driver), []).
:- use_module(ciaopp(auto_interface), []). 
%
:- use_module(engine(stream_basic)).
:- use_module(engine(io_basic)).
:- use_module(library(io_port_reify), [io_once_port_reify/4]).
:- use_module(library(port_reify), [port_call/1]).
:- use_module(library(terms), [atom_concat/2]).

:- use_module(engine(system_info)).

:- use_module(ciaopp(ciaoppcl_common), [
    parse_opts/3, ciaopp_run/2,
    ciaopp_error_message/1
]).

% ---------------------------------------------------------------------------
%! # Help

help("Usage: ciao-exfilter [PATH]

where PATH is the path encoding the result (see documentation).
Then no path is added, the tool looks automatically in the results/ directory.
").

% ---------------------------------------------------------------------------
%! # Main

:- export(main/1).
main(['help']) :- !,
    help(Msg),
    format("~s~n", [Msg]).
main([ResultPath]) :- !,
    run1(ResultPath).
main([]) :- !,
    process_call(path(find), ['results', '-name', '*.txt'], [stdout(atmlist(Files))]),
    filter_analyze_files(Files).
main(_) :- !,
    help(Msg),
    format("~s~n", [Msg]).

filter_analyze_files([]).
filter_analyze_files([File|Files]) :- run1(File), filter_analyze_files(Files).

:- export(filter_analyze/2).
filter_analyze(File, Opts) :-  
    concatenate_items(File, Opts, Result),
    atom_codes(ResFile, Result),
    run2(File,ResFile).

:- export(filter_analyze_exercise_mode/3).
filter_analyze_exercise_mode(File,Answer,Opts):- 
    concatenate_items(File, Opts, Result),
    atom_codes(ResFile, Result),
    decode_params(ResFile, _, _, _, _, _, _, _, _, Sol, Message),
    atom_codes(File1, File),
    atom_concat(SrcResult,'.pl', File1),
    atom_concat(SrcResult,'.txt', OutFile),
    check_solution(File1, Answer, Sol, Message, OutFile),
    give_feedback(File1, OutFile,Answer,Sol).

give_feedback(_, OutFile, _, equal):-
    file_to_string(OutFile, StrOutFile),
    (StrOutFile = [] ->
        string_to_file("Correct", OutFile)
    ;
        string_to_file("Incorrect", OutFile)
    ).

give_feedback(_, OutFile, _, errors):-
    file_to_string(OutFile, StrOutFile),
    (StrOutFile = [] ->
        string_to_file("Correct", OutFile)
    ;
         true
    ).

give_feedback(_, OutFile, _, verify_assert):-
    file_to_string(OutFile, StrOutFile),
    (StrOutFile = [] ->
        string_to_file("Correct", OutFile)
    ;
        true
    ).

give_feedback(_, _, _, analyze).

splitline([], []) :- !.
splitline(Cs, [X|Xs]) :-
    splitline_(Cs, X, Cs2),
    splitline(Cs2, Xs).

splitline_([], [], []).
splitline_([0' |Cs], [], Cs) :- !.
splitline_([C|Cs], [C|Ds], Cs2) :-
    splitline_(Cs, Ds, Cs2).

run1(ResultPath) :-
    decode_params(ResultPath, File, Action, Opts, Filter, NameFilter, Include, Comments, AbstDomain, _, _),
    path_concat('code', File, SrcFile0),
    atom_concat(SrcFile0,'.pl',SrcFile),  
    run_and_filter(SrcFile, Action, Opts, Filter, NameFilter, Include, Comments, AbstDomain, ResultPath).

run2(Code, ResultPath) :-
    decode_params(ResultPath, _, Action,  Opts, Filter, NameFilter, Include, Comments, AbstDomain, _, _),
    % Path to source
    atom_codes(Code1, Code),
    atom_concat(SrcResult,'.pl', Code1),
    atom_concat(SrcResult,'.txt', Result),        
    run_and_filter(Code1, Action, Opts, Filter, NameFilter, Include, Comments, AbstDomain, Result).

concatenate_items(File, Opts, Result1):-
    split_file(File, NameStr),
    split_opts(Opts, ListOpts),
    ListResults = [NameStr|ListOpts],
    concatenate_opts(Result, ListResults),
    append(Result, ".txt", Result1).

split_file(File,NameStr):-
    atom_codes(Atom, File),
    path_splitext(Atom, Name,'.pl'),
    atom_codes(Name, NameStr).

concatenate_opts(Item, [Item]) :- !.
concatenate_opts(Cs, [Item|Items]) :-
    ( append(Item, "--"||Cs2, Cs) -> true
    ; Item = Cs, Cs2 = []
    ),
    concatenate_opts(Cs2, Items).

split_opts([], []):- !.
split_opts(Cs, [Item|Items]) :-
    (append(Item0, ","||Cs0, Cs) -> 
    (append(_,"["||_, Item0) ->
   	append(Item1,"],"||Cs1,Cs),
   	Cs2 = Cs1,
        append(Item1,"]",Item)
    ;
        Item = Item0, Cs2 = Cs0
    )
    ; Item = Cs, Cs2 = []
    ),    
    split_opts(Cs2, Items).

% ---------------------------------------------------------------------------
%! # Decode parameters from input

:- pred decode_params(ResulthPath, File, Action, Opts, Filter, NameFilter, Include, Comments, AbstDomain, Sol, Message)
 # "Extract the @var(File) and split the different elements @var(Opts), @var(Filter) and if there is a @var(NameFilter), @var(Include) or @var{Comments} from the @var(ResultPath)".

decode_params(ResultPath, File, Action,  Opts, Filter, NameFilter, Include, Comments, AbstDomain, Sol, Message) :-
    % Extract name without extension from Path
    path_split(ResultPath, _, Name),
    path_splitext(Name, NameNext, '.txt'),
    atom_codes(NameNext, Cs),
    % Split name in strings separated by '--'
    split_items(Cs, Items),
    % Extract elements:
    %  - first the file name
    %  - any filter=Filter as Filter
    %  - any name=Name as NameFiler
    %  - assertion=Include
    %  - any comments=on/off as comments
    %  - any absdomain=Abstract Domain (E.g. eterms, modes, or nfdet)
    %  - rest are options
    Items = [File0|Opts0],
    atom_codes(File, File0),
    ( select("filter="||Filter0, Opts0, Opts1) ->
        true
    ; Filter0 = "tpred", % Default filter 
      Opts1 = Opts0
    ),
    ( select("name="||NameFilter0, Opts1, Opts2) ->
        true
    ; NameFilter0 = "none", % Default name
      Opts2 = Opts1
    ),
    ( select("assertion=["||Include1, Opts2, Opts3) ->
        append(Include0, "]",Include1)
    ; Include0 = "none", % Default 
      Opts3 = Opts2
    ),
    ( select("comments="||Comments0, Opts3, Opts4) ->
        true
    ; Comments0 = "off", % Default option comments
      Opts4 = Opts3
    ),
    ( select("absdomain="||AbstDomain0, Opts4, Opts5) ->
        true
    ; AbstDomain0 = "none", % Default abstract domain
      Opts5 = Opts4
    ),
    ( select("solution="||Sol0, Opts5, Opts6) ->
        true
    ; Sol0 = "none", % Default solution mode
      Opts6 = Opts5
    ),
    ( select("message="||Message0, Opts6, Opts7) ->
        true
    ;
        Message0 = "none", % Default
        Opts7 = Opts6
    ),
    parse_options(Opts7, Action, Opts),
    atom_codes(Filter, Filter0),
    atom_codes(NameFilter, NameFilter0),
    atom_codes(Include,Include0),
    atom_codes(Comments,Comments0),
    atom_codes(AbstDomain,AbstDomain0),
    atom_codes(Sol, Sol0),
    atom_codes(Message, Message0).

parse_options([], _, []). 
parse_options(["A"|Opts], Action, Flags) :- !,
    Action = '-A',
    parse_options(Opts, Action, Flags).
parse_options(["V"|Opts], Action, Flags) :- !,
    Action = '-V',
    parse_options(Opts, Action, Flags).
parse_options(["O"|Opts], Action, Flags) :- !,
    Action = '-O',
    parse_options(Opts, Action, Flags).
parse_options([SV|Opts], Action, [FV|Flags]) :-
    atom_codes(V,SV),
    atom_concat('-f', V, FV),
    parse_options(Opts, Action, Flags).

% Split a list separated by '--'
split_items([], []) :- !.
split_items(Cs, [Item|Items]) :-
    ( append(Item, "--"||Cs2, Cs) -> true
    ; Item = Cs, Cs2 = []
    ),
    split_items(Cs2, Items).



% ---------------------------------------------------------------------------
%! # Run and filter

:- pred run_and_filter(File, Action, Opts, Filter, NameFilter, Include, Comments, AbstDomain, ResultPath)
   # "Run CiaoPP on @var{File} with the corresponding @var{Action} and @var{Opts} and output the result into @var{ResultPath} and proceed to apply the @var{Filter}".

:- export(run_and_filter/9).
run_and_filter(SrcFile, Action, Opts, Filter, NameFilter, Include, Comments, AbstDomain, ResultPath) :-
    ( Action = '-A' -> OutputSource = out_file % Program Analysis
    ; Action = '-O' -> OutputSource = out_file % Optimization
    ; Action = '-V' -> OutputSource = out_std  % Checking assertions
    ; throw(error(unrecognized_action, run_and_filter/8))
    ),
    ( member('-foutput=on',Opts) ->  RawExt = '.raw.pl'
        ; OutputSource = out_file -> RawExt = '.raw.pl'
    ; RawExt = '.raw.out'
    ),
    atom_concat(ResultPath, RawExt, ResultPathRaw),
    make_file_nofail(ResultPathRaw),
    ciaopp_call(OutputSource, Action, SrcFile, Opts, ResultPathRaw),
    run_filter_on_file(Filter, NameFilter, Include, Comments, AbstDomain, ResultPathRaw, ResultPath).

% Call CiaoPP with the corresponding flags and output the result into a file.
ciaopp_call(out_file, Action, SrcFile, Opts, OutFile) :- !,
    append([Action,SrcFile,'-o',OutFile],Opts,Args),
    format(" -> ciaopp ~q~n", [Args]),
    ciaopp_call_(Args,_).

ciaopp_call(out_std, Action, SrcFile, Opts, OutFile) :- !,
    ( member('-foutput=on',Opts) ->
        append(['-o',OutFile, Action, SrcFile],Opts,Args),
        format(" -> ciaopp ~q~n", [Args]),
        ciaopp_call_(Args,_)
    ; append([Action,SrcFile],Opts,Args),
      format(" -> ciaopp ~q~n", [Args]),
      ciaopp_call_(Args,Out),
      string_to_file(Out,OutFile)
    ).

:- export(ciaopp_call_/2).
ciaopp_call_(Args,Result) :- get_arch(wasm32), !,
    io_once_port_reify(cmdrun_(Args), Port, OutString, ErrString),
    Result = ~append(OutString,ErrString),
    port_call(Port).
ciaopp_call_(Args,Out) :-
    process_call(path(ciaopp),Args,[stderr(stdout), stdout(string(Out))]).

cmdrun_(Args) :-
    catch(cmdrun__(Args), E, ciaopp_error_message(E)).

cmdrun__(Args) :-
    ( parse_opts(Args, Cmd, Flags),
      ( var(Cmd) -> Cmd = help ; true ),  % (default)
      ciaopp_cmd(Cmd, Flags)  ->
        true
    ; display(user_error, '{ERROR: unexpected failure}'), nl(user_error)
    ).

ciaopp_cmd(Cmd, _Flags) :- 
    ( Cmd = help % (use ciaoppcl)
    ; Cmd = toplevel(_)
    ; Cmd = customize_and_preprocess(_)
    ; Cmd = restore_menu(_,_)
    ),
    !,
    display(user_error, '{ERROR: Action unavailable}'), nl(user_error).
ciaopp_cmd(Cmd, Flags) :-
    ciaopp_run(Cmd, Flags).

make_file_nofail(F) :-
    path_split(F, Dir, _),
    ( file_exists(Dir) -> true
    ; mkpath(Dir)
    ),
    ( file_exists(F) -> true
    ; touch(F)
    ).


% ---------------------------------------------------------------------------
%! # Check solution

:- discontiguous(check_solution/5).

:- export(check_solution/5).
:- pred check_solution(SrcFile, Answer, Sol, Message, OutFile) .

check_solution(File, Answer, equal, none, OutFile) :-
    atom_concat(SrcFile,'.pl',File),  % Answer user
    atom_concat(SrcFile,'-solution.pl',SrcFileSol),
    make_file_nofail(SrcFileSol),
    string_to_file(Answer, SrcFileSol), % Solution
    open(File,read,StreamA), % Answer user
    open(SrcFileSol,read,StreamB),
    make_file_nofail(OutFile),
    open(OutFile,write,StreamOut),
    new_queue(QueueA),
    new_queue(QueueB),
    enable_deepfind_syntax,
    diff(StreamA,StreamB,QueueA,QueueB,0,0,1,2,StreamOut),
    close(StreamA),
    close(StreamB),
    close(StreamOut).

enable_deepfind_syntax :-
    % flags for hiord
    set_prolog_flag(read_hiord, on),
    % operators for assertions
    op(975, xfx,(=>)),
    op(978, xfx,(::)),
    op(1150, fx,(decl)),
    op(1150,xfx,(decl)),
    op(1150, fx,(pred)),
    op(1150,xfx,(pred)),
    op(1150, fx,(func)),
    op(1150,xfx,(func)),
    op(1150, fx,(prop)),
    op(1150,xfx,(prop)),
    op(1150, fx,(modedef)),
    op(1150,xfx,(modedef)),
    op(1150, fx,(calls)),
    op(1150,xfx,(calls)),
    op(1150, fx,(success)),
    op(1150,xfx,(success)),
    op(1150, fx,(test)),
    op(1150,xfx,(test)),
    op(1150, fx,(texec)),
    op(1150,xfx,(texec)),
    op(1150, fx,(comp)),
    op(1150,xfx,(comp)),
    op(1150, fx,(entry)),
    op(1150,xfx,(entry)),
    op(1150, fx,(exit)),
    op(1150,xfx,(exit)),
    % operators for regtypes
    op(1150, fx,(regtype)),
    op(1150,xfx,(regtype)).


diff(StreamA,StreamB,QueueA,QueueB,LCA,LCB,IdA,IdB,StreamOut):-
    read_term(StreamA,LineA,[variable_names(DictA)]),
    diff_line(LineA,DictA,StreamA,StreamB,QueueA,QueueB,LCA,LCB,IdA,IdB,StreamOut).

diff_line(end_of_file,_Dict,_Stream,StreamB,_Queue,QueueB,_LC,LCB,_Id,IdB,StreamOut):- !,
    dump(QueueB,IdB,StreamOut),
    output(StreamB,LCB,IdB,StreamOut).
diff_line(LineA,_DictA,StreamA,StreamB,QueueA,QueueB,LCA,LCB,IdA,IdB,StreamOut):-
   split(QueueB,LineA,LeftQueueB,RightQueueB),!,
   LCA1 is LCA+1,
   dump_queues(IdA,IdB,QueueA,LeftQueueB,StreamOut),
   new_queue(NewQueueA),
   diff_next_line(RightQueueB,NewQueueA,StreamB,StreamA,LCB,LCA1,IdB,IdA,StreamOut).
diff_line(LineA,DictA,StreamA,StreamB,QueueA,QueueB,LCA,LCB,IdA,IdB,StreamOut):-
    LCA1 is LCA+1,
    enqueue(QueueA,line(LineA,DictA,LCA1),NewQueueA),
    diff(StreamB,StreamA,QueueB,NewQueueA,LCB,LCA1,IdB,IdA,StreamOut).

diff_next_line(QueueA,QueueB,StreamA,StreamB,LCA,LCB,IdA,IdB,StreamOut):-
    empty_queue(QueueA), !,
    diff(StreamA,StreamB,QueueA,QueueB,LCA,LCB,IdA,IdB,StreamOut).
diff_next_line(QueueA,QueueB,StreamA,StreamB,LCA,LCB,IdA,IdB,StreamOut):-
    diff(StreamB,StreamA,QueueB,QueueA,LCB,LCA,IdB,IdA,StreamOut).

split(Queue,_Line,_LeftQueue,_RightQueue):-
    empty_queue(Queue), !,
    fail.
split(Queue,Line0,LeftQueue,RightQueue):-
    dequeue(Queue,line(Line,_Dict,_LC),NewQueue),
    equal_lines(Line0,Line), !,
    new_queue(LeftQueue),
    RightQueue = NewQueue.
split(Queue,Line,LeftQueue,RightQueue):-
    dequeue(Queue,Item,NewQueue),
    enqueue(NewLeftQueue,Item,LeftQueue),
    split(NewQueue,Line,NewLeftQueue,RightQueue).


new_queue(queue(X,X)).

empty_queue(queue(X,Y)):-  X==Y.

enqueue(queue(H,[X|T]),X,queue(H,T)).

dequeue(queue([X|H],T),X,queue(H,T)).

dump_queues(1,2,QueueA,QueueB,StreamOut):-
    dump_queues_in_order(QueueA,QueueB,StreamOut).
dump_queues(2,1,QueueB,QueueA,StreamOut):-
    dump_queues_in_order(QueueA,QueueB,StreamOut).

dump_queues_in_order(QueueA,QueueB,_StreamOut):-
    empty_queue(QueueA),
    empty_queue(QueueB), !.
dump_queues_in_order(QueueA,QueueB,StreamOut):-
    format("*** diff found:~n",[]),
    dump(QueueA,1,StreamOut),
    dump(QueueB,2,StreamOut).

dump(Queue,_Id,_StreamOut):-
    empty_queue(Queue), !.
dump(Queue,Id,StreamOut):-
    dequeue(Queue,line(Line,Dict,LC),NewQueue),
    dump_line(Line,Dict,LC,Id,StreamOut),
    dump(NewQueue,Id,StreamOut).


output(Stream,LC,Id,StreamOut):-
    read_term(Stream,Line,[variable_names(Dict)]),
    ( Line = end_of_file
    -> true
     ; LC1 is LC+1,
       dump_line(Line,Dict,LC1,Id,StreamOut),
       output(Stream,LC1,Id,StreamOut)
    ).

dump_line(Line,Dict,_,_,StreamOut):-
    unify_vars(Dict),
    format_to_string("~q~n",[Line],String),
    write_string(StreamOut,String).

unify_vars([]).
unify_vars([N=V|Dict]):-
    V='$VAR'(N),
    unify_vars(Dict).

equal_lines(LineA,LineB):-
    variant(LineA,LineB).

variant(Term1,Term2):-
    \+ \+
    (  numbervars(Term1,0,N),
       numbervars(Term2,0,N),
       Term1 = Term2
    ).
    
check_solution(File, _, analyze, Message, Result) :-
    run_and_filter(File, '-A', [], all, _, Message, _, _, Result).
    
check_solution(File, _, errors, Message, Result) :-
    run_and_filter(File, '-V', [], warn_error, _, Message, _, _, Result).

check_solution(File, _, verify_assert, Message, Result):- 
    run_and_filter(File, '-V', ['-foutput=on'], check_pred, _, Message, _, _, Result).
   
% ===========================================================================
%! # Filters

:- pred run_filter_on_file(Filter, NameFilter, Include, Comments, AbstDomain, InFile, OutFile)
   # "Load @var(InFile), apply filters and options and printed on @var(OutFile)".

run_filter_on_file(Filter, NameFilter, Include,  Comments, AbstDomain, InFile, OutFile) :-
    file_to_string(InFile, InStr),
    run_filter(Filter, NameFilter, Include, Comments, AbstDomain, InStr, OutStr),
    string_to_file(OutStr, OutFile).

:- discontiguous(run_filter/7).
% ---------------------------------------------------------------------------
% 'all': keep all data

run_filter(all, none, none, off, none, InStr, OutStr) :- !, OutStr = InStr.

% ---------------------------------------------------------------------------
% 'tpred': all "true pred" assertions

run_filter(tpred, Name, Include, off, AbstDomain,  InStr, OutStr) :- !, tpred(OutStr3, InStr, []),  tpred_name(Name,OutStr3,OutStr2), tpred_include(Include, OutStr2, OutStr1), tpred_AD(AbstDomain, OutStr1, OutStr).

tpred([]) --> \+ [_], !. %To match EOS
tpred(Zs) --> truepred(Ys),!, tpred(Xs), {append(Ys,Xs,Zs)}.
tpred(Xs) --> [_], tpred(Xs).

truepred(Ys) --> ":- true pred ", tpkeep(Xs), {append(":- true pred ",Xs,XXs),  append(XXs,"\n\n",Ys)}.

tpkeep(".") --> ".", eol, !.
tpkeep([Y, X|Xs]) -->  [Y], {Y == 0'. }, [X], {X \= 0'., X \= eol }, tpkeep(Xs).
tpkeep([X|Xs]) -->  [X], {X \= 0'. }, tpkeep(Xs).

eol        --> [0'
].  

% ---------------------------------------------------------------------------
% 'tpred_plus': all "true pred" assertions including comp properties

run_filter(tpred_plus, Name, Include, off, AbstDomain, InStr, OutStr) :- !, tpredp(OutStr3, InStr, []), tpred_name(Name,OutStr3,OutStr2), tpred_include(Include, OutStr2, OutStr1), tpred_AD(AbstDomain, OutStr1, OutStr).


tpredp([]) --> \+ [_], !. %To match EOS
tpredp(Zs) --> truepredp(Ys),!, tpredp(Xs), {append(Ys,Xs,Zs)}.
tpredp(Xs) --> [_], tpredp(Xs).

truepredp(As) --> ":- true pred ", tpkeepp(Xs), tpkeep(Ys),
    { append(":- true pred ",Xs,XXs), append(XXs,Ys,Zs), append(Zs,"\n\n",As) }.

tpkeepp("+") --> "+", !.
tpkeepp([X|Xs]) --> [X], {X \= 0'+}, {X \= 0'. }, tpkeepp(Xs).


% ---------------------------------------------------------------------------
% 'tpred_regtype': all "true pred" assertions and all regtypes

run_filter(tpred_regtype, Name, Include, off, AbstDomain, InStr, OutStr) :- !, tpredreg(OutStr3, InStr, []), tpred_name(Name,OutStr3,OutStr2), tpred_include(Include, OutStr2, OutStr1), tpred_AD(AbstDomain, OutStr1, OutStr).

tpredreg([]) --> \+ [_], !. %To match EOS
tpredreg(Zs) --> truepred(Ys),!, tpredreg(Xs), {append(Ys,Xs,Zs)}.
tpredreg(Zs) --> regpred(Ys),!, regtype__(Xs), {append(Ys,Xs,Zs)}.
tpredreg(Zs) --> rtpred(Ys),!, tpredreg(Xs), {append(Ys,Xs,Zs)}.
tpredreg(Xs) --> [_], tpredreg(Xs).

regpred(As) --> ":- regtype ", tpkeep(Xs),
    { append(":- regtype ",Xs,Ys), append(Ys,"\n\n",As) }.

rtpred(As) --> "rt", rtkeep(Xs), tpkeep(Ys),
    { append("rt",Xs,XXs), append(XXs,Ys,Zs), append(Zs,"\n\n",As) }.

rtkeep(".") --> ".", !.
rtkeep(":-") --> ":-", !.
rtkeep([X|Xs]) --> [X],  {X \= "0':-" }, {X \= "0'." }, rtkeep(Xs).

regtype__([]) --> \+ [_], !. %To match EOS
regtype__(Zs) --> rtpred(Xs), !, tpredreg(Rs), {append(Xs,Rs,Zs)} .
regtype__(Xs) --> [_], regtype_(Xs).

% ---------------------------------------------------------------------------
% 'regtype': only all regtypes

run_filter(regtype, Name, Include, off, none, InStr, OutStr) :- !, regtype(OutStr2, InStr, []),  tpred_name(Name,OutStr2,OutStr1), tpred_include(Include, OutStr1, OutStr).

regtype([]) --> \+ [_], !. %To match EOS
regtype(Zs) --> regpred(Ys), !, regtype_(Rs), {append(Ys,Rs,Zs)}.
regtype(Xs) --> [_], regtype(Xs).

regtype_([]) --> \+ [_], !. %To match EOS
regtype_(Zs) --> rtpred(Xs), !, regtype(Rs), {append(Xs,Rs,Zs)} .
regtype_(Xs) --> [_], regtype_(Xs).

% ---------------------------------------------------------------------------
% warnings: all WARNINGs

run_filter(warnings, none, Include, off, none, InStr, OutStr) :- !, warn(OutStr1, InStr, []), message_include(Include, OutStr1, OutStrPar), strip_closepar(OutStrPar, OutStr).

warn([]) --> \+ [_], !. % To match EOS % TODO: ???
warn(Zs) --> warn_(Ys),!, warn(Xs), {append(Ys,Xs,Zs)}.
warn(Xs) --> [_], warn(Xs).

warn_(As) --> "WARNING", warnkeep(Xs), 
    { append("WARNING",Xs,XXs), strip_blanks(XXs, Zs), append(Zs,"\n\n",As) }.

warnkeep("}") --> eol, eol, "}", !.
warnkeep("}") --> eol, "}", !.
warnkeep("}") --> "}", !.
warnkeep([X|Xs]) --> [X], {X \= 0'} }, warnkeep(Xs).

% ---------------------------------------------------------------------------
% errors: all ERRORs

run_filter(errors, none, Include, off, none, InStr, OutStr) :- !, err(OutStr1, InStr, []), message_include(Include, OutStr1, OutStrPar), strip_closepar(OutStrPar, OutStr).

err([]) --> \+ [_], !. %To match EOS
err(Zs) --> err_(Ys),!, err(Xs), {append(Ys,Xs,Zs)}.
err(Xs) --> [_], err(Xs).

err_(As) --> "ERROR", errkeep(Xs), 
    { append("ERROR",Xs,XXs), strip_blanks(XXs, Zs), append(Zs,"\n\n",As) }.

errkeep("}") --> eol, eol, "}", !.
errkeep("}") --> eol, "}", !.
errkeep("}") --> "}", !.
errkeep([X|Xs]) --> [X], {X \= 0'} }, errkeep(Xs).

% ---------------------------------------------------------------------------
% 'warn_error' : all ERRORs and all WARNINGs
run_filter(warn_error, none, Include, off, none, InStr, OutStr) :- !, run_filter(warnings, none, Include, off, none, InStr, OutStr3), run_filter(errors, none, Include, off, none, InStr, OutStr2), append(OutStr3,OutStr2,OutStr).

% ---------------------------------------------------------------------------
% 'all_message' : all top-level messages including WARNINGs, ERRORs...

run_filter(all_message, none, Include, off, none, InStr, OutStr) :- !, run_filter(warnings, none, Include, off, none, InStr, OutStr1), run_filter(errors, none, Include, off, none, InStr, OutStr2), append(OutStr1,OutStr2,OutStr3), run_filter(notes, none, Include, off, none, InStr, OutStr4),  append(OutStr3,OutStr4,OutStr).

run_filter(notes, none, Include, off, none, InStr, OutStr) :- !, note(OutStr1, InStr, []),  message_include(Include, OutStr1, OutStrPar), strip_closepar(OutStrPar, OutStr).

note([]) --> \+ [_], !. %To match EOS
note(Zs) --> note_(Ys),!, note(Xs), {append(Ys,Xs,Zs)}.
note(Xs) --> [_], note(Xs).

note_(As) --> "NOTE", notekeep(Xs), 
    { append("NOTE",Xs,XXs), strip_blanks(XXs, Zs), append(Zs,"\n\n",As) }.

notekeep("}") --> eol, eol, "}", !.
notekeep("}") --> eol, "}", !.
notekeep("}") --> "}", !.
notekeep([X|Xs]) --> [X], {X \= 0'} }, notekeep(Xs).

% ---------------------------------------------------------------------------
% check_pred: all check assertions

run_filter(check_pred, none, none, on, none, InStr, OutStr) :- !, checkassrtC(OutStr, InStr, []).
run_filter(check_pred, none, none, off, none, InStr, OutStr) :- !, checkassrt(OutStr, InStr, []).
run_filter(checked_pred, none, none, off, none, InStr, OutStr) :- !, checkedassrt(OutStr, InStr, []).
run_filter(checked_pred, none, none, on, none, InStr, OutStr) :- !, checkedassrtC(OutStr, InStr, []).
run_filter(not_checked_pred, none, none, off, none, InStr, OutStr) :- !, notcheckedassrt(OutStr, InStr, []).
run_filter(not_checked_pred, none, none, on, none, InStr, OutStr) :- !, notcheckedassrtC(OutStr, InStr, []).

checkassrtC([]) --> \+ [_], !. %To match EOS
checkassrtC(Zs) --> checked_assrt(Ys),!, checkassrtC(Xs), {append(Ys,Xs,Zs)}.
checkassrtC(Zs) --> check_assrt_comment(Ys),!, checkassrtC(Xs), {append(Ys,Xs,Zs)}.
checkassrtC(Zs) --> check_assrt(Ys),!, checkassrtC(Xs), {append(Ys,Xs,Zs)}.
checkassrtC(Zs) --> false_assrt(Ys),!, checkassrtC(Xs), {append(Ys,Xs,Zs)}.
checkassrtC(Zs) --> trust_assrt(Ys),!, checkassrtC(Xs), {append(Ys,Xs,Zs)}.
checkassrtC(Xs) --> [_], checkassrtC(Xs).

checkassrt([]) --> \+ [_], !. %To match EOS
checkassrt(Zs) --> checked_assrt(Ys),!, checkassrt(Xs), {append(Ys,Xs,Zs)}.
checkassrt(Zs) --> check_assrt(Ys),!, checkassrt(Xs), {append(Ys,Xs,Zs)}.
checkassrt(Zs) --> false_assrt(Ys),!, checkassrt(Xs), {append(Ys,Xs,Zs)}.
checkassrt(Zs) --> trust_assrt(Ys),!, checkassrt(Xs), {append(Ys,Xs,Zs)}.
checkassrt(Xs) --> [_], checkassrt(Xs).

checkedassrt([]) --> \+ [_], !. %To match EOS
checkedassrt(Zs) --> checked_assrt(Ys),!, checkedassrt(Xs), {append(Ys,Xs,Zs)}.
checkedassrt(Xs) --> [_], checkedassrt(Xs).

checkedassrtC([]) --> \+ [_], !. %To match EOS
checkedassrtC(Zs) --> checked_assrt(Ys),!, checkedassrtC(Xs), {append(Ys,Xs,Zs)}.
checkedassrtC(Zs) --> check_assrt_comment(Ys),!, checkedassrtC(Xs), {append(Ys,Xs,Zs)}.
checkedassrtC(Xs) --> [_], checkedassrtC(Xs).

notcheckedassrtC([]) --> \+ [_], !. %To match EOS
notcheckedassrtC(Zs) --> check_assrt(Ys),!, notcheckedassrtC(Xs), {append(Ys,Xs,Zs)}.
notcheckedassrtC(Zs) --> false_assrt(Ys),!, notcheckedassrtC(Xs), {append(Ys,Xs,Zs)}.
notcheckedassrtC(Xs) --> [_], notcheckedassrtC(Xs).

notcheckedassrt([]) --> \+ [_], !. %To match EOS
notcheckedassrt(Zs) --> check_assrt(Ys),!, notcheckedassrt(Xs), {append(Ys,Xs,Zs)}.
notcheckedassrt(Zs) --> false_assrt(Ys),!, notcheckedassrt(Xs), {append(Ys,Xs,Zs)}.
notcheckedassrt(Xs) --> [_], notcheckedassrt(Xs).

check_assrt_comment(Ys) -->  "%% %% :- check ", tpkeep(Xs), {append("%% %% :- check ",Xs,XXs),  append(XXs,"\n\n",Ys)}.
check_assrt(_) --> "%% %% :- check ".
check_assrt(Ys) --> ":- check ", tpkeep(Xs), {append(":- check ",Xs,XXs),  append(XXs,"\n\n",Ys)}.
checked_assrt(Ys) --> ":- checked ", tpkeep(Xs), {append(":- checked ",Xs,XXs),  append(XXs,"\n\n",Ys)}.
false_assrt(Ys) --> ":- false ", tpkeep(Xs), {append(":- false ",Xs,XXs),  append(XXs,"\n\n",Ys)}.
trust_assrt(Ys) --> ":- trust ", tpkeep(Xs), {append(":- trust ",Xs,XXs),  append(XXs,"\n\n",Ys)}.

% ---------------------------------------------------------------------------
% test: all tests

run_filter(test, none, none, off, none, InStr, OutStr) :- !, checkTest(OutStr, InStr, []).

checkTest([]) --> \+ [_], !. %To match EOS
checkTest(Zs) --> checked_assrtTest(Ys),!, checkTest(Xs), {append(Ys,Xs,Zs)}.
checkTest(Zs) --> check_assrtTest(Ys),!, checkTest(Xs), {append(Ys,Xs,Zs)}.
checkTest(Zs) --> false_assrtTest(Ys),!, checkTest(Xs), {append(Ys,Xs,Zs)}.
checkTest(Xs) --> [_], checkTest(Xs).

check_assrtTest(Ys) --> ":- check test ", tpkeep(Xs), {append(":- check test ",Xs,XXs),  append(XXs,"\n\n",Ys)}.
checked_assrtTest(Ys) --> ":- checked test ", tpkeep(Xs), {append(":- checked test ",Xs,XXs),  append(XXs,"\n\n",Ys)}.
false_assrtTest(Ys) --> ":- false test ", tpkeep(Xs), {append(":- false test ",Xs,XXs),  append(XXs,"\n\n",Ys)}.

% ---------------------------------------------------------------------------
% Filter: types or  modes
tpred_AD(AD, OutStr1, OutStr):-
    (AD == none ->
        OutStr = OutStr1
    ;
         AD == types ->
            split_opts("int,flt,num,arithexpression,list,list1,term", List),
            tpred_AD_(OutStr1, List, OutStr)
            ;
                AD == modes ->
                    split_opts("ground, var, mshare", List),
                    tpred_AD_(OutStr1, List, OutStr)
                
        ).

tpred_AD_(OutStr1, List, OutStr):-
    read_assertions(OutStr1,Xs),
    extract_all_assertions_AD(Xs, List, Xs0),
    flatten(Xs0, OutStr).
         
         
% Filter the assertions of a specific predicate
tpred_name(Name,OutStr0,OutStr):-
    (Name == none ->
        OutStr = OutStr0
    ;
        atom_codes(Name,NameString),
        append(":- true pred ",NameString, PredName),
        read_assertions(OutStr0,Xs),
        extract_tpred_name(Xs,PredName,Xs0),
        flatten(Xs0, OutStr)
        ).

% Filter the assertions that include certain words
tpred_include(Include, OutStr1, OutStr):-
    (Include == none ->
        OutStr = OutStr1
    ;
        atom_codes(Include,Include0),
        split_opts(Include0, List),
        read_assertions(OutStr1,Xs),
        extract_all_assertions_include(Xs, List, Xs0),
        flatten(Xs0, OutStr)
    ).

message_include(Include, OutStr1, OutStr):-
    (Include == none ->
        OutStr = OutStr1
    ;
        atom_codes(Include,Include0),
        split_opts(Include0, List),
        read_messages(OutStr1,Xs),
        extract_all_assertions_include(Xs, List, Xs0),
        flatten(Xs0, OutStr)
    ).
    
extract_all_assertions_AD([L|Ls], X, [R|Rs]):-
    extract_assertions_AD(L, X, R),
    extract_all_assertions_AD(Ls, X, Rs).
extract_all_assertions_AD([], _, [[]]).

extract_assertions_AD(L, [X|Rest], Result) :-
    append(".*", X, X0),
    (match_posix_rest(X0, L , _) ->
        Result = L
    ;
        extract_assertions_AD(L, Rest, Result)
    ).
extract_assertions_AD(_, [], [[]]).

extract_all_assertions_include(L, [X|Rest], Result):-
    extract_assertions_include(L, X, Xs),
    extract_all_assertions_include(Xs, Rest, Result).

extract_all_assertions_include(L, [X], Result):-
    extract_assertions_include(L, X, Result).

extract_assertions_include([L|Rest], Include, [X|Xs]):-
    append(".*", Include , Include0),
    (match_posix_rest(Include0, L , _) ->
        X = L,
        extract_assertions_include(Rest, Include, Xs)
    ;
        X = [],
        extract_assertions_include(Rest, Include, Xs)        
    ).
extract_assertions_include([], _, [[]]).

read_assertions([L|Rest], [X|Xs]) :-
    ( char_dot(L), Rest = [L1|Rest1], blank(L1)  ->
        X = [L,L1],
        Rest = [_LF|Rest1],
        read_assertions(Rest1,Xs)
    ; X = [L|Xs0],
      read_assertions(Rest, [Xs0|Xs])
    ).
read_assertions([], [[]]).

read_messages([L|Rest], [X|Xs]) :-
    ( closepar(L) ->
        X = [L],
        read_messages(Rest,Xs)
    ; X = [L|Xs0],
      read_messages(Rest, [Xs0|Xs])
    ).
read_messages([], [[]]).

extract_tpred_name([L|Rest],Name,[Pred|Xs]):-
      append(".*", Name , Name0),
      (match_posix_rest(Name0, L , _) ->
          Pred = L,
         extract_tpred_name(Rest, Name, Xs)
     ;
        Pred = [],
        extract_tpred_name(Rest,Name,Xs)
    ).
extract_tpred_name([],_,[[]]).

char_dot(0'.).

closepar(0'}).

% Strip left and right blanks and newlines from a string
strip_blanks(Str0, Str) :-
    reverse(Str0, Str1),
    strip_lblanks(Str1, Str2),
    reverse(Str2, Str).

strip_lblanks([], []).
strip_lblanks([C|Cs], Ds) :- blank(C), !, strip_lblanks(Cs, Ds).
strip_lblanks(Cs, Cs).
    
blank(0' ).
blank(0'\n).
blank(0'\t).

% Strip close parenthesis from a string
strip_closepar([], []).
strip_closepar([C|Cs], Ds) :- closepar(C), !, strip_closepar(Cs, Ds).
strip_closepar([C|Cs], [C|Ds]) :- strip_closepar(Cs, Ds).


% ---------------------------------------------------------------------------
% none: no output

run_filter(none, none, none, _, off, none,  OutStr) :- !, OutStr = "".