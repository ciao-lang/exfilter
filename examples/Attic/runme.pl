:- module(_, [main/0], []).

:- use_module(engine(io_basic)).
:- use_module(library(classic/classic_predicates)).
:- use_module(library(stream_utils)).
:- use_module(engine(stream_basic)).
:- use_module(library(system), [touch/1]).
:- use_module(library(process), [process_call/3]).
:- use_module(library(system_extra)).
:- use_module(library(source_tree)).

main :-
    clean_up,
    In = 'tutorial.pl',
    open(In, read, Stream),
    read_lines(Stream, Lines),
    close(Stream),
    scan_result_files(Lines),
    run_exfilter,
    write_string("Run 'lpdoc -t html tutorial.pl' to generate the documentation."), nl.
    
scan_result_files([]). 
scan_result_files([Li|Lines]) :-
    ( extract_result_files(Li, File) ->
        make_dir_nofail('results'), atom_codes(Atom,File), touch(Atom), 
        scan_result_files(Lines)
    ; scan_result_files(Lines)
    ).

extract_result_files(Li, File):-
    append("@includecode{results",Rest,Li),
    append(Rest0,"}",Rest),
    append("results",Rest0,File),
    write_string("Expected result: "),
    write_string(File), nl.

make_dir_nofail(D) :-
    ( file_exists(D) -> true
    ; make_directory(D)
    ).

read_lines(Stream, Xs) :-
    get_line(Stream, L),
    !,
    ( L = end_of_file ->
        Xs = []
    ; Xs = [L|Xs0],
      read_lines(Stream, Xs0)
    ).
read_lines(_, []).

clean_up:-
    Dir = 'results',
    make_dir_nofail(Dir),
    delete_glob(Dir,'*.txt|*.raw.pl|*.raw.out').
    
run_exfilter:-
    Dir = 'results',
    process_call(path(find), [Dir, '-name', '\*.txt'], [stdout(atmlist(Files))]),
    run(Files).

run([]).
run([File|Files]):-
    write_string("Processing: "), write(File), nl,
    process_call(path('ciao-exfilter'),[File],[stdout(file(File))]),
    run(Files).
