:- module(_, [], [doccfg]).

% Site definition (as an LPdoc doc)

filepath := '.'.

doc_structure := tutorial .
      
% No indices
index := concept|lib|pred|prop|regtype|decl|author|global.

doc_mainopts := no_patches|no_biblio|no_math.
doc_compopts := no_isoline|no_engmods|propmods|no_changelog|no_authors|no_biblio|no_math.

% TODO: port this manual
allow_markdown := yes.
syntax_highlight := yes.
allow_runnable := yes.


% ===========================================================================

% (extensions)
load_doc_module := exfilter(exfilter_lpdoc).