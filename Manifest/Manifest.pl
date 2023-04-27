:- bundle(exfilter).
version('1.0').
depends([ciaopp]).
alias_paths([
    exfilter = 'src'
]).
lib(src).
cmd('ciao-exfilter', [main='src/exfilter']).

