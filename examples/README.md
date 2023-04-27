# Build instructions

1 - Collect examples in `code/`
2 - Steps to (re)generate automatically the items at `results/` and include them in the documentation:

 - Clean to ensure regeneration
```
lpdoc --clean -t html SETTINGS.pl
lpdoc -t html SETTINGS.pl
```
 - Process items at `results/`
```
ciao-exfilter
```
 - Clean again to force regeneration
```
lpdoc --clean -t html SETTINGS.pl
lpdoc -t html SETTINGS.pl
```