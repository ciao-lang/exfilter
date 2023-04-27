#!/bin/sh

# Clean to ensure regeneration
lpdoc --clean -t html SETTINGS.pl
lpdoc -t html SETTINGS.pl
# Process items at results/
ciao-exfilter
# Clean again to force regeneration
lpdoc --clean -t html SETTINGS.pl
lpdoc -t html SETTINGS.pl
