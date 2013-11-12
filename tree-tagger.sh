#!/bin/sh

# Set these paths appropriately

BIN=./tree-tagger/bin
CMD=./tree-tagger/cmd
LIB=./tree-tagger/lib

OPTIONS="-token -lemma -no-unknown"

TAGGER=${BIN}/tree-tagger
TOKENIZER=${CMD}/tokenize.pl
ABBR_LIST=${LIB}/french-abbreviations
PARFILE=${LIB}/french.par

$TOKENIZER -f -a $ABBR_LIST $* |
$TAGGER $OPTIONS $PARFILE
