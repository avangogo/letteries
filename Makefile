#   Lettreries is a random poem generator.
#   Copyright (C) 2012 Rémi de Verclos, Martin Bodin
#
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.

LIBS = unix,bigarray,camomile

COMPILE = compile

REPS = src/contrainte,src/ecriture,src/graphe,src/lecture,src/phonetique,src/grammaire,src/finiteautomaton,src/misc,src/main,src/structures

IGNS = data

COMMANDS = -w,Ae

BUILD_FLAGS = \
	${COMMANDS},-I,${shell ocamlfind query camomile}

BUILD_COMMAND = \
	ocamlbuild -j 0 \
	-build-dir ${COMPILE} \
	-Is ${REPS} \
	-Xs ${IGNS} \
	-libs ${LIBS} \
	-cflags ${BUILD_FLAGS} \
	-lflags ${BUILD_FLAGS} \

EXEC = lettreries
MAIN = main
PATHMAIN = src/main

PHON_EXEC = phonetique
PHON_MAIN = phonetique_interractiveloop
PHON_PATH = src/misc

-include settings.sh # If you want to personalize your options…

all: lettreries phonetique

clean:
	rm -R ${COMPILE}/*

%.byte: src/*
	${BUILD_COMMAND} $*.byte

%.native: src/*
	${BUILD_COMMAND} $*.native

byte: ${MAIN}.byte
	cp ${COMPILE}/${PATHMAIN}/${MAIN}.byte ${EXEC}

lettreries: ${MAIN}.native
	cp ${COMPILE}/${PATHMAIN}/${MAIN}.native ${EXEC}

phonetique: ${PHON_MAIN}.native
	cp ${COMPILE}/${PHON_PATH}/${PHON_MAIN}.native ${PHON_EXEC}
