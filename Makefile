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

LIBS = unix

COMPILE = compile

REPS = src/contrainte,src/ecriture,src/graphe,src/lecture,src/phonetique,src/grammaire,src/finiteautomaton,src/misc

IGNS = data

COMMANDS = -w,Aem

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
PATHMAIN = src/ecriture

PHON_EXEC = phonetique
PHON_MAIN = phonetique_interractiveloop
PHON_PATH = src/misc

-include settings.sh # If you want to personalize your options…

all: lettreries

clean:
	rm -R ${COMPILE}/*

%.native: src/*
	${BUILD_COMMAND} $*.native

lettreries: ${MAIN}.native
	cp ${COMPILE}/${PATHMAIN}/${MAIN}.native ${EXEC}

phonetique: ${PHON_MAIN}.native
	cp ${COMPILE}/${PHON_PATH}/${PHON_MAIN}.native ${PHON_EXEC}