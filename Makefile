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

LIBS = camomile

COMPILE = compile

REPS = src/contrainte,src/ecriture,src/graphe,src/lecture

IGNS = data

COMMANDS = -linkpkg,-w,Aem,-inline,20,-nodynlink

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
FILES = automate.ml \
		phoneme.ml \
		regles.ml \
		parser.mly lexer.mll \
		affichage.ml \
		format.ml \
		traduction.ml \
		outils.ml \
		phonetique.ml \
		parser_l.mly \
		lexer_l.mll \
		lecture.ml \
		contrainte.ml \
		rime.ml \
		pieds.ml \
		record.ml \
		creation.ml \
		debug.ml\
		state.ml \
		bdd.ml \
		random_bdd.ml \
		engendre.ml \
		ecriture.ml \
		main.ml

ECHO = echo

-include settings.sh # If you want to personalize your options…

all:
	${BUILD_COMMAND} ${EXEC}.native

