FILES=$(shell ls *.tex)
DICTS=${FILES:.tex=.hdict}

CESURE=../src/cesure/cesure

all: ${DICTS}

%.hdict: %.tex
	${CESURE} $<

clean:
	- rm *.hdict
