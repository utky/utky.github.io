.PHONY: all build clean latex run

all: build
	
clean:
	stack exec site -- clean

build: clean
	stack exec site -- build

run:
	stack exec site -- watch

latex:
	sudo docker run --rm -v `pwd`/latex:/latex thii/platex build profile.tex
	cp latex/*.pdf resources/
