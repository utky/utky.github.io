.PHONY: all build run clean latex

all: build
	
clean:
	rm ../*.html && rm -rf ../css ../images ../posts && ./site clean

site: site.hs
	stack build

build: site
	stack exec site build

run: build
	stack exec site watch

deploy: site
	stack exec site deploy

latex:
	sudo docker run --rm -v `pwd`/latex:/latex thii/platex build profile.tex
	cp latex/*.pdf resources/
