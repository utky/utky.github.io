.PHONY: all build run clean latex

all: build
	
clean:
	rm ../*.html && rm -rf ../css ../images ../posts && ./site clean

site: site.hs
	stack ghc -- --make site.hs

build: site
	./site build

run: build
	./site watch

deploy: site
	./site deploy

latex:
	sudo docker run --rm -v `pwd`/latex:/latex thii/platex build profile.tex
	cp latex/*.pdf resources/
