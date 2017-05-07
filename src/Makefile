.PHONY: all build run clean latex

all: build
	
clean:
	rm ../*.html && rm -rf ../css ../images ../posts && ./site clean

site: site.hs
	stack build
	cp -p .stack-work/dist/x86_64-linux-nix/Cabal-1.22.5.0/build/site/site .

build: site
	./site build

run: build
	./site watch -p 8080

deploy: site
	./site deploy

latex:
	sudo docker run --rm -v `pwd`/latex:/latex thii/platex build profile.tex
	cp latex/*.pdf resources/
