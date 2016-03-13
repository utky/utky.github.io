.PHONY: all build run clean

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
