.PHONY: all build run clean

all: build
	
clean:
	./site clean

site: site.hs
	ghc --make -threaded site.hs

build: site
	./site build

run: site
	./site watch

deploy: site
	./site deploy
