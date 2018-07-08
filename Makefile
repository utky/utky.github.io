.PHONY: all build clean latex run build_hakyll publish

all: build

build_hakyll:
	stack build
	
clean: build_hakyll
	stack exec site -- clean

build: clean
	stack exec site -- build

run:
	stack exec site -- watch

publish:
	# Temporarily store uncommited changes
	git stash
	
	# Verify correct branch
	git checkout develop
	
	# Build new files
	stack exec site clean
	stack exec site build
	
	# Get previous files
	git fetch --all
	git checkout -b master --track origin/master
	
	# Overwrite existing files with new files
	cp -a _site/. .
	
	# Commit
	git add -A
	git commit -m "Publish."
	
	# Push
	git push origin master:master
	
	# Restoration
	git checkout develop
	git branch -D master
	git stash pop
