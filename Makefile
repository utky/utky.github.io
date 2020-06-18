.PHONY: all build clean run publish

all: build

clean:
	rm -rf ./public

build:
	zola build

run:
	zola serve -p 8080 -i 0.0.0.0

publish: 
	# Temporarily store uncommited changes
	git stash

	# Verify correct branch
	git checkout develop

	# Build new files
	rm -rf ./public
	zola build

	# Get previous files
	git fetch --all
	git checkout -b master --track origin/master

	# Overwrite existing files with new files
	cp -a public/. .

	# Commit
	git add -A
	git commit -m "Publish."

	# Push
	git push origin master:master

	# Restoration
	git checkout develop
	git branch -D master
	git stash pop
