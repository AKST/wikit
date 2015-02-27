GULP=PATH=$$PATH:~/.bins gulp --require=LiveScript

build: build-server build-client

build-client:
	${GULP} purescript

build-server:
	cabal build

watch-client: 
	${GULP} watch
	
serve-client:
	ruby -run -e httpd ./public -p3000

init:
	mkdir -p public/scripts public/styles
