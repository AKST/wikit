WATCH=find . -name '*.hs' | entr
GULP=PATH=$$PATH:~/.bins gulp --require=LiveScript

build: build-server build-client

build-client:
	${GULP} js

build-server:
	cabal build

watch-client: 
	${GULP} watch

watch-server:
	${WATCH} make build-server serve-socket
	
serve-client:
	ruby -run -e httpd ./public -p3000

serve-socket:
	cabal run

init:
	mkdir -p public/scripts public/styles
