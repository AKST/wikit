GULP=gulp --require=LiveScript
CBIN=~/.bins

build: build-server build-client

build-client:
	PATH=$$PATH:${CBIN} ${GULP} purescript

build-server:
	cabal build
