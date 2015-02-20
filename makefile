GULP=gulp --require=LiveScript
CBIN=./.cabal-sandbox/bin/

build: build-client

build-client:
	PATH=$$PATH:${CBIN} ${GULP} purescript
