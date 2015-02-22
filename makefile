GULP=gulp --require=LiveScript
CBIN=~/.bins

build: build-client

build-client:
	PATH=$$PATH:${CBIN} ${GULP} purescript
