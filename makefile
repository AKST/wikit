WATCH=find . -name '*.hs' | entr
GULP=gulp --require=LiveScript


define when-dependency-missing 
	@echo 'CHECKING FOR DEPENDENCY $1' 
	@command -v $1 >/dev/null 2>&1 || $2
endef


define npm-check
	$(call when-dependency-missing, $1, npm install -g $2)
endef


build: build-server build-client

build-client:
	${GULP} js

build-server:
	cabal build

watch-client: 
	${GULP} watch

watch-server:
	${WATCH} make build-server
	
serve-client:
	ruby -run -e httpd ./public -p3000

serve-socket:
	cabal run

test-client:
	${GULP} test

init:
	$(call npm-check, phantomjs, phantomjs)
	$(call npm-check, bower,     bower)
	$(call npm-check, gulp,      gulp)
	$(call npm-check, lsc,       LiveScript)
	mkdir -p public/scripts public/styles
