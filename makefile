WATCH=find . -name '*.hs' | entr
GULP=gulp --require=LiveScript


define when-dependency-missing 
	@echo 'CHECKING FOR DEPENDENCY $1' 
	@command -v $1 >/dev/null 2>&1 || $2
endef


define npm-check
	$(call when-dependency-missing, $1, npm install -g $2)
endef


build: build-client build-server

build-client:
	${GULP} js-src-build html scss mocha-css

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

test: test-client test-server

test-client:
	${GULP} test

test-server:
	cabal test

init:
	$(call npm-check, bower, bower)
	$(call npm-check, gulp,  gulp)
	$(call npm-check, lsc,   LiveScript)
	mkdir -p public/scripts public/styles
	npm install
	bower install
	cabal sandbox init
	cabal configure --enable-tests
	cabal install
