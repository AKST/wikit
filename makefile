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
	${GULP} js-src-build js-src-offline-build html scss 3rd-party-css icons manifest

build-server:
	cabal build wikit
	cabal build wikit-tests

watch-client:
	${GULP} watch

watch-client-test:
	${GULP} watch-test

watch-client-offline:
	${GULP} watch-offline

watch-server:
	${WATCH} make test
	
serve-client:
	ruby -run -e httpd ./public -p3000

serve-socket:
	cabal run

test: test-client test-server

test-client:
	${GULP} test

test-server:
	cabal test

debug-server:
	$(call npm-check, wscat, wscat)
	wscat -c ws://0.0.0.0:8080
	
init:
	$(call npm-check, bower, bower)
	$(call npm-check, gulp,  gulp)
	$(call npm-check, lsc,   LiveScript)
	mkdir -p public/scripts public/styles
	npm install
	bower install
	cabal sandbox init
	cabal install --enable-tests
  

