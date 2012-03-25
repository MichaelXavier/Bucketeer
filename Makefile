CABAL=cabal

all: build

build: configure Main.hs Bucketeer/*.hs
	$(CABAL) build
	
configure: Bucketeer.cabal
	$(CABAL) configure

clean:
	$(CABAL) clean
	rm -f **/*.{o,hi} **/**/*.{o,hi}

quick_spec: Bucketeer/*.hs Bucketeer/Testing/*.hs
	runhaskell Bucketeer/Testing/Main.hs

spec: configure_tests
	$(CABAL) build
	$(CABAL) test

configure_tests:
	$(CABAL) configure --enable-tests --user
