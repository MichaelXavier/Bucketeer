all: build

build: configure Main.hs Bucketeer/*.hs
	cabal-dev build
	
configure: Bucketeer.cabal
	cabal-dev configure

clean:
	cabal-dev clean
	rm -f **/*.{o,hi} **/**/*.{o,hi}

quick_spec: Bucketeer/*.hs Bucketeer/Testing/*.hs
	runhaskell Bucketeer/Testing/Main.hs

spec: configure_tests
	cabal-dev build
	cabal-dev test

configure_tests:
	cabal-dev configure --enable-tests --user
