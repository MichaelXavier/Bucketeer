CABAL=cabal-dev
OUTPUT_BIN=dist/build/bucketeer/bucketeer
CONFIG_OPTS=
MAX_BENCH_CONCURRENCY=15

.PHONY: benchmark

all: build
	strip $(OUTPUT_BIN)
	rm -rf dist/build/bucketeer/bucketeer-tmp
	tar czfv bucketeer.keter $(OUTPUT_BIN) config/keter.yaml

build: configure Bucketeer/*.hs
	$(CABAL) build
	
configure: Bucketeer.cabal install_dependencies
	$(CABAL) configure $(CONFIG_OPTS)

strip: $(OUTPUT_BIN)
	strip $(OUTPUT_BIN)

clean_tmp:
	rm -rf dist/build/bucketeer/bucketeer-tmp

keter: $(OUTPUT_BIN) config/keter.yaml
	tar czfv bucketeer.keter $(OUTPUT_BIN) config/keter.yaml

install_dependencies:
	$(CABAL) install --only-dependencies --enable-tests

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

benchmark:
	cd benchmark && ./run_benchmark.sh $(MAX_BENCH_CONCURRENCY)
