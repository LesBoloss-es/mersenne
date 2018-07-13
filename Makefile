default: bench

bench:
	dune build bench/bench.exe
	./_build/default/bench/bench.exe

test:
	dune build test/test.exe
	dune exec test/test.exe

clean:
	dune clean

.PHONY: bench clean default test
