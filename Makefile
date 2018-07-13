default: bench

bench:
	dune build --profile=profiling bench/bench.exe
	./_build/default/bench/bench.exe
	gprof _build/default/bench/bench.exe gmon.out > profiling.txt

test:
	dune build test/test.exe
	dune exec test/test.exe

clean:
	dune clean

.PHONY: bench clean default test
