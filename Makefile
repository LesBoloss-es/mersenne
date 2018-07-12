test:
	dune build test/test.exe
	dune exec test/test.exe

clean:
	dune clean

.PHONY: test clean
