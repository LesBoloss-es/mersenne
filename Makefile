test:
	jbuilder build test/test.exe
	jbuilder exec test/test.exe

clean:
	jbuilder clean

.PHONY: test clean
