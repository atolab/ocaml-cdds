.PHONY: all demo clean test

BUILD=dune build
CLEAN= dune clean
TEST=dune runtest -j1 --no-buffer
INSTALL=dune install

all:
		${BUILD}

demo:
	make -C demo

test:
		${TEST}

install:
	${INSTALL}

clean:
	${CLEAN}
	make -C demo clean
