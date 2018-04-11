.PHONY: all demo clean test

BUILD=jbuilder build --dev
CLEAN= jbuilder clean
TEST=jbuilder runtest -j1 --no-buffer --dev
INSTALL=jbuilder install

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
