.PHONY: all clean

TARGET=simple.exe
BUILD_DIR=_build/default
BUILD=jbuilder build --dev ${TARGET}
CLEAN= jbuilder clean

all:
		${BUILD}
		cp ${BUILD_DIR}/${TARGET} .
clean:
	${CLEAN}
	$(RM) -f ${TARGET}
