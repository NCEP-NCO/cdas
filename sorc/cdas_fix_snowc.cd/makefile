#LIB=-L/nwprod/lib/ -lgribw
#INCLUDE=-I/nwprod/lib/sorc/gribw/include
#LIB=${GRIBW_LIB} -I

GRIBW_SRC=../gribw_lib.cd
LIB=-L${GRIBW_SRC} -lgribw
INCLUDE=-I${GRIBW_SRC}

CC=icc

CMD=cdas_fix_snowc

${CMD}:	fix_snowc.c
	${CC} -o ${CMD} -O2 ${INCLUDE} fix_snowc.c ${LIB} -lm

clean:
	rm -f *.o ${CMD}
