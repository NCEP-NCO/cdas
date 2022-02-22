CC=icc
# CC=cc

CMD=cdas_ex_level

${CMD}: ex_level.c get_level.c
	${CC} -o ${CMD} -O2 ex_level.c get_level.c

clean:
	touch ${CMD}
	rm ${CMD}
