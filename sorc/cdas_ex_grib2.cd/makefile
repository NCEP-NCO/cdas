# CC=ncepcc -q64
CC=icc
# CC=cc


CCFLAGS=-O2

CMD=cdas_ex_grib2

${CMD}: ex_grib2.c cnames.o cnames_file.o
	${CC} -o ${CMD} ${CCFLAGS}  ex_grib2.c cnames.o cnames_file.o

cnames_file.o: cnames_file.c
	${CC} -c ${CCFLAGS} cnames_file.c

cnames.o: cnames.c
	${CC} -c ${CCFLAGS}  cnames.c

clean:
	rm -f *.o cdas_ex_grib2
