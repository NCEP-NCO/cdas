# LIBS = -L/nwprod/lib -lw3emc_8 -lw3nco_8 -lbacio_8
LIBS = ${W3EMC_LIB8} ${W3NCO_LIB8} ${BACIO_LIB8}

FC=ifort
# FC=ftn

w3=w3fi01.o w3fi63.o w3fi83.o w3movdat.o w3fs26.o  w3reddat.o iw3jdn.o \
   gbytes_char.o

gb=baread.ansi.standard.o baopen.ansi.standard.o

l=cyclev05.o

# FFLAGS=-qrealsize=8 -qintsize=8 -O2
FFLAGS=-O2 -r8 -i8 -assume byterecl -convert big_endian
w3flags=-O2 -r8 -i8 -assume byterecl -convert big_endian

CMD=cdas_cycle

#${CMD}:	${l} ${w3} ${gb}
#	${FC} -o ${CMD} ${l} ${w3} ${gb} ${LIBS}
#
${CMD}:	${l}
	${FC} -o ${CMD} ${l} ${LIBS}

$l:	cyclev05.f
	${FC} ${FFLAGS} -c $*.f

${w3}:	
	${FC} ${w3flag} -c $*.f

${gb}:
	${FC} ${w3flag} -c $*.f

clean:
	rm -f *.o ${CMD}
