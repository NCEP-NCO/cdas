FC=ifort
# FC=ftn

# lib=-L/nwprod/lib -lw3nco_4
lib=${W3NCO_LIB4}

CMD=cdas_sigmatime

w3=w3movdat.o w3reddat.o iw3jdn.o w3fs26.o

#FFLAGS=-qrealsize=4 -qintsize=4 -O2
FFLAGS=-O2 -convert big_endian -assume byterecl

${CMD}:	sigmatimey2k.f
	ifort -o ${CMD} ${FFLAGS} sigmatimey2k.f ${lib}

.f.o:	
	${FC} ${FFLAGS} -c $<

clean:
	touch  ${CMD}
	rm  ${CMD}
