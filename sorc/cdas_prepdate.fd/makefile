FC=ifort
# FC=ftn

#LIBS=-L/nwprod/lib -lbufr_d_64 -lw3nco_d
LIBS=${BUFR_LIBd} ${W3NCO_LIBd}

f=prepdate
target=cdas_prepdate
FFLAGS=-O2 -assume byterecl -r8

${target}:	$f.f
	${FC} $(FFLAGS) -o ${target} $f.f ${LIBS}

clean:
	touch ${target}
	rm ${target}
