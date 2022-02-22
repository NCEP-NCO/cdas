#
# AIX  FC=ncepxlf
FC=ifort
# FC=ftn

# LIBS =    -L/nwprod/lib -lw3nco_d -lw3emc_d
LIBS =${W3NCO_LIBd} ${W3EMC_LIBd}

mdl=cdas_mrf

obj=fft99.o cray.o w3fi68.o w3fi71.o w3fi72.o w3fi73.o w3fi74.o w3fi75.o \
 w3fi82.o w3fi01.o w3fi76.o w3fi58.o w3fi59.o gbytes.o gbytes_char.o \
 ibm.o f6228d_snowdiag_12.o \
 w3fs26.f iw3jdn.f w3tagb.o w3locdat.o w3doxdat.o
# AIX summary.o

# note: comment out w3tag and you do not need the following
#  w3fs26.f iw3jdn.f w3tagb.o w3locdat.o w3doxdat.o summary.o

# AIX FFLAGS=-qrealsize=8 -qintsize=4 -O2 -q64
# FFLAGS=-r8 -O2  -convert big_endian -assume byterecl -xHost
FFLAGS=-r8 -O2  -convert big_endian -assume byterecl

lflags=   
${mdl}:	${obj}
	${FC} -o ${mdl} ${lflags} ${FFLAGS} ${obj} ${LIBS}

.f.o:	$@
	${FC} -c ${FFLAGS} $*.f

clean:
	touch junk.o ${mdl}
	rm *.o ${mdl}
