# LIBS =    -L /nwprod/lib -lw3nco_4
LIBS =${W3NCO_LIB4}

FC=ifort
# FC=ftn

# w3=fft99.o w3fi01.o  gbytes_char.o  
w3=fft99.o
  
sp=minv.o dirsol.o

ip=ipolates.o polates0.o polates1.o polates2.o polates3.o polates4.o \
 polates6.o gdswiz.o gdswiz00.o gdswiz01.o gdswiz03.o gdswiz04.o gdswiz05.o \
 gdswizc9.o gdswizca.o gausslat.o polfixs.o ijkgds.o

gb=getbit.o pdsens.o wryte_univ.f

l=igb1y2k.o cray.o

# FFLAGS=-qrealsize=8 -qintsize=8 -O2
# FFLAGS=-O2
FFLAGS=-O2 -convert big_endian -assume byterecl


lflag=   

CMD=cdas_igb

${CMD}:	${l} ${w3} ${sp} ${gb}
	${FC} -o ${CMD} ${lflag} ${FFLAGS} ${l} ${w3} ${sp} ${gb} ${LIBS}

${l}:
	${FC} -c ${FFLAGS} $*.f

${w3}:	
	${FC} -c ${FFLAGS} $*.f

${ip}:
	${FC} -c ${FFLAGS} $*.f

${sp}:
	${FC} -c ${FFLAGS} $*.f

${gb}:
	${FC} -c ${FFLAGS} $*.f

clean:
	rm -f *.o ${CMD}
