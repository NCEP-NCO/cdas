#LIB=-L/nwprod/lib -lw3nco_4
LIB=${W3NCO_LIB4}
FC=ifort
# FC=ftn

w3=fft99.o w3fi71.o w3fi72.o w3fi73.o w3fi74.o w3fi75.o w3fi82.o \
  w3fi01.o w3fi76.o w3fi58.o w3fi59.o gbytes_char.o w3fi68.o \
  sbyte.o sbytes.o


sp=minv.o dirsol.o

l=cray.o sgby2k.o

CMD=cdas_sgb

# FFLAGS=-qrealsize=8 -qintsize=8 -O2
#FFLAGS=-qrealsize=8 -qintlog -O2
#FFLAGS=-qrealsize=8  -O2
FFLAGS=-O2 -convert big_endian -assume byterecl


${CMD}:	${l} ${w3} ${sp}
	${FC} -o ${CMD} ${FFLAGS} $l ${w3} ${sp} ${LIB}

.f.o:
	${FC} -c ${FFLAGS} $<

clean:
	rm -f *.o ${CMD}
