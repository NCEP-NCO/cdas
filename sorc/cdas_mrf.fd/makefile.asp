#
mdl=cdas_mrf

obj=fft99.o cray.o w3fi68.o w3fi71.o w3fi72.o w3fi73.o w3fi74.o w3fi75.o \
 w3fi82.o w3fi01.o w3fi76.o w3fi58.o w3fi59.o gbytes.o gbytes_char.o \
 ibm.o f6228d_snowdiag_12.o \
 w3fs26.f iw3jdn.f w3tagb.o w3locdat.o w3doxdat.o summary.o

# note: comment out w3tag and you do not need the following
#  w3fs26.f iw3jdn.f w3tagb.o w3locdat.o w3doxdat.o summary.o

f77=xlf
fflags=-qrealsize=8 -qintsize=8 -O2
fflags=-qrealsize=8 -qintsize=4 -O2 
fflags=-qrealsize=8 -qintsize=4 -O2 -qextchk
fflags=-qrealsize=8 -qintsize=4 -O2 -qextchk -C -g
fflags=-qrealsize=8 -qintsize=4 -O2 -qextchk -g -qsource -qattr
fflags=-qrealsize=8 -qintsize=4 -O2  -qsource -qattr
fflags=-qrealsize=8 -qintsize=4 -O2 

lflags=-bmaxdata:1000000000 -bmaxstack:1000000000
${mdl}:	${obj}
	${f77} -o ${mdl} ${lflags} ${fflags} ${obj}

.f.o:	$@
	${f77} -c ${fflags} $*.f

.c.o:	$@
	xlc  -c $*.c
clean:
	rm *.o {mdl}.x
