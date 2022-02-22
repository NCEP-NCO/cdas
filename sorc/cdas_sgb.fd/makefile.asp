
w3=fft99.o w3fi71.o w3fi72.o w3fi73.o w3fi74.o w3fi75.o w3fi82.o \
  w3fi01.o w3fi76.o w3fi58.o w3fi59.o gbytes_char.o w3fi68.o \
  sbyte.o sbytes.o


sp=minv.o dirsol.o

l=cray.o sgby2k.o

CMD=cdas_sgb

fflag=-qrealsize=8 -qintsize=8 -O2
fflag=-qrealsize=8 -qintlog -O2
fflag=-qrealsize=8  -O2
fflag=-O2

${CMD}:	${l} ${w3} ${sp}
	f77 -o ${CMD} ${fflag} $l ${w3} ${sp}

$l:
	f77 -c ${fflag} $<

${w3}:	
	f77 -c ${fflag} $<


${sp}:
	f77 -c ${fflag} $<

clean:
	rm -f *.o
