
w3=w3fi01.o w3fi63.o w3fi83.o w3movdat.o w3fs26.o  w3reddat.o iw3jdn.o \
   gbytes_char.o

gb=baread.ansi.standard.o baopen.ansi.standard.o

l=cyclev05.o

fflag=-qrealsize=8 -qintsize=8 -O2

CMD=cdas_cycle

${CMD}:	${l} ${w3} ${gb}
	f77 -o ${CMD} ${l} ${w3} ${gb}


$l:
	f77 ${fflag} -c $<

${w3}:	
	f77 ${fflag} -c $<

${gb}:
	f77 ${fflag} -c $<

clean:
	rm -f *.o

