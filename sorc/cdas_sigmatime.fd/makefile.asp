

CMD=cdas_sigmatime

w3=w3movdat.o w3reddat.o iw3jdn.o w3fs26.o

fflag=-qrealsize=4 -qintsize=4 -O2

${CMD}:	sigmatimey2k.f ${w3}
	f77 -o ${CMD} ${fflag} sigmatimey2k.f ${w3}

${w3}:	
	f77 ${fflag} -c $<

clean:
	/bin/rm -f *.o
