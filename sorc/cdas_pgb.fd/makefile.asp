
w3=fft99.o w3fi68.o w3fi71.o w3fi72.o w3fi73.o w3fi74.o w3fi75.o \
 w3fi82.o w3fi01.o w3fi76.o w3fi58.o w3fi59.o gbytes.o gbytes_char.o 

sp=minv.o sptrun.o sptrunm.o sptrung.o sptran.o sptruns.o spgget.o \
  ncpus.o splat.o sptgpsv.o spanaly.o dirsol.o sptgptv.o sptgpmv.o \
  spwget.o spsynth.o sptgps.o speps.o splegend.o sptgpm.o \
  sptgpt.o spdz2uv.o

ip=ipolates.o polates0.o polates1.o polates2.o polates3.o polates4.o \
 polates6.o gdswiz.o gdswiz00.o gdswiz01.o gdswiz03.o gdswiz04.o gdswiz05.o \
 gdswizc9.o gdswizca.o gausslat.o polfixs.o ijkgds.o

gb=getbit.o pdsens.o wryte_univ.o  baopen_univ.o

l=cray.o idsdef.o gtbits.o pgb.o gribit.o

fflag=-qrealsize=8 -qintsize=8 -O2
fflag=-qrealsize=4 -qintsize=4 -O3
lflag=-bmaxdata:1000000000 -bmaxstack:1000000000

CMD=cdas_pgb
${CMD}:	${w3} ${sp} ${gb} ${l}
	f77 -o ${CMD} ${lflag} ${fflag} ${w3} ${sp} ${gb} ${l}

${l}:
	f77 -c ${fflag} $<

${w3}:	
	f77 -c ${fflag} $<

${ip}:
	f77 -c ${fflag} $<

${sp}:
	f77 -c ${fflag} $<

${gb}:
	f77 -c ${fflag} $<

clean:
	rm -f *.o

