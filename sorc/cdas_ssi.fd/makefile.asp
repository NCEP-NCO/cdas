SHELL=/bin/sh
#
#

w3=w3difdat.o w3reddat.o iw3jdn.o

sp=dirsol.o fft99.o

# portable
# MATINV=iminv.o
# blas=ddot.o dgemv.o xerbla.o lsame.o

# essl version
MATINV=iminv_essl.o
blas=

OBJS=  chlml.o conmc.o dtast.o dvast.o fulldivt.o \
       g2s0.o gdcrdn.o gdcrdp.o genqsat.o getbaln.o getlalo.o getpln.o \
       glbsoi2.o grad2s.o gtbdivt.o gtbhalf.o hoper.o hopers.o htoper.o \
       inguess.o inguessv.o \
       initps2.o initqpw2.o initsat6.o \
       initt3.o initw2.o intps2.o intqpw2.o \
       intrp2.o intrp3.o intt3.o intw2.o \
       m1glat.o m1ipqr.o m1poly.o m1rcons2.o \
       pcgsoi5.o \
       bufr.o \
       prepp.o preppw.o prepq.o preps.o prept.o prepw.o prnon85.o \
       qoper.o qtoper.o \
       rdfact.o rdgesc.o rdprep7.o rdtest9.o \
       residw.o respsf.o respw.o resq.o ressat.o restmp.o \
       s2g0.o s2grad.o s2gvec.o s2mg2x.o satc2.o satcov.o satop46.o \
       setuprhs5y2k.o sprp2.o sprqpw2.o sprs6.o sprt2.o spruv2.o \
       ssi2.o tg2s0.o tgrad2s.o ts2g0.o ts2grad.o ts2gvec.o wranlcy2k.o \
	w3tagb.o cray.o ${MATINV} \
       w3locdat.o w3doxdat.o summary.o w3fs26.o

CMD = cdas_ssi

#FF=xlf_r
FF=xlf

#FFLAGS = -c -qrealsize=8 -qintsize=4 -O2
#FFLAGS = -c -qrealsize=8 -qintsize=4 -O2 -q64
#FFLAGS = -c -qrealsize=8 -qintsize=4 -O2 -q64 -qsmp
#FFLAGS = -c -qrealsize=8 -qintsize=4 -O2 -q64
#FFLAGS = -c -qrealsize=8 -qintsize=4 -O2 -g -q64 -qcheck -qextchk
FFLAGS = -c -qrealsize=8 -qintsize=4 -O2 -q64 -g

CFLAGS = -c -qrealsize=8 -qintsize=4 -O2 -q64 -g

#LDFLAGS = -qrealsize=8 -qintsize=4 -O2 -q64 -lessl
#LDFLAGS = -qrealsize=8 -qintsize=4 -O2 -g -q64 
#LDFLAGS = -qrealsize=8 -qintsize=4 -O2 -q64 -lessl
LDFLAGS = -qrealsize=8 -qintsize=4 -O2 -q64 -lessl -bmaxdata:1000000000 -bmaxstack:1000000000


$(CMD):		$(OBJS) ${w3} ${sp} ${blas}
	$(FF) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS) ${w3} ${sp} ${blas}


clean:
	-rm -f *.o

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile

${obj}:
	${FF} ${FFLAGS} $*.f

${w3}:
	${FF} ${FFLAGS} $<

${sp}:
	${FF} ${FFLAGS} $<

summary.o:	summary.c
	xlc ${CFLAGS} summary.c
