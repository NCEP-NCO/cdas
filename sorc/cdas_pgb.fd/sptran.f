C-----------------------------------------------------------------------
      SUBROUTINE SPTRAN(IROMB,MAXWV,IDRT,IMAX,JMAX,KMAX,
     &                  IPRIME,ISKIP,JNSKIP,JSSKIP,KWSKIP,KGSKIP,
     &                  JBEG,JEND,JCPU,
     &                  WAVE,GRIDN,GRIDS,IDIR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTRAN     PERFORM A SCALAR SPHERICAL TRANSFORM
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS A SPHERICAL TRANSFORM
C           BETWEEN SPECTRAL COEFFICIENTS OF SCALAR QUANTITIES
C           AND FIELDS ON A GLOBAL CYLINDRICAL GRID.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE GRID-SPACE CAN BE EITHER AN EQUALLY-SPACED GRID
C           (WITH OR WITHOUT POLE POINTS) OR A GAUSSIAN GRID.
C           THE WAVE AND GRID FIELDS MAY HAVE GENERAL INDEXING,
C           BUT EACH WAVE FIELD IS IN SEQUENTIAL 'IBM ORDER',
C           I.E. WITH ZONAL WAVENUMBER AS THE SLOWER INDEX.
C           TRANSFORMS ARE DONE IN LATITUDE PAIRS FOR EFFICIENCY;
C           THUS GRID ARRAYS FOR EACH HEMISPHERE MUST BE PASSED.
C           IF SO REQUESTED, JUST A SUBSET OF THE LATITUDE PAIRS
C           MAY BE TRANSFORMED IN EACH INVOCATION OF THE SUBPROGRAM.
C           THE TRANSFORMS ARE ALL MULTIPROCESSED OVER LATITUDE EXCEPT
C           THE TRANSFORM FROM FOURIER TO SPECTRAL IS MULTIPROCESSED
C           OVER ZONAL WAVENUMBER TO ENSURE REPRODUCIBILITY.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C   98-06-30  EBISUZAKI    LINUX PORT
C
C USAGE:    CALL SPTRAN(IROMB,MAXWV,IDRT,IMAX,JMAX,KMAX,
C    &                  IPRIME,ISKIP,JNSKIP,JSSKIP,KWSKIP,KGSKIP,
C    &                  JBEG,JEND,JCPU,
C    &                  WAVE,GRIDN,GRIDS,IDIR)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     IDRT     - INTEGER GRID IDENTIFIER
C                (IDRT=4 FOR GAUSSIAN GRID,
C                 IDRT=0 FOR EQUALLY-SPACED GRID INCLUDING POLES,
C                 IDRT=256 FOR EQUALLY-SPACED GRID EXCLUDING POLES)
C     IMAX     - INTEGER EVEN NUMBER OF LONGITUDES.
C     JMAX     - INTEGER NUMBER OF LATITUDES.
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     IPRIME   - INTEGER LONGITUDE INDEX FOR THE PRIME MERIDIAN.
C                (DEFAULTS TO 1 IF IPRIME=0)
C     ISKIP    - INTEGER SKIP NUMBER BETWEEN LONGITUDES
C                (DEFAULTS TO 1 IF ISKIP=0)
C     JNSKIP   - INTEGER SKIP NUMBER BETWEEN N.H. LATITUDES FROM NORTH
C                (DEFAULTS TO IMAX IF JNSKIP=0)
C     JSSKIP   - INTEGER SKIP NUMBER BETWEEN S.H. LATITUDES FROM SOUTH
C                (DEFAULTS TO -IMAX IF JSSKIP=0)
C     KWSKIP   - INTEGER SKIP NUMBER BETWEEN WAVE FIELDS
C                (DEFAULTS TO (MAXWV+1)*((IROMB+1)*MAXWV+2) IF KWSKIP=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C                (DEFAULTS TO IMAX*JMAX IF KGSKIP=0)
C     JBEG     - INTEGER LATITUDE INDEX (FROM POLE) TO BEGIN TRANSFORM
C                (DEFAULTS TO 1 IF JBEG=0)
C                (IF JBEG=0 AND IDIR<0, WAVE IS ZEROED BEFORE TRANSFORM)
C     JEND     - INTEGER LATITUDE INDEX (FROM POLE) TO END TRANSFORM
C                (DEFAULTS TO (JMAX+1)/2 IF JEND=0)
C     JCPU     - INTEGER NUMBER OF CPUS OVER WHICH TO MULTIPROCESS
C     WAVE     - REAL (*) WAVE FIELDS IF IDIR>0
C     GRIDN    - REAL (*) N.H. GRID FIELDS (STARTING AT JBEG) IF IDIR<0
C     GRIDS    - REAL (*) S.H. GRID FIELDS (STARTING AT JBEG) IF IDIR<0
C     IDIR     - INTEGER TRANSFORM FLAG
C                (IDIR>0 FOR WAVE TO GRID, IDIR<0 FOR GRID TO WAVE)
C   OUTPUT ARGUMENTS:
C     WAVE     - REAL (*) WAVE FIELDS IF IDIR<0
C     GRIDN    - REAL (*) N.H. GRID FIELDS (STARTING AT JBEG) IF IDIR>0
C     GRIDS    - REAL (*) S.H. GRID FIELDS (STARTING AT JBEG) IF IDIR>0
C
C SUBPROGRAMS CALLED:
C   SPWGET       GET WAVE-SPACE CONSTANTS
C   SPGGET       GET GRID-SPACE CONSTANTS
C   SPLEGEND     COMPUTE LEGENDRE POLYNOMIALS
C   SPSYNTH      SYNTHESIZE FOURIER FROM SPECTRAL
C   SPANALY      ANALYZE SPECTRAL FROM FOURIER
C   RFFTMLT      PERFORM FAST FOURIER TRANSFORM
C
C REMARKS: MINIMUM GRID DIMENSIONS FOR UNALIASED TRANSFORMS TO SPECTRAL:
C   DIMENSION                    LINEAR              QUADRATIC
C   -----------------------      ---------           -------------
C   IMAX                         2*MAXWV+2           3*MAXWV/2*2+2
C   JMAX (IDRT=4,IROMB=0)        1*MAXWV+1           3*MAXWV/2+1
C   JMAX (IDRT=4,IROMB=1)        2*MAXWV+1           5*MAXWV/2+1
C   JMAX (IDRT=0,IROMB=0)        2*MAXWV+3           3*MAXWV/2*2+3
C   JMAX (IDRT=0,IROMB=1)        4*MAXWV+3           5*MAXWV/2*2+3
C   JMAX (IDRT=256,IROMB=0)      2*MAXWV+1           3*MAXWV/2*2+1
C   JMAX (IDRT=256,IROMB=1)      4*MAXWV+1           5*MAXWV/2*2+1
C   -----------------------      ---------           -------------
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
      REAL WAVE(*),GRIDN(*),GRIDS(*)
      REAL EPS((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EPSTOP(MAXWV+1)
      REAL ENN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL ELONN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL EON((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EONTOP(MAXWV+1)
      REAL CLAT(JMAX),SLAT(JMAX),WLAT(JMAX),TRIG(2*IMAX)
      INTEGER IFAX(20)
      INTEGER MP(KMAX)
      REAL WFFT(2*IMAX,2*KMAX)
      REAL WTOP(2*(MAXWV+1),KMAX)
      REAL PLN((MAXWV+1)*((IROMB+1)*MAXWV+2)/2,JCPU)
      REAL PLNTOP(MAXWV+1,JCPU)
      REAL F(IMAX+3,2,KMAX,JCPU)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SET PARAMETERS
      CALL SPWGET(IROMB,MAXWV,EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP)
      CALL SPGGET(IDRT,IMAX,JMAX,CLAT,SLAT,WLAT,TRIG,IFAX)
      MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
      MXTOP=MAXWV+1
      IDIM=IMAX+3
      IP=IPRIME
      IS=ISKIP
      JN=JNSKIP
      JS=JSSKIP
      KW=KWSKIP
      KG=KGSKIP
      JB=JBEG
      JE=JEND
      JC=JCPU
      IF(IP.EQ.0) IP=1
      IF(IS.EQ.0) IS=1
      IF(JN.EQ.0) JN=IMAX
      IF(JS.EQ.0) JS=-JN
      IF(KW.EQ.0) KW=2*MX
      IF(KG.EQ.0) KG=IMAX*JMAX
      IF(JB.EQ.0) JB=1
      IF(JE.EQ.0) JE=(JMAX+1)/2
c     MP=0
      do ii = 1, KMAX
         MP(ii)=0
      enddo
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSFORM WAVE TO GRID
      IF(IDIR.GT.0) THEN
        DO K=1,KMAX
c         WTOP(1:2*MXTOP,K)=0
          do ii = 1, 2*MXTOP
             WTOP(ii,K)=0
          enddo
        ENDDO
        DO J1=JB,JE,JC
          J2=MIN(J1+JC-1,JE)
          DO J=J1,J2
            JD=J-J1+1
            CALL SPLEGEND(IROMB,MAXWV,SLAT(J),CLAT(J),EPS,EPSTOP,
     &                    PLN(1,JD),PLNTOP(1,JD))
            CALL SPSYNTH(IROMB,MAXWV,IMAX,IDIM,KW,2*MXTOP,KMAX,
     &                   CLAT(J),PLN(1,JD),PLNTOP(1,JD),MP,
     &                   WAVE,WTOP,F(1,1,1,JD))
            CALL RFFTMLT(F(1,1,1,JD),WFFT,TRIG,IFAX,1,IDIM,IMAX,
     &                   2*KMAX,1)
            DO K=1,KMAX
              DO I=1,IMAX
                IJKN=MOD(I+IP-2,IMAX)*IS+(J-JB)*JN+(K-1)*KG+1
                IJKS=MOD(I+IP-2,IMAX)*IS+(J-JB)*JS+(K-1)*KG+1
                GRIDN(IJKN)=F(I,1,K,JD)
                GRIDS(IJKS)=F(I,2,K,JD)
              ENDDO
            ENDDO
          ENDDO
        ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSFORM GRID TO WAVE
      ELSE
        IF(JBEG.EQ.0) THEN
          DO K=1,KMAX
            KWS=(K-1)*KW
c           WAVE(KWS+1:KWS+2*MX)=0
            do ii = KWS+1, KWS+2*MX
               WAVE(ii)=0
            enddo
c           WTOP(1:2*MXTOP,K)=0
            do ii = 1, 2*MXTOP
               WTOP(ii,K)=0
            enddo
          ENDDO
        ENDIF
        DO J1=JB,JE,JC
          J2=MIN(J1+JC-1,JE)
          DO J=J1,J2
            JD=J-J1+1
            IF(WLAT(J).GT.0.) THEN
              DO K=1,KMAX
                DO I=1,IMAX
                  IJKN=MOD(I+IP-2,IMAX)*IS+(J-JB)*JN+(K-1)*KG+1
                  IJKS=MOD(I+IP-2,IMAX)*IS+(J-JB)*JS+(K-1)*KG+1
                  F(I,1,K,JD)=GRIDN(IJKN)
                  F(I,2,K,JD)=GRIDS(IJKS)
                ENDDO
              ENDDO
              CALL RFFTMLT(F(1,1,1,JD),WFFT,TRIG,IFAX,1,IDIM,IMAX,
     &                     2*KMAX,-1)
              CALL SPLEGEND(IROMB,MAXWV,SLAT(J),CLAT(J),EPS,EPSTOP,
     &                      PLN(1,JD),PLNTOP(1,JD))
            ENDIF
          ENDDO
          DO J=J1,J2
            JD=J-J1+1
            IF(WLAT(J).GT.0.) THEN
              CALL SPANALY(IROMB,MAXWV,IMAX,IDIM,KW,2*MXTOP,KMAX,
     &                     WLAT(J),CLAT(J),PLN(1,JD),PLNTOP(1,JD),MP,
     &                     F(1,1,1,JD),WAVE,WTOP)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END