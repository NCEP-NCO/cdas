C-----------------------------------------------------------------------
      SUBROUTINE SPTGPMV(IROMB,MAXWV,KMAX,MI,MJ,
     &                   KWSKIP,KGSKIP,NISKIP,NJSKIP,
     &                   RLAT1,RLON1,DLAT,DLON,WAVED,WAVEZ,UM,VM)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTGPMV    TRANSFORM SPECTRAL VECTOR TO MERCATOR
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS A SPHERICAL TRANSFORM
C           FROM SPECTRAL COEFFICIENTS OF DIVERGENCES AND CURLS
C           TO VECTOR FIELDS ON A MERCATOR GRID.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE WAVE AND GRID FIELDS MAY HAVE GENERAL INDEXING,
C           BUT EACH WAVE FIELD IS IN SEQUENTIAL 'IBM ORDER',
C           I.E. WITH ZONAL WAVENUMBER AS THE SLOWER INDEX.
C           THE MERCATOR GRID IS IDENTIFIED BY THE LOCATION
C           OF ITS FIRST POINT AND BY ITS RESPECTIVE INCREMENTS.
C           THE TRANSFORMS ARE ALL MULTIPROCESSED OVER SECTOR POINTS.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C   98-06-30  EBISUZAKI    LINUX PORT
C
C USAGE:    CALL SPTGPMV(IROMB,MAXWV,KMAX,MI,MJ,
C    &                   KWSKIP,KGSKIP,NISKIP,NJSKIP,
C    &                   RLAT1,RLON1,DLAT,DLON,WAVED,WAVEZ,UM,VM)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     MI       - INTEGER NUMBER OF POINTS IN THE FASTER ZONAL DIRECTION
C     MJ       - INTEGER NUMBER OF POINTS IN THE SLOWER MERID DIRECTION
C     KWSKIP   - INTEGER SKIP NUMBER BETWEEN WAVE FIELDS
C                (DEFAULTS TO (MAXWV+1)*((IROMB+1)*MAXWV+2) IF KWSKIP=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C                (DEFAULTS TO MI*MJ IF KGSKIP=0)
C     NISKIP   - INTEGER SKIP NUMBER BETWEEN GRID I-POINTS
C                (DEFAULTS TO 1 IF NISKIP=0)
C     NJSKIP   - INTEGER SKIP NUMBER BETWEEN GRID J-POINTS
C                (DEFAULTS TO MI IF NJSKIP=0)
C     RLAT1    - REAL LATITUDE OF THE FIRST GRID POINT IN DEGREES
C     RLON1    - REAL LONGITUDE OF THE FIRST GRID POINT IN DEGREES
C     DLAT     - REAL LATITUDE INCREMENT IN DEGREES SUCH THAT
C                D(PHI)/D(J)=DLAT*COS(PHI) WHERE J IS MERIDIONAL INDEX.
C                DLAT IS NEGATIVE FOR GRIDS INDEXED SOUTHWARD.
C                (IN TERMS OF GRID INCREMENT DY VALID AT LATITUDE RLATI,
C                 THE LATITUDE INCREMENT DLAT IS DETERMINED AS
C                 DLAT=DPR*DY/(RERTH*COS(RLATI/DPR))
C                 WHERE DPR=180/PI AND RERTH IS EARTH'S RADIUS)
C     DLON     - REAL LONGITUDE INCREMENT IN DEGREES SUCH THAT
C                D(LAMBDA)/D(I)=DLON WHERE I IS ZONAL INDEX.
C                DLON IS NEGATIVE FOR GRIDS INDEXED WESTWARD.
C     WAVED    - REAL (*) WAVE DIVERGENCE FIELDS
C     WAVEZ    - REAL (*) WAVE VORTICITY FIELDS
C   OUTPUT ARGUMENTS:
C     UM       - REAL (*) MERCATOR U-WINDS
C     VM       - REAL (*) MERCATOR V-WINDS
C
C SUBPROGRAMS CALLED:
C   SPWGET       GET WAVE-SPACE CONSTANTS
C   SPLEGEND     COMPUTE LEGENDRE POLYNOMIALS
C   SPSYNTH      SYNTHESIZE FOURIER FROM SPECTRAL
C   SPDZ2UV      COMPUTE WINDS FROM DIVERGENCE AND VORTICITY
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
      REAL WAVED(*),WAVEZ(*),UM(*),VM(*)
      REAL EPS((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EPSTOP(MAXWV+1)
      REAL ENN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL ELONN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL EON((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EONTOP(MAXWV+1)
      INTEGER MP(2*KMAX)
      REAL W((MAXWV+1)*((IROMB+1)*MAXWV+2)/2*2+1,2*KMAX)
      REAL WTOP(2*(MAXWV+1),2*KMAX)
      REAL PLN((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),PLNTOP(MAXWV+1)
      REAL F(2*MAXWV+3,2,2*KMAX)
      REAL CLAT(MJ),SLAT(MJ),CLON(MAXWV,MI),SLON(MAXWV,MI)
      PARAMETER(RERTH=6.3712E6)
      PARAMETER(PI=3.14159265358979,DPR=180./PI)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CALCULATE PRELIMINARY CONSTANTS
      CALL SPWGET(IROMB,MAXWV,EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP)
      MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
      MXTOP=MAXWV+1
      MDIM=2*MX+1
      IDIM=2*MAXWV+3
      KW=KWSKIP
      KG=KGSKIP
      NI=NISKIP
      NJ=NJSKIP
      IF(KW.EQ.0) KW=2*MX
      IF(KG.EQ.0) KG=MI*MJ
      IF(NI.EQ.0) NI=1
      IF(NJ.EQ.0) NJ=MI
      DO I=1,MI
        RLON=MOD(RLON1+DLON*(I-1)+3600,360.)
        DO L=1,MAXWV
          CLON(L,I)=COS(L*RLON/DPR)
          SLON(L,I)=SIN(L*RLON/DPR)
        ENDDO
      ENDDO
      YE=1-LOG(TAN((RLAT1+90)/2/DPR))*DPR/DLAT
      DO J=1,MJ
        RLAT=ATAN(EXP(DLAT/DPR*(J-YE)))*2*DPR-90
        CLAT(J)=COS(RLAT/DPR)
        SLAT(J)=SIN(RLAT/DPR)
      ENDDO
c     MP=1
      do ii = 1, 2*KMAX
      MP(ii)=1
      enddo
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CALCULATE SPECTRAL WINDS
      DO K=1,KMAX
        KWS=(K-1)*KW
        CALL SPDZ2UV(IROMB,MAXWV,ENN1,ELONN1,EON,EONTOP,
     &               WAVED(KWS+1),WAVEZ(KWS+1),
     &               W(1,K),W(1,KMAX+K),WTOP(1,K),WTOP(1,KMAX+K))
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TRANSFORM TO GRID
      DO J=1,MJ
        CALL SPLEGEND(IROMB,MAXWV,SLAT(J),CLAT(J),EPS,EPSTOP,
     &                PLN,PLNTOP)
        CALL SPSYNTH(IROMB,MAXWV,2*MAXWV,IDIM,MDIM,2*MXTOP,2*KMAX,
     &               CLAT(J),PLN,PLNTOP,MP,W,WTOP,F)
        DO K=1,KMAX
          KU=K
          KV=K+KMAX
          DO I=1,MI
            IJK=(I-1)*NI+(J-1)*NJ+(K-1)*KG+1
            UM(IJK)=F(1,1,KU)
            VM(IJK)=F(1,1,KV)
          ENDDO
          DO L=1,MAXWV
            DO I=1,MI
              IJK=(I-1)*NI+(J-1)*NJ+(K-1)*KG+1
              UM(IJK)=UM(IJK)+2.*(F(2*L+1,1,KU)*CLON(L,I)
     &                           -F(2*L+2,1,KU)*SLON(L,I))
              VM(IJK)=VM(IJK)+2.*(F(2*L+1,1,KV)*CLON(L,I)
     &                           -F(2*L+2,1,KV)*SLON(L,I))
            ENDDO
          ENDDO
        ENDDO
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
