      SUBROUTINE GRIBIT(F,LBM,IDRT,IM,JM,MXBIT,COLAT1,
     1                  ILPDS,IPTV,ICEN,IGEN,IBMS,IPU,ITL,IL1,IL2,
     2                  IYR,IMO,IDY,IHR,IFTU,IP1,IP2,ITR,
     3                  INA,INM,ICEN2,IDS,IENS,
     4                  XLAT1,XLON1,DELX,DELY,ORIENT,PROJ,
     5                  GRIB,LGRIB,IERR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    GRIBIT      CREATE GRIB MESSAGE
C   PRGMMR: IREDELL          ORG: W/NMC23    DATE: 92-10-31
C
C ABSTRACT: CREATE A GRIB MESSAGE FROM A FULL FIELD.
C   AT PRESENT, ONLY GLOBAL LATLON GRIDS AND GAUSSIAN GRIDS
C   AND REGIONAL POLAR PROJECTIONS ARE ALLOWED.
C
C PROGRAM HISTORY LOG:
C   92-10-31  IREDELL
C   94-05-04  JUANG (FOR GSM AND RSM USE)
C
C USAGE:    CALL GRIBIT(F,LBM,IDRT,IM,JM,MXBIT,COLAT1,
C    &                  ILPDS,IPTV,ICEN,IGEN,IBMS,IPU,ITL,IL1,IL2,
C    &                  IYR,IMO,IDY,IHR,IFTU,IP1,IP2,ITR,
C    &                  INA,INM,ICEN2,IDS,IENS,
C    &                  XLAT1,XLON1,DELX,DELY,ORIENT,PROJ,
C    &                  GRIB,LGRIB,IERR)
C   INPUT ARGUMENT LIST:
C     F        - REAL (IM*JM) FIELD DATA TO PACK INTO GRIB MESSAGE
C     LBM      - LOGICAL (IM*JM) BITMAP TO USE IF IBMS=1
C     IDRT     - INTEGER DATA REPRESENTATION TYPE
C                (0 FOR LATLON OR 4 FOR GAUSSIAN OR 5 FOR POLAR)
C     IM       - INTEGER LONGITUDINAL DIMENSION
C     JM       - INTEGER LATITUDINAL DIMENSION
C     MXBIT    - INTEGER MAXIMUM NUMBER OF BITS TO USE (0 FOR NO LIMIT)
C     COLAT1   - REAL FIRST COLATITUDE OF GRID IF IDRT=4 (RADIANS)
C     ILPDS    - INTEGER LENGTH OF THE PDS (USUALLY 28)
C     IPTV     - INTEGER PARAMETER TABLE VERSION (USUALLY 1)
C     ICEN     - INTEGER FORECAST CENTER (USUALLY 7)
C     IGEN     - INTEGER MODEL GENERATING CODE
C     IBMS     - INTEGER BITMAP FLAG (0 FOR NO BITMAP)
C     IPU      - INTEGER PARAMETER AND UNIT INDICATOR
C     ITL      - INTEGER TYPE OF LEVEL INDICATOR
C     IL1      - INTEGER FIRST LEVEL VALUE (0 FOR SINGLE LEVEL)
C     IL2      - INTEGER SECOND LEVEL VALUE
C     IYR      - INTEGER YEAR
C     IMO      - INTEGER MONTH
C     IDY      - INTEGER DAY
C     IHR      - INTEGER HOUR
C     IFTU     - INTEGER FORECAST TIME UNIT (1 FOR HOUR)
C     IP1      - INTEGER FIRST TIME PERIOD
C     IP2      - INTEGER SECOND TIME PERIOD (0 FOR SINGLE PERIOD)
C     ITR      - INTEGER TIME RANGE INDICATOR (10 FOR SINGLE PERIOD)
C     INA      - INTEGER NUMBER INCLUDED IN AVERAGE
C     INM      - INTEGER NUMBER MISSING FROM AVERAGE
C     ICEN2    - INTEGER FORECAST SUBCENTER
C                (USUALLY 0 BUT 1 FOR REANAL OR 2 FOR ENSEMBLE)
C     IDS      - INTEGER DECIMAL SCALING
C     IENS     - INTEGER (5) ENSEMBLE EXTENDED PDS VALUES
C                (APPLICATION,TYPE,IDENTIFICATION,PRODUCT,SMOOTHING)
C                (USED ONLY IF ICEN2=2 AND ILPDS>=45)
C     XLAT1    - REAL FIRST POINT OF REGIONAL LATITUDE (RADIANS)
C     XLON1    - REAL FIRST POINT OF REGIONAL LONGITUDE (RADIANS)
C     DELX     - REAL DX ON 60N FOR REGIONAL (M)
C     DELY     - REAL DY ON 60N FOR REGIONAL (M)
C     PROJ     - REAL POLAR PROJECTION FLAG 0 FOR NORTH 1 FOR SOUTH
C     ORIENT   - REAL ORIENTATION OF REGIONAL DOMAIN
C
C   OUTPUT ARGUMENT LIST:
C     GRIB     - CHARACTER (LGRIB) GRIB MESSAGE
C     LGRIB    - INTEGER LENGTH OF GRIB MESSAGE
C                (NO MORE THAN 100+ILPDS+IM*JM*(MXBIT+1)/8)
C     IERR     - INTEGER ERROR CODE (0 FOR SUCCESS)
C
C SUBPROGRAMS CALLED:
C   GTBITS     - COMPUTE NUMBER OF BITS AND ROUND DATA APPROPRIATELY
C   W3FI72     - ENGRIB DATA INTO A GRIB1 MESSAGE
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
      INTEGER IENS(5)
      REAL F(IM*JM)
      LOGICAL LBM(IM*JM)
      CHARACTER GRIB(*)*1
      INTEGER IBM(IM*JM*IBMS+1-IBMS),IPDS(100),IGDS(100),IBDS(100)
      REAL FR(IM*JM)
      CHARACTER PDS(ILPDS)*1
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  DETERMINE GRID PARAMETERS
      PI=ACOS(-1.)
      NF=IM*JM
      IF(IDRT.EQ.0) THEN
        IF(IM.EQ.144.AND.JM.EQ.73) THEN
          IGRID=2
        ELSEIF(IM.EQ.360.AND.JM.EQ.181) THEN
          IGRID=3
        ELSE
          IGRID=255
        ENDIF
        IRESFL=128
        ISCAN=0
        LAT1=NINT(90.E3)
        LON1=0
        LATI=NINT(180.E3/(JM-1))
        LONI=NINT(360.E3/IM)
        IGDS09=-LAT1
        IGDS10=-LONI
        IGDS11=LATI
        IGDS12=LONI
      ELSEIF(IDRT.EQ.4) THEN
        IF(IM.EQ.192.AND.JM.EQ.94) THEN
          IGRID=98
        ELSEIF(IM.EQ.384.AND.JM.EQ.190) THEN
          IGRID=126
        ELSE
          IGRID=255
        ENDIF
        IRESFL=128
        ISCAN=0
        LAT1=NINT(90.E3-180.E3/PI*COLAT1)
        LON1=0
        LATI=JM/2
        LONI=NINT(360.E3/IM)
        IGDS09=-LAT1
        IGDS10=-LONI
        IGDS11=LATI
        IGDS12=LONI
      ELSEIF(IDRT.EQ.5) THEN    ! POLAR PROJECTION
        IGRID=255
        IRESFL=0
        ISCAN=2
        LAT1=180.E3/PI*XLAT1
        LON1=180.E3/PI*XLON1
        IGDS09=ORIENT*1.E3
        IGDS10=DELX
        IGDS11=DELY
        IGDS12=PROJ
      ELSE
        IERR=40
        RETURN
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  RESET TIME RANGE PARAMETER IN CASE OF OVERFLOW
      IF(ITR.GE.2.AND.ITR.LE.5.AND.IP2.GE.256) THEN
        JP1=IP2
        JP2=0
        JTR=10
      ELSE
        JP1=IP1
        JP2=IP2
        JTR=ITR
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FILL PDS PARAMETERS
      IPDS(01)=ILPDS    ! LENGTH OF PDS
      IPDS(02)=IPTV     ! PARAMETER TABLE VERSION ID
      IPDS(03)=ICEN     ! CENTER ID
      IPDS(04)=IGEN     ! GENERATING MODEL ID
      IPDS(05)=IGRID    ! GRID ID
      IPDS(06)=1        ! GDS FLAG
      IPDS(07)=IBMS     ! BMS FLAG
      IPDS(08)=IPU      ! PARAMETER UNIT ID
      IPDS(09)=ITL      ! TYPE OF LEVEL ID
      IPDS(10)=IL1      ! LEVEL 1 OR 0
      IPDS(11)=IL2      ! LEVEL 2
c     IPDS(12)=IYR      ! YEAR
      IPDS(13)=IMO      ! MONTH
      IPDS(14)=IDY      ! DAY
      IPDS(15)=IHR      ! HOUR
      IPDS(16)=0        ! MINUTE
      IPDS(17)=IFTU     ! FORECAST TIME UNIT ID
      IPDS(18)=JP1      ! TIME PERIOD 1
      IPDS(19)=JP2      ! TIME PERIOD 2 OR 0
      IPDS(20)=JTR      ! TIME RANGE INDICATOR
      IPDS(21)=INA      ! NUMBER IN AVERAGE
      IPDS(22)=INM      ! NUMBER MISSING
c     IPDS(23)=20       ! CENTURY
c     y2k mod
      if (mod(IYR,100).eq.0) then
           IPDS(12) = 100
           IPDS(23) = IYR/100
      else
           IPDS(12) = mod(IYR,100)
           IPDS(23) = IYR/100 + 1
           if (IPDS(23).eq.1) IPDS(23) = 20
      endif
      IPDS(24)=ICEN2    ! FORECAST SUBCENTER
      IPDS(25)=IDS      ! DECIMAL SCALING
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FILL GDS AND BDS PARAMETERS
      IGDS(01)=0        ! NUMBER OF VERTICAL COORDS
      IGDS(02)=255      ! VERTICAL COORD FLAG
      IGDS(03)=IDRT     ! DATA REPRESENTATION TYPE
      IGDS(04)=IM       ! EAST-WEST POINTS
      IGDS(05)=JM       ! NORTH-SOUTH POINTS
      IGDS(06)=LAT1     ! LATITUDE OF ORIGIN
      IGDS(07)=LON1     ! LONGITUDE OF ORIGIN
      IGDS(08)=IRESFL   ! RESOLUTION FLAG
      IGDS(09)=IGDS09   ! LATITUDE OF END OR ORIENTATION
      IGDS(10)=IGDS10   ! LONGITUDE OF END OR DX IN METER ON 60N
      IGDS(11)=IGDS11   ! LAT INCREMENT OR GAUSSIAN LATS OR DY IN METER
      IGDS(12)=IGDS12   ! LONGITUDE INCREMENT OR PROJECTION
      IGDS(13)=ISCAN    ! SCANNING MODE FLAGS
c     IGDS(14:18)=0     ! NOT USED
      do i = 14, 18
	igds(i) = 0
      enddo
c     IBDS(1:9)=0       ! BDS FLAGS
      do i = 1, 9
          ibds(i) = 0
      enddo
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  FILL BITMAP AND COUNT VALID DATA.  RESET BITMAP FLAG IF ALL VALID.
      NBM=NF
      IF(IBMS.NE.0) THEN
        NBM=0
        DO I=1,NF
          IF(LBM(I)) THEN
            IBM(I)=1
            NBM=NBM+1
          ELSE
            IBM(I)=0
          ENDIF
        ENDDO
        IF(NBM.EQ.NF) IPDS(7)=0
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  ROUND DATA AND DETERMINE NUMBER OF BITS
      IF(NBM.EQ.0) THEN
        DO I=1,NF
          FR(I)=0.
        ENDDO
        NBIT=0
      ELSE
        CALL GTBITS(IPDS(7),IDS,NF,IBM,F,FR,FMIN,FMAX,NBIT)
        IF(MXBIT.GT.0) NBIT=MIN(NBIT,MXBIT)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CREATE PRODUCT DEFINITION SECTION
      CALL W3FI68(IPDS,PDS)
      IF(ICEN2.EQ.2.AND.ILPDS.GE.45) THEN
        ILAST=45
        CALL PDSENS(IENS,KPROB,XPROB,KCLUST,KMEMBR,ILAST,PDS)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CREATE GRIB MESSAGE
      CALL W3FI72(0,FR,0,NBIT,1,IPDS,PDS,
     1            1,255,IGDS,0,0,IBM,NF,IBDS,
     1            NFO,GRIB,LGRIB,IERR)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
C-----------------------------------------------------------------------
