      PROGRAM SFCTEST
C
C  Program to call SURFACE subroutine
C
C  LUGI,LUGB are 2 unit numbers used in the subprogram
C  IDIM,JDIM ... Gaussian grid dimension
C  LSOIL .. Number of soil layers (2 as of April, 1994)
C  IY,IM,ID,IH .. Year, month, day, and hour of initial state.
C  FH .. Forecast hour
C  SIG1T .. Sigma level 1 temperature for dead start.  Should be on Gaussian
C           grid.  If not dead start, no need for dimension but set to zero
C           as in the example below.
C
      NAMELIST/NAMTEST/ IDIM,JDIM,LSOIL,LUGI,LUGB,
     1                  IY,IM,ID,IH,FH
C
      DATA IDIM,JDIM,LSOIL/192,94,2/
      DATA LUGI,LUGB/11,12/
C
      READ(5,NAMTEST)
      WRITE(6,NAMTEST)
C
      IJDIM=IDIM*JDIM
C
      SIG1T=0.
C
      CALL SURFACE(LUGI,LUGB,IDIM,JDIM,LSOIL,
     1             IY,IM,ID,IH,FH,SIG1T)

        write(*,*) 'normal end of cycle'
      STOP
      END
      SUBROUTINE SURFACE(LUGI,LUGB,IDIM,JDIM,LSOIL,
     1                   IY,IM,ID,IH,FH,SIG1T)
C
C  THIS IS A VERSION II SURFACE PROGRAM.
C
C  This program runs in two different modes:
C
C  1.  Analysis mode (FH=0.)
C
C      This program merges climatology, analysis and forecast guess to create
C      new surface fields (BGES).  If analysis file is given, the program 
C      uses it if date of the analysis matches with IY,IM,ID,IH (see Note
C      below).
C
C  2.  Forecast mode (FH.GT.0.)
C    
C      This program makes climatology evolve with forecast hour.  If surface
C      analysis file is given, the program picks it up.  This is forcing-by-
C      observation experiment.  The program matches the dates with IY,IM,ID,
C      IH and FH (see Note below).
C
C   NOTE:
C
C      If date of the analysis does not match with IY,IM,ID,IH,
C      the program look for dates going back to NREPMX (Parameter
C      statement in the SUBROTINE FIXRD. Now defined as 8).
C      This is not to use the old analysis but to make sure that proper
C      file name is given.  This allows the user to provide non-daily
C      analysis to be used.  Note that in this case, forecast guess is
C      used.
C
C      Use of earlier analysis for current analysis is NOT allowed.
C      (as was done in the old version for snow analysis in which
C      old snow analysis is used in combination with initial guess).
C
C      For dead start, do not supply FNBGSI or set FNBGSI='        ' 
C
C  LUGI,LUGB are 2 unit numbers used in this subprogram
C  IDIM,JDIM ... Gaussian grid dimension in x and y direction, respectovely
C  LSOIL .. Number of soil layers (2 as of April, 1994)
C  IY,IM,ID,IH .. Year, month, day, and hour of initial state.
C  FH .. Forecast hour
C  SIG1T .. Sigma level 1 temperature for dead start.  Should be on Gaussian
C           grid.  If not dead start, no need for dimension but set to zero
C           as in the example below.
C
C  Variable naming conventions:
C
C     ORO .. Orography
C     ALB .. Albedo
C     WET .. Soil wetness as defined for bucket model
C     SNO .. Snow DEPTH
C     ZOR .. Surface roughness length
C     PLR .. Plant evaporation resistance
C     TSF .. Surface skin temperature.  Sea surface temp. over ocean.
C     TG3 .. Deep soil temperature (at 500cm)
C     STC .. Soil temperature (LSOIL layrs)
C     SMC .. Soil moisture (LSOIL layrs)
C     SCV .. Snow cover (not snow depth)
C     AIS .. Sea ice mask (0 or 1)
C     ACN .. Sea ice concentration (fraction)
C     GLA .. Glacier (permanent snow) mask (0 or 1)
C     MXI .. Maximum sea ice extent (0 or 1)
C     MSK .. Land ocean mask (0=ocean 1=land)
C     CNP .. Canopy water content
C     CV  .. Convective cloud cover
C     CVB .. Convective cloud base
C     CVT .. Convective cloud top
C     SLI .. LAND/SEA/SEA-ICE mask. (1/0/2 respectively)
C
C  Definition of Land/Sea mask. SLLND for land and SLSEA for sea.
C  Definition of Sea/ice mask. AICICE for ice, AICSEA for sea.
C  TGICE=max ice temperature
C  RLAPSE=lapse rate for sst correction due to surface angulation
C
      PARAMETER(SLLND =1.0,SLSEA =0.0)
      PARAMETER(AICICE=1.0,AICSEA=0.0)
      PARAMETER(TGICE=271.2)
      PARAMETER(RLAPSE=0.65E-2)
C
C  Max/Min of fields for check and replace.
C
C     ???LMX .. Max over bare land
C     ???LMN .. Min over bare land
C     ???OMX .. Max over open ocean
C     ???OMN .. Min over open ocean
C     ???SMX .. Max over snow surface (land and sea-ice)
C     ???SMN .. Min over snow surface (land and sea-ice)
C     ???IMX .. Max over bare sea ice
C     ???IMN .. Min over bare sea ice
C     ???JMX .. Max over snow covered sea ice
C     ???JMN .. Min over snow covered sea ice
C
      PARAMETER(OROLMX=8000.,OROLMN=-1000.,OROOMX=3000.,OROOMN=-1000.,
     1          OROSMX=8000.,OROSMN=-1000.,OROIMX=3000.,OROIMN=-1000.,
     2          OROJMX=3000.,OROJMN=-1000.)
      PARAMETER(ALBLMX=0.80,ALBLMN=0.06,ALBOMX=0.06,ALBOMN=0.06,
     1          ALBSMX=0.80,ALBSMN=0.06,ALBIMX=0.80,ALBIMN=0.80,
     2          ALBJMX=0.80,ALBJMN=0.80)
      PARAMETER(WETLMX=0.15,WETLMN=0.00,WETOMX=0.15,WETOMN=0.15,
     1          WETSMX=0.15,WETSMN=0.15,WETIMX=0.15,WETIMN=0.15,
     2          WETJMX=0.15,WETJMN=0.15)
      PARAMETER(SNOLMX=0.0,SNOLMN=0.0,SNOOMX=0.0,SNOOMN=0.0,
     1          SNOSMX=55000.,SNOSMN=0.01,SNOIMX=0.,SNOIMN=0.0,
     2          SNOJMX=10000.,SNOJMN=0.01)
      PARAMETER(ZORLMX=300.,ZORLMN=2.,ZOROMX=1.0,ZOROMN=1.E-05,
     1          ZORSMX=300.,ZORSMN=2.,ZORIMX=1.0,ZORIMN=1.0,
     2          ZORJMX=1.0,ZORJMN=1.0)
      PARAMETER(PLRLMX=1000.,PLRLMN=0.0,PLROMX=1000.0,PLROMN=0.0,
     1          PLRSMX=1000.,PLRSMN=0.0,PLRIMX=1000.,PLRIMN=0.0,
     2          PLRJMX=1000.,PLRJMN=0.0)
      PARAMETER(TSFLMX=353.,TSFLMN=173.0,TSFOMX=313.0,TSFOMN=271.21,
     1          TSFSMX=273.16,TSFSMN=173.0,TSFIMX=271.21,TSFIMN=173.0,
     2          TSFJMX=273.16,TSFJMN=173.0)
      PARAMETER(TG3LMX=310.,TG3LMN=200.0,TG3OMX=310.0,TG3OMN=200.0,
     1          TG3SMX=310.,TG3SMN=200.0,TG3IMX=310.0,TG3IMN=200.0,
     2          TG3JMX=310.,TG3JMN=200.0)
      PARAMETER(STCLMX=353.,STCLMN=173.0,STCOMX=313.0,STCOMN=200.0,
     1          STCSMX=310.,STCSMN=200.0,STCIMX=310.0,STCIMN=200.0,
     2          STCJMX=310.,STCJMN=200.0)
      PARAMETER(SMCLMX=0.55,SMCLMN=0.0,SMCOMX=0.55,SMCOMN=0.0,
     1          SMCSMX=0.55,SMCSMN=0.0,SMCIMX=0.55,SMCIMN=0.0,
     2          SMCJMX=0.55,SMCJMN=0.0)
      PARAMETER(SCVLMX=0.0,SCVLMN=0.0,SCVOMX=0.0,SCVOMN=0.0,
     1          SCVSMX=1.0,SCVSMN=1.0,SCVIMX=0.0,SCVIMN=0.0,
     2          SCVJMX=1.0,SCVJMN=1.0)
C
C  Criteria used for monitoring
C
      PARAMETER(EPSTSF=0.01,EPSALB=0.001,EPSSNO=0.01,
     1          EPSWET=0.01,EPSZOR=0.0000001,EPSPLR=1.,EPSORO=0.,
     2          EPSSMC=0.0001,EPSSCV=0.,EPTSFC=0.01,EPSTG3=0.01,
     3          EPSAIS=0.,EPSACN=0.01)
C
C  Quality control of analysis snow and sea ice
C
C   QCTSFS .. Surface temperature above which no snow allowed
C   QCSNOS .. Snow depth above which snow must exist
C   QCTSFI .. SST above which sea-ice is not allowed
C
      PARAMETER(QCTSFS=283.16,QCSNOS=100.,QCTSFI=275.16)
C
C  Ice concentration for ice limit (55%)
C
      PARAMETER(AISLIM=0.55)
C
C  Parameters to obtain snow depth from snow cover and temperatture
C
      PARAMETER(SNWMIN=25.,SNWMAX=100.)
C
C  COEEFICIENTS OF BLENDING FORECAST AND INTERPOLATED CLIM
C  (OR ANALYZED) FIELDS OVER SEA OR LAND(L) (NOT FOR CLOUDS)
C  1.0 = USE OF FORECAST
C  0.0 = REPLACE WITH INTERPOLATED ANALYSIS
C
C    These values are set for analysis mode.
C
C   Variables                  Land                 Sea
C   ---------------------------------------------------------
C   Surface temperature        Forecast             Analysis
C   Albedo                     Analysis             Analysis
C   Snow                       Analysis             Forecast (over sea ice)
C   Roughness                  Analysis             Forecast
C   Plant resistance           Analysis             Analysis
C   Soil wetness (layer)       Weighted average     Analysis
C   Soil temperature           Forecast             Analysis
C   Canopy waver content       Forecast             Forecast
C   Convective cloud cover     Forecast             Forecast
C   Convective cloud bottm     Forecast             Forecast
C   Convective cloud top       Forecast             Forecast
C
C  Note: If analysis file is not given, then time interpolated climatology
C        is used.  If analyiss file is given, it will be used as far as the 
C        date and time matches.  If they do not match, it uses forecast.
C
      PARAMETER(CTSFL=1.0,CTSFS=0.0)
      PARAMETER(CALBL=0.0,CALBS=0.0)
      PARAMETER(CSNOL=0.0,CSNOS=1.0)
      PARAMETER(CZORL=0.0,CZORS=1.0)
      PARAMETER(CPLRL=0.0,CPLRS=0.0)
      PARAMETER(CSMCL=0.99583,CSMCS=0.0)
      PARAMETER(CSTCL=1.0,CSTCS=0.0)
      PARAMETER(CCNP =1.0)
      PARAMETER(CCV  =1.0,CCVB =1.0,CCVT =1.0)
C
C  Critical percentage value for aborting bad points when LGCHEK=.T.
C
      LOGICAL LGCHEK
      DATA LGCHEK/.true./
      DATA CRITP1,CRITP2,CRITP3/80.,80.,25./
C
C  GRIB code for each parameter
C    Note!!!! Same parameter statement used in subroutine SETRMSK.
C
      PARAMETER(KPDTSF=11,KPDWET=86,KPDSNO=65,KPDZOR=83,
     1          KPDALB=84,KPDAIS=91,KPDTG3=11,KPDPLR=224,
     2          KPDGLA=238,KPDMXI=91,KPDSCV=238,
     3          KPDORO=8,KPDMSK=81,KPDSTC=11,KPDACN=91)
C
C  MASK OROGRAPHY AND VARIANCE ON GAUSSIAN GRID
C
      CHARACTER*80 FNOROG,FIOROG,FNMASK,FIMASK
      DIMENSION SLMASK(IDIM*JDIM),OROG(IDIM*JDIM)
C
C  Permanent/extremes
C
      CHARACTER*80 FNGLAC,FNMXIC
      CHARACTER*80 FIGLAC,FIMXIC
      DIMENSION GLACIR(IDIM*JDIM),AMXICE(IDIM*JDIM)
C
C  CLIMATOLOGY SURFACE FIELDS (Last character 'C' or 'CLM' indicate CLIMATOLOGY)
C
      CHARACTER*80 FNTSFC,FNWETC,FNSNOC,FNZORC,FNALBC,FNAISC,
     1             FNPLRC,FNTG3C,FNSCVC,FNSMCC,FNSTCC,FNACNC 
      CHARACTER*80 FITSFC,FIWETC,FISNOC,FIZORC,FIALBC,FIAISC,
     1             FIPLRC,FITG3C,FISCVC,FISMCC,FISTCC,FIACNC
      DIMENSION TSFCLM(IDIM*JDIM),WETCLM(IDIM*JDIM),SNOCLM(IDIM*JDIM),
     1          ZORCLM(IDIM*JDIM),ALBCLM(IDIM*JDIM),AISCLM(IDIM*JDIM),
     2          TG3CLM(IDIM*JDIM),PLRCLM(IDIM*JDIM),ACNCLM(IDIM*JDIM),
     3          CVCLM (IDIM*JDIM),CVBCLM(IDIM*JDIM),CVTCLM(IDIM*JDIM),
     4          CNPCLM(IDIM*JDIM),
     5          SMCCLM(IDIM*JDIM,LSOIL),STCCLM(IDIM*JDIM,LSOIL),
     6          SLICLM(IDIM*JDIM), 
     7          SCVCLM(IDIM*JDIM)
C
C  Climatological TSF at forcast hour=0.
C
      DIMENSION TSFCL0(IDIM*JDIM) 
C
C  ANALYZED SURFACE FIELDS (Last character 'A' or 'ANL' indicate ANALYSIS)
C
      CHARACTER*80 FNTSFA,FNWETA,FNSNOA,FNZORA,FNALBA,FNAISA,
     1             FNPLRA,FNTG3A,FNSCVA,FNSMCA,FNSTCA,FNACNA
      CHARACTER*80 FITSFA,FIWETA,FISNOA,FIZORA,FIALBA,FIAISA,
     1             FIPLRA,FITG3A,FISCVA,FISMCA,FISTCA,FIACNA
      DIMENSION TSFANL(IDIM*JDIM),WETANL(IDIM*JDIM),SNOANL(IDIM*JDIM),
     1          ZORANL(IDIM*JDIM),ALBANL(IDIM*JDIM),AISANL(IDIM*JDIM),
     2          TG3ANL(IDIM*JDIM),PLRANL(IDIM*JDIM),ACNANL(IDIM*JDIM),
     3          CVANL (IDIM*JDIM),CVBANL(IDIM*JDIM),CVTANL(IDIM*JDIM),
     4          CNPANL(IDIM*JDIM),
     5          SMCANL(IDIM*JDIM,LSOIL),STCANL(IDIM*JDIM,LSOIL),
     6          SLIANL(IDIM*JDIM),
     7          SCVANL(IDIM*JDIM)
C
C  PREDICTED SURFACE FIELDS (Last characters 'FCS' indicates FORECAST)
C
      DIMENSION TSFFCS(IDIM*JDIM),WETFCS(IDIM*JDIM),SNOFCS(IDIM*JDIM),
     1          ZORFCS(IDIM*JDIM),ALBFCS(IDIM*JDIM),AISFCS(IDIM*JDIM),
     2          TG3FCS(IDIM*JDIM),PLRFCS(IDIM*JDIM),ACNFCS(IDIM*JDIM),
     3          CVFCS (IDIM*JDIM),CVBFCS(IDIM*JDIM),CVTFCS(IDIM*JDIM),
     4          CNPFCS(IDIM*JDIM),
     5          SMCFCS(IDIM*JDIM,LSOIL),STCFCS(IDIM*JDIM,LSOIL),
     6          SLIFCS(IDIM*JDIM)
C
C Ratio of sigma level 1 wind and 10m wind (diagnozed by model and not touched
C in this program).
C
      DIMENSION F10M  (IDIM*JDIM)
C
C  Input and output SURFACE FIELDS (BGES) file names
C
      CHARACTER*80 FNBGSI,FNBGSO
C
C  Sigma level 1 temperature for dead start
C
      DIMENSION SIG1T(IDIM*JDIM)
C
      CHARACTER*32 LABEL
C
C  = 1 ==> FORECAST IS USED
C  = 0 ==> ANALYSIS (OR CLIMATOLOGY) IS USED
C
C     OUTPUT FILE  ... PRIMARY SURFACE FILE FOR RADIATION AND FORECAST
C
C       REC.  1    LABEL
C       REC.  2    DATE RECORD
C       REC.  3    TSF
C       REC.  4    SOILM(TWO LAYERS)
C       REC.  5    SNOW
C       REC.  6    SOILT(TWO LAYERS)
C       REC.  7    TG3
C       REC.  8    ZOR
C       REC.  9    CV
C       REC. 10    CVB
C       REC. 11    CVT
C       REC. 12    ALBEDO
C       REC. 13    SLIMSK
C       REC. 14    PLANTR
C       REC. 15    F10M
C       REC. 16    CANOPY WATER CONTENT (CNPANL)
C
C  LAT/LON of GAUSSIAN GRID FOR MONITORING
C
      DIMENSION GAUL(JDIM),RLA(IDIM*JDIM),RLO(IDIM*JDIM)
C
C  Debug only
C   LDEBUG=.T. creates BGES files for climatology and analysis
C   LQCBGS=.T. Quality controls input BGES file before merging (should have been
C              QCed in the forecast program)
C
      LOGICAL LDEBUG,LQCBGS
C
C  Debug only
C
      CHARACTER*80 FNDCLM,FNDANL
C
      NAMELIST/NAMSFC/FNGLAC,FNMXIC,FIGLAC,FIMXIC,
     2                FNTSFC,FNWETC,FNSNOC,FNZORC,FNALBC,FNAISC,
     3                FNPLRC,FNTG3C,FNSCVC,FNSMCC,FNSTCC,FNACNC,
     4                FITSFC,FIWETC,FISNOC,FIZORC,FIALBC,FIAISC,
     5                FIPLRC,FITG3C,FISCVC,FISMCC,FISTCC,FIACNC,
     6                FNTSFA,FNWETA,FNSNOA,FNZORA,FNALBA,FNAISA,
     7                FNPLRA,FNTG3A,FNSCVA,FNSMCA,FNSTCA,FNACNA,
     8                FITSFA,FIWETA,FISNOA,FIZORA,FIALBA,FIAISA,
     9                FIPLRA,FITG3A,FISCVA,FISMCA,FISTCA,FIACNA,
     A                FNOROG,FIOROG,FNMASK,FIMASK,
     B                FNBGSI,FNBGSO,
     C                LDEBUG,LGCHEK,LQCBGS,CRITP1,CRITP2,CRITP3,
     D                FNDCLM,FNDANL
C
C  Defaults file names
C
      DATA FNOROG/'        '/
      DATA FIOROG/'        '/
      DATA FNMASK/'        '/
      DATA FIMASK/'        '/
C
      DATA FNGLAC/'/reanl2/sfcclm/glacier'/
      DATA FIGLAC/'/reanl2/sfcclm/glacier.index'/
      DATA FNMXIC/'/reanl2/sfcclm/maxice'/
      DATA FIMXIC/'/reanl2/sfcclm/maxice.index'/
C
      DATA FNTSFC/'/reanl2/sfcclm/sst'/
      DATA FITSFC/'/reanl2/sfcclm/sst.index'/
      DATA FNWETC/'/reanl2/sfcclm/soilwet'/
      DATA FIWETC/'/reanl2/sfcclm/soilwet.index'/
      DATA FNSNOC/'/reanl2/sfcclm/snow'/
      DATA FISNOC/'/reanl2/sfcclm/snow.index'/
      DATA FNZORC/'/reanl2/sfcclm/sibrough'/
      DATA FIZORC/'/reanl2/sfcclm/sibrough.index'/
      DATA FNALBC/'/reanl2/sfcclm/sibalbedo'/
      DATA FIALBC/'/reanl2/sfcclm/sibalbedo.index'/
      DATA FNAISC/'/reanl2/sfcclm/ice'/
      DATA FIAISC/'/reanl2/sfcclm/ice.index'/
      DATA FNPLRC/'/reanl2/sfcclm/sibresis'/
      DATA FIPLRC/'/reanl2/sfcclm/sibresis.index'/
      DATA FNTG3C/'/reanl2/sfcclm/tg3'/
      DATA FITG3C/'/reanl2/sfcclm/tg3.index'/
      DATA FNSMCC/'        '/
      DATA FISMCC/'        '/
      DATA FNSTCC/'        '/
      DATA FISTCC/'        '/
      DATA FNSCVC/'        '/
      DATA FISCVC/'        '/
      DATA FNACNC/'        '/
      DATA FIACNC/'        '/
C     
      DATA FNTSFA/'        '/
      DATA FITSFA/'        '/
      DATA FNWETA/'        '/
      DATA FIWETA/'        '/
      DATA FNSNOA/'        '/
      DATA FISNOA/'        '/
      DATA FNZORA/'        '/
      DATA FIZORA/'        '/
      DATA FNALBA/'        '/
      DATA FIALBA/'        '/
      DATA FNAISA/'        '/
      DATA FIAISA/'        '/
      DATA FNPLRA/'        '/
      DATA FIPLRA/'        '/
      DATA FNTG3A/'        '/
      DATA FITG3A/'        '/
      DATA FNSMCA/'        '/
      DATA FISMCA/'        '/
      DATA FNSTCA/'        '/
      DATA FISTCA/'        '/
      DATA FNSCVA/'        '/
      DATA FISCVA/'        '/
      DATA FNACNA/'        '/
      DATA FIACNA/'        '/
C
      DATA FNBGSI/'        '/
      DATA FNBGSO/'        '/
C
      DATA LDEBUG/.false./,LQCBGS/.true./
      DATA FNDCLM/'        '/
      DATA FNDANL/'        '/
C
C  ZONAL DIAGNOSTICS ARRAYS
C
      PARAMETER(NRCZNL=14)
      LOGICAL LSMSK(IDIM,2,6)
      DIMENSION ZNLSL(6,6,NRCZNL),WEIS(6,6)
C
      DATA IFP/0/
C
      SAVE IFP,FNGLAC,FNMXIC,FIGLAC,FIMXIC,
     2     FNTSFC,FNWETC,FNSNOC,FNZORC,FNALBC,FNAISC,
     3     FNPLRC,FNTG3C,FNSCVC,FNSMCC,FNSTCC,FNACNC,
     4     FITSFC,FIWETC,FISNOC,FIZORC,FIALBC,FIAISC,
     5     FIPLRC,FITG3C,FISCVC,FISMCC,FISTCC,FIACNC,
     6     FNTSFA,FNWETA,FNSNOA,FNZORA,FNALBA,FNAISA,
     7     FNPLRA,FNTG3A,FNSCVA,FNSMCA,FNSTCA,FNACNA,
     8     FITSFA,FIWETA,FISNOA,FIZORA,FIALBA,FIAISA,
     9     FIPLRA,FITG3A,FISCVA,FISMCA,FISTCA,FIACNA,
     A     FNOROG,FIOROG,FNMASK,FIMASK,
     B     FNBGSI,FNBGSO,
     C     LDEBUG,LGCHEK,LQCBGS,CRITP1,CRITP2,CRITP3,
     D     FNDCLM,FNDANL
C
      IF(IFP.EQ.0) THEN
        IFP=1
        READ (5,NAMSFC)
        WRITE(6,NAMSFC)
      ENDIF
C
      IJDIM=IDIM*JDIM
C
C  Set default F10M for debugging for dead start
C
      F10M=1.
C
      WRITE(6,*) ' '
      WRITE(6,*) 'LUGI=',LUGI,' LUGB=',LUGB
      WRITE(6,*) 'IDIM=',IDIM,' JDIM=',JDIM,' IJDIM=',IJDIM,
     1           ' LSOIL=',LSOIL
      WRITE(6,*) 'IY=',IY,' IM=',IM,' ID=',ID,' IH=',IH,' FH=',FH
      WRITE(6,*) 'SIG1T(1)=',SIG1T(1)
      WRITE(6,*) ' '
C
      IDIMT=IDIM*2
C
C  COMPUTE GAUSSIAN LATITUDE FOR MONITORING
C
      DX=360./FLOAT(IDIM)
      CALL GAULAT(GAUL,JDIM)
C
      DO J=1,JDIM
        DO I=1,IDIM
          RLA((J-1)*IDIM+I)=90.-GAUL(J)
          RLO((J-1)*IDIM+I)=FLOAT(I-1)*DX
          IF(RLO((J-1)*IDIM+I).GT.180.) THEN
            RLO((J-1)*IDIM+I)=RLO((J-1)*IDIM+I)-360.
          ENDIF
        ENDDO
      ENDDO
C
C  READ IN LAND/SEA MASK, OROGRAPHY ON GIVEN GAUSSIAN GRIDS. 
C  NOTE OROGRAPHY IS COMPUTED FROM SPHERICAL COEFF WITH ANGULATIONS OVER OCEAN
C
      WRITE(6,*) '=============='
      WRITE(6,*) '   MASK/OROG'
      WRITE(6,*) '=============='
C
      PERCRIT=CRITP1
C
C  Read in orography and land/sea mask
C
      CALL MSKRD(LUGI,LUGB,IDIM,JDIM,IJDIM,IY,IM,ID,IH,FH,
     1           FNOROG,FNMASK,FIOROG,FIMASK,KPDORO,KPDMSK,OROG,SLMASK)
C
C  Quality control of MASK
C
      CALL QCMASK(SLMASK,SLLND,SLSEA,IDIM,JDIM,RLA,RLO)
C
C  Quality control of Orography
C
      SLICLM=1.
      SNOCLM=0.
      CALL QCMXMN('Orog',OROG,SLICLM,SNOCLM,
     1            OROLMX,OROLMN,OROOMX,OROOMN,OROIMX,OROIMN,
     2            OROJMX,OROJMN,OROSMX,OROSMN,EPSORO,
     3            RLA,RLO,IJDIM,0,PERCRIT,LGCHEK)
C
C  Reading permanent/extreme features (glacier points and maximum ice extent)
C
      CALL PEREX(LUGI,LUGB,IDIM,JDIM,IJDIM,SLMASK,IY,IM,ID,IH,FH,
     1          FNGLAC,FNMXIC,FIGLAC,FIMXIC,KPDGLA,KPDMXI,GLACIR,AMXICE)
      CALL ROF01(GLACIR,IJDIM,'GE',0.5)
      CALL ROF01(AMXICE,IJDIM,'GE',0.5)
C
C  Quality control max ice limit based on glacier points
C
      CALL QCMXICE(GLACIR,AMXICE,IJDIM)
C
C  Read climatology fields
C
      WRITE(6,*) '=============='
      WRITE(6,*) 'CLIMATOLOGY'
      WRITE(6,*) '=============='
C
      PERCRIT=CRITP1
C
      CALL CLIMA(LUGI,LUGB,IY,IM,ID,IH,FH,IDIM,JDIM,IJDIM,LSOIL,SLMASK,
     Z           FNTSFC,FNWETC,FNSNOC,FNZORC,FNALBC,FNAISC,
     1           FNPLRC,FNTG3C,FNSCVC,FNSMCC,FNSTCC,FNACNC,
     2           FITSFC,FIWETC,FISNOC,FIZORC,FIALBC,FIAISC,
     3           FIPLRC,FITG3C,FISCVC,FISMCC,FISTCC,FIACNC,
     4           TSFCLM,WETCLM,SNOCLM,ZORCLM,ALBCLM,AISCLM,
     5           TG3CLM,PLRCLM,CVCLM ,CVBCLM,CVTCLM,
     6           CNPCLM,SMCCLM,STCCLM,SLICLM,SCVCLM,ACNCLM,
     7           KPDTSF,KPDWET,KPDSNO,KPDZOR,KPDALB,KPDAIS,
     8           KPDTG3,KPDPLR,KPDSCV,KPDACN,TSFCL0)
C
C  Scale surface roughness and albedo to model required units
C
      CALL SCALE(ZORCLM,IJDIM,100.)
      CALL SCALE(ALBCLM,IJDIM,0.01)
C
C  Set albedo over ocean to ALBOMX
C
      CALL ALBOCN(ALBCLM,SLMASK,ALBOMX,IJDIM)
C
C  Ice concentration or ice mask (only ice mask used in the model now)
C
      IF(FNAISC(1:8).NE.'        ') THEN
        CALL ROF01(AISCLM,IJDIM,'GE',0.5)
      ELSEIF(FNACNC(1:8).NE.'        ') THEN
        CALL ROF01(ACNCLM,IJDIM,'GE',AISLIM)
        AISCLM=ACNCLM
      ENDIF
C
C  Quality control of sea ice mask
C
      CALL QCSICE(AISCLM,GLACIR,AMXICE,AICICE,AICSEA,SLLND,SLMASK,
     1            RLA,RLO,IDIM,JDIM)
C
C  Set ocean/land/sea-ice mask
C
      CALL SETLSI(SLMASK,AISCLM,IJDIM,AICICE,SLICLM)
C     WRITE(6,*) 'SLICLM'
C     CALL NNTPRT(SLICLM,IDIM,JDIM)
C
C  Quality control of snow
C
      CALL QCSNOW(SNOCLM,SLMASK,AISCLM,GLACIR,IJDIM,SNOSMX)
C
      CALL SETZRO(SNOCLM,EPSSNO,IJDIM)
C
C  Snow cover handling (We assume climatological snow depth is available)
C  Quality control of snow depth (Note that Snow should be corrected first
C  because it influences TSF
C
      CALL QCMXMN('Snow',SNOCLM,SLICLM,SNOCLM,
     1            SNOLMX,SNOLMN,SNOOMX,SNOOMN,SNOIMX,SNOIMN,
     2            SNOJMX,SNOJMN,SNOSMX,SNOSMN,EPSSNO,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
C     WRITE(6,*) 'SNOCLM'
C     CALL NNTPRT(SNOCLM,IDIM,JDIM)
C
C  Get snow cover from snow depth array
C
      IF(FNSCVC(1:8).EQ.'        ') THEN
        CALL GETSCV(SNOCLM,SCVCLM,IJDIM)
      ENDIF
C
C  Set TSFC over snow to TSFSMX if greater
C
      CALL SNOSFC(SNOCLM,TSFCLM,TSFSMX,IJDIM)
C
C  Quality control
C
      CALL QCMXMN('TSFc',TSFCLM,SLICLM,SNOCLM,
     1            TSFLMX,TSFLMN,TSFOMX,TSFOMN,TSFIMX,TSFIMN,
     2            TSFJMX,TSFJMN,TSFSMX,TSFSMN,EPSTSF,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
      CALL QCMXMN('ALBc',ALBCLM,SLICLM,SNOCLM,
     1            ALBLMX,ALBLMN,ALBOMX,ALBOMN,ALBIMX,ALBIMN,
     2            ALBJMX,ALBJMN,ALBSMX,ALBSMN,EPSALB,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
      CALL QCMXMN('WETc ',WETCLM,SLICLM,SNOCLM,
     1            WETLMX,WETLMN,WETOMX,WETOMN,WETIMX,WETIMN,
     2            WETJMX,WETJMN,WETSMX,WETSMN,EPSWET,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
      CALL QCMXMN('ZORc ',ZORCLM,SLICLM,SNOCLM,
     1            ZORLMX,ZORLMN,ZOROMX,ZOROMN,ZORIMX,ZORIMN,
     2            ZORJMX,ZORJMN,ZORSMX,ZORSMN,EPSZOR,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
      CALL QCMXMN('PLNTc',PLRCLM,SLICLM,SNOCLM,
     1            PLRLMX,PLRLMN,PLROMX,PLROMN,PLRIMX,PLRIMN,
     2            PLRJMX,PLRJMN,PLRSMX,PLRSMN,EPSPLR,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
      CALL QCMXMN('TG3c ',TG3CLM,SLICLM,SNOCLM,
     1            TG3LMX,TG3LMN,TG3OMX,TG3OMN,TG3IMX,TG3IMN,
     2            TG3JMX,TG3JMN,TG3SMX,TG3SMN,EPSTG3,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
C
C  Get soil temp and moisture (after all the QCs are completed)
C
      IF(FNSMCC(1:8).EQ.'        ') THEN
        CALL GETSMC(WETCLM,IJDIM,LSOIL,SMCCLM)
      ENDIF
      CALL QCMXMN('SMC1c',SMCCLM(1,1),SLICLM,SNOCLM,
     1            SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     2            SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
      CALL QCMXMN('SMC2c',SMCCLM(1,2),SLICLM,SNOCLM,
     1            SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     2            SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
      IF(FNSTCC(1:8).EQ.'        ') THEN
        CALL GETSTC(TSFCLM,TG3CLM,SLICLM,IJDIM,LSOIL,STCCLM,TSFIMX)
      ENDIF
      CALL QCMXMN('STC1c',STCCLM(1,1),SLICLM,SNOCLM,
     1            STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     2            STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
      CALL QCMXMN('STC2c',STCCLM(1,2),SLICLM,SNOCLM,
     1            STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     2            STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
C
C  MONITORING PRINTS
C
      PRINT *,' '
      PRINT *,'MONITOR OF TIME AND SPACE INTERPOLATED CLIMATOLOGY'
      PRINT *,' '
      CALL COUNT(SLICLM,SNOCLM,IJDIM)
      PRINT *,' '
      CALL MONITR('TSFCLM',TSFCLM,SLICLM,SNOCLM,IJDIM)
      CALL MONITR('ALBCLM',ALBCLM,SLICLM,SNOCLM,IJDIM)
      CALL MONITR('AISCLM',AISCLM,SLICLM,SNOCLM,IJDIM)
      CALL MONITR('SNOCLM',SNOCLM,SLICLM,SNOCLM,IJDIM)
      CALL MONITR('SCVCLM',SCVCLM,SLICLM,SNOCLM,IJDIM)
      CALL MONITR('SMCCLM1',SMCCLM(1,1),SLICLM,SNOCLM,IJDIM)
      CALL MONITR('SMCCLM2',SMCCLM(1,2),SLICLM,SNOCLM,IJDIM)
      CALL MONITR('STCCLM1',STCCLM(1,1),SLICLM,SNOCLM,IJDIM)
      CALL MONITR('STCCLM2',STCCLM(1,2),SLICLM,SNOCLM,IJDIM)
      CALL MONITR('TG3CLM',TG3CLM,SLICLM,SNOCLM,IJDIM)
      CALL MONITR('ZORCLM',ZORCLM,SLICLM,SNOCLM,IJDIM)
      CALL MONITR('CVACLM',CVCLM ,SLICLM,SNOCLM,IJDIM)
      CALL MONITR('CVBCLM',CVBCLM,SLICLM,SNOCLM,IJDIM)
      CALL MONITR('CVTCLM',CVTCLM,SLICLM,SNOCLM,IJDIM)
      CALL MONITR('SLICLM',SLICLM,SLICLM,SNOCLM,IJDIM)
      CALL MONITR('PLRCLM',PLRCLM,SLICLM,SNOCLM,IJDIM)
      CALL MONITR('OROG  ',OROG  ,SLICLM,SNOCLM,IJDIM)
C
      CALL ZNLSFC(SLICLM,IDIM,JDIM,IJDIM,LSOIL,NRCZNL,
     1            TSFCLM,WETCLM,SNOCLM,STCCLM,TG3CLM,
     2            ZORCLM,PLRCLM,CVCLM,CVBCLM,CVTCLM,
     3            ALBCLM,AISCLM,SMCCLM,CNPCLM,
     4            ZNLSL,NZL1,NZL2,LSMSK,WEIS,GAUL)
      CALL ZNLODY(FH,ZNLSL,NZL1,NZL2,LSMSK,WEIS,
     1            IDIM,JDIM,IDIMT,NRCZNL)
C
C  Debugging only
C
      IF(LDEBUG) THEN
        CALL BGSWRT(LUGI,FNDCLM,LABEL,FH,IY,IM,ID,IH,
     1              TSFCLM,SMCCLM,SNOCLM,STCCLM,TG3CLM,ZORCLM,
     2              CVCLM,CVBCLM,CVTCLM,ALBCLM,SLICLM,PLRCLM,
     3              F10M,CNPCLM,IJDIM,LSOIL)
      ENDIF
C
      WRITE(6,*) '=============='
      WRITE(6,*) '   ANALYSIS'
      WRITE(6,*) '=============='
C
C  Fill in analysis array with climatology before reading analysis.
C
      CALL FILANL(TSFANL,WETANL,SNOANL,ZORANL,ALBANL,AISANL,
     1            TG3ANL,PLRANL,CVANL ,CVBANL,CVTANL,
     2            CNPANL,SMCANL,STCANL,SLIANL,SCVANL,
     3            TSFCLM,WETCLM,SNOCLM,ZORCLM,ALBCLM,AISCLM,
     4            TG3CLM,PLRCLM,CVCLM ,CVBCLM,CVTCLM,
     5            CNPCLM,SMCCLM,STCCLM,SLICLM,SCVCLM,
     6            IJDIM,LSOIL)
C
C  Reverse scaling to match with grib analysis input
C
      CALL SCALE(ZORANL,IJDIM, 0.01)
      CALL SCALE(ALBANL,IJDIM,100.)
C
      PERCRIT=CRITP2
C
C  READ ANALYSIS FIELDS
C
      CALL ANALY(LUGI,LUGB,IY,IM,ID,IH,FH,IDIM,JDIM,IJDIM,LSOIL,SLMASK,
     Z           FNTSFA,FNWETA,FNSNOA,FNZORA,FNALBA,FNAISA,
     1           FNPLRA,FNTG3A,FNSCVA,FNSMCA,FNSTCA,FNACNA,
     2           FITSFA,FIWETA,FISNOA,FIZORA,FIALBA,FIAISA,
     3           FIPLRA,FITG3A,FISCVA,FISMCA,FISTCA,FIACNA,
     4           TSFANL,WETANL,SNOANL,ZORANL,ALBANL,AISANL,
     5           TG3ANL,PLRANL,CVANL ,CVBANL,CVTANL,
     6           SMCANL,STCANL,SLIANL,SCVANL,ACNANL,
     7           KPDTSF,KPDWET,KPDSNO,KPDZOR,KPDALB,KPDAIS,
     8           KPDTG3,KPDPLR,KPDSCV,KPDACN,KPDSMC,KPDSTC,
     7           IRTTSF,IRTWET,IRTSNO,IRTZOR,IRTALB,IRTAIS,
     8           IRTTG3,IRTPLR,IRTSCV,IRTACN,IRTSMC,IRTSTC)
C
C  Scale ZOR and ALB to match forecast model units
C
      CALL SCALE(ZORANL,IJDIM, 100.)
      CALL SCALE(ALBANL,IJDIM,0.01)
C
C  Interpolate climatology but fixing initial anomaly
C
      IF(FH.GT.0.0.AND.FNTSFA(1:8).EQ.'        ') THEN
        CALL ANOMINT(TSFANL,TSFCLM,TSFCL0,IJDIM)
      ENDIF
C
C  Ice concentration or ice mask (only ice mask used in the model now)
C
      IF(FNAISA(1:8).NE.'        ') THEN
        CALL ROF01(AISANL,IJDIM,'GE',0.5)
      ELSEIF(FNACNA(1:8).NE.'        ') THEN
        CALL ROF01(ACNANL,IJDIM,'GE',AISLIM)
        AISANL=ACNANL
      ENDIF
      CALL QCSICE(AISANL,GLACIR,AMXICE,AICICE,AICSEA,SLLND,SLMASK,
     1            RLA,RLO,IDIM,JDIM)
C
C  Set ocean/land/sea-ice mask 
C
      CALL SETLSI(SLMASK,AISANL,IJDIM,AICICE,SLIANL)
C     WRITE(6,*) 'SLIANL'
C     CALL NNTPRT(SLIANL,IDIM,JDIM)
C
C  Set albedo over ocean to ALBOMX
C
      CALL ALBOCN(ALBANL,SLMASK,ALBOMX,IJDIM)
C
C  Quality control of snow and sea-ice
C    Process snow depth or snow cover
C
      IF(FNSNOA(1:8).NE.'        ') THEN
        CALL SETZRO(SNOANL,EPSSNO,IJDIM)
        CALL QCSNOW(SNOANL,SLMASK,AISANL,GLACIR,IJDIM,10.)
        CALL SNOSFC(SNOANL,TSFANL,TSFSMX,IJDIM)
        CALL QCMXMN('Snoa',SNOANL,SLIANL,SNOANL,
     1              SNOLMX,SNOLMN,SNOOMX,SNOOMN,SNOIMX,SNOIMN,
     2              SNOJMX,SNOJMN,SNOSMX,SNOSMN,EPSSNO,
     3              RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
        CALL GETSCV(SNOANL,SCVANL,IJDIM)
        CALL QCMXMN('Sncva',SCVANL,SLIANL,SNOANL,
     1              SCVLMX,SCVLMN,SCVOMX,SCVOMN,SCVIMX,SCVIMN,
     2              SCVJMX,SCVJMN,SCVSMX,SCVSMN,EPSSCV,
     3              RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
      ELSE
        CALL ROF01(SCVANL,IJDIM,'GE',0.5)
        CALL QCSNOW(SCVANL,SLMASK,AISANL,GLACIR,IJDIM,1.)
        CALL QCMXMN('SNcva',SCVANL,SLIANL,SCVANL,
     1              SCVLMX,SCVLMN,SCVOMX,SCVOMN,SCVIMX,SCVIMN,
     2              SCVJMX,SCVJMN,SCVSMX,SCVSMN,EPSSCV,
     3              RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
        CALL SNODPTH(SCVANL,SLIANL,TSFANL,SNOCLM,
     1               GLACIR,SNWMAX,SNWMIN,IJDIM,SNOANL)
        CALL QCSNOW(SCVANL,SLMASK,AISANL,GLACIR,IJDIM,SNOSMX)
        CALL SNOSFC(SNOANL,TSFANL,TSFSMX,IJDIM)
        CALL QCMXMN('SNowa',SNOANL,SLIANL,SNOANL,
     1              SNOLMX,SNOLMN,SNOOMX,SNOOMN,SNOIMX,SNOIMN,
     2              SNOJMX,SNOJMN,SNOSMX,SNOSMN,EPSSNO,
     3              RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
      ENDIF
C
      CALL QCMXMN('TSFa',TSFANL,SLIANL,SNOANL,
     1            TSFLMX,TSFLMN,TSFOMX,TSFOMN,TSFIMX,TSFIMN,
     2            TSFJMX,TSFJMN,TSFSMX,TSFSMN,EPSTSF,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
      CALL QCMXMN('ALBa',ALBANL,SLIANL,SNOANL,
     1            ALBLMX,ALBLMN,ALBOMX,ALBOMN,ALBIMX,ALBIMN,
     2            ALBJMX,ALBJMN,ALBSMX,ALBSMN,EPSALB,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
      CALL QCMXMN('WETa',WETANL,SLIANL,SNOANL,
     1            WETLMX,WETLMN,WETOMX,WETOMN,WETIMX,WETIMN,
     2            WETJMX,WETJMN,WETSMX,WETSMN,EPSWET,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
      CALL QCMXMN('ZORa',ZORANL,SLIANL,SNOANL,
     1            ZORLMX,ZORLMN,ZOROMX,ZOROMN,ZORIMX,ZORIMN,
     2            ZORJMX,ZORJMN,ZORSMX,ZORSMN,EPSZOR,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
      CALL QCMXMN('PLNa',PLRANL,SLIANL,SNOANL,
     1            PLRLMX,PLRLMN,PLROMX,PLROMN,PLRIMX,PLRIMN,
     2            PLRJMX,PLRJMN,PLRSMX,PLRSMN,EPSPLR,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
      CALL QCMXMN('TG3a',TG3ANL,SLIANL,SNOANL,
     1            TG3LMX,TG3LMN,TG3OMX,TG3OMN,TG3IMX,TG3IMN,
     2            TG3JMX,TG3JMN,TG3SMX,TG3SMN,EPSTG3,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
C
C  Get soil temp and moisture
C
      IF(FNSMCA(1:8).EQ.'        ') THEN
        CALL GETSMC(WETANL,IJDIM,LSOIL,SMCANL)
      ENDIF
      CALL QCMXMN('SMC1a',SMCANL(1,1),SLIANL,SNOANL,
     1            SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     2            SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
      CALL QCMXMN('SMC2a',SMCANL(1,2),SLIANL,SNOANL,
     1            SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     2            SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
      IF(FNSTCA(1:8).EQ.'        ') THEN
        CALL GETSTC(TSFANL,TG3ANL,SLIANL,IJDIM,LSOIL,STCANL,TSFIMX)
      ENDIF
      CALL QCMXMN('STC1a',STCANL(1,1),SLIANL,SNOANL,
     1            STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     2            STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
      CALL QCMXMN('STC2a',STCANL(1,2),SLIANL,SNOANL,
     1            STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     2            STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     3            RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
C
C  MONITORING PRINTS
C
      PRINT *,' '
      PRINT *,'MONITOR OF TIME AND SPACE INTERPOLATED ANALYSIS'
      PRINT *,' '
      CALL COUNT(SLIANL,SNOANL,IJDIM)
      PRINT *,' '
      CALL MONITR('TSFANL',TSFANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('ALBANL',ALBANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('AISANL',AISANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('SNOANL',SNOANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('SCVANL',SCVANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('SMCANL1',SMCANL(1,1),SLIANL,SNOANL,IJDIM)
      CALL MONITR('SMCANL2',SMCANL(1,2),SLIANL,SNOANL,IJDIM)
      CALL MONITR('STCANL1',STCANL(1,1),SLIANL,SNOANL,IJDIM)
      CALL MONITR('STCANL2',STCANL(1,2),SLIANL,SNOANL,IJDIM)
      CALL MONITR('TG3ANL',TG3ANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('ZORANL',ZORANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('CVAANL',CVANL ,SLIANL,SNOANL,IJDIM)
      CALL MONITR('CVBANL',CVBANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('CVTANL',CVTANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('SLIANL',SLIANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('PLRANL',PLRANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('OROG  ',OROG  ,SLIANL,SNOANL,IJDIM)
C
      CALL ZNLSFC(SLIANL,IDIM,JDIM,IJDIM,LSOIL,NRCZNL,
     1            TSFANL,WETANL,SNOANL,STCANL,TG3ANL,
     2            ZORANL,PLRANL,CVANL,CVBANL,CVTANL,
     3            ALBANL,AISANL,SMCANL,CNPANL,
     4            ZNLSL,NZL1,NZL2,LSMSK,WEIS,GAUL)
      CALL ZNLODY(FH,ZNLSL,NZL1,NZL2,LSMSK,WEIS,
     1            IDIM,JDIM,IDIMT,NRCZNL)
C
C  Read in forecast fields
C
      WRITE(6,*) '=============='
      WRITE(6,*) '  FCST GUESS'
      WRITE(6,*) '=============='
C
      PERCRIT=CRITP3
C
C  Fill in guess array with Analysis if dead start.
C
      IF(FNBGSI(1:8).EQ.'        ') THEN
        WRITE(6,*) 'THIS RUN IS DEAD START RUN'
        WRITE(6,*) 'THIS RUN IS DEAD START RUN'
        WRITE(6,*) 'THIS RUN IS DEAD START RUN'
        CALL FILFCS(TSFFCS,WETFCS,SNOFCS,ZORFCS,ALBFCS,
     1              TG3FCS,PLRFCS,CVFCS ,CVBFCS,CVTFCS,
     2              CNPFCS,SMCFCS,STCFCS,SLIFCS,AISFCS,
     3              TSFANL,WETANL,SNOANL,ZORANL,ALBANL,
     4              TG3ANL,PLRANL,CVANL ,CVBANL,CVTANL,
     5              CNPANL,SMCANL,STCANL,SLIANL,AISANL,
     6              IJDIM,LSOIL)
        IF(SIG1T(1).NE.0.) THEN
          CALL USESGT(SIG1T,SLIANL,TG3ANL,IJDIM,LSOIL,TSFFCS,STCFCS,
     1                TSFIMX)
          CALL QCMXMN('TSFf ',TSFFCS,SLIFCS,SNOFCS,
     1                TSFLMX,TSFLMN,TSFOMX,TSFOMN,TSFIMX,TSFIMN,
     2                TSFJMX,TSFJMN,TSFSMX,TSFSMN,EPSTSF,
     3                RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
          CALL QCMXMN('STC1f',STCFCS(1,1),SLIFCS,SNOFCS,
     1                STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     2                STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     3                RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
          CALL QCMXMN('STC2f',STCFCS(1,2),SLIFCS,SNOFCS,
     1                STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     2                STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     3                RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
        ENDIF
      ELSE
        PERCRIT=CRITP2
        CALL BGSRD(LUGI,FNBGSI,IDIM,JDIM,IJDIM,LSOIL,
     1             TSFFCS,WETFCS,SNOFCS,ZORFCS,ALBFCS,AISFCS,
     2             TG3FCS,PLRFCS,CVFCS ,CVBFCS,CVTFCS,
     3             CNPFCS,SMCFCS,STCFCS,SLIFCS,F10M,LABEL)
C
C  Make reverse angulation correction to TSF
C  Make reverse orography correction to TG3
C
        CALL TSFCOR(TG3FCS,OROG,SLMASK,1.,IJDIM,-RLAPSE)
        CALL TSFCOR(TSFFCS,OROG,SLMASK,0.,IJDIM,-RLAPSE)
C
        IF(LQCBGS) THEN
          CALL QCSLI(SLIANL,SLIFCS,IJDIM)
          CALL ALBOCN(ALBFCS,SLMASK,ALBOMX,IJDIM)
          CALL QCMXMN('Snof',SNOFCS,SLIFCS,SNOFCS,
     1                SNOLMX,SNOLMN,SNOOMX,SNOOMN,SNOIMX,SNOIMN,
     2                SNOJMX,SNOJMN,SNOSMX,SNOSMN,EPSSNO,
     3                RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
          CALL QCMXMN('TSFf',TSFFCS,SLIFCS,SNOFCS,
     1                TSFLMX,TSFLMN,TSFOMX,TSFOMN,TSFIMX,TSFIMN,
     2                TSFJMX,TSFJMN,TSFSMX,TSFSMN,EPSTSF,
     3                RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
          CALL QCMXMN('ALBf',ALBFCS,SLIFCS,SNOFCS,
     1                ALBLMX,ALBLMN,ALBOMX,ALBOMN,ALBIMX,ALBIMN,
     2                ALBJMX,ALBJMN,ALBSMX,ALBSMN,EPSALB,
     3                RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
          CALL QCMXMN('WETf',WETFCS,SLIFCS,SNOFCS,
     1                WETLMX,WETLMN,WETOMX,WETOMN,WETIMX,WETIMN,
     2                WETJMX,WETJMN,WETSMX,WETSMN,EPSWET,
     3                RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
          CALL QCMXMN('ZORf',ZORFCS,SLIFCS,SNOFCS,
     1                ZORLMX,ZORLMN,ZOROMX,ZOROMN,ZORIMX,ZORIMN,
     2                ZORJMX,ZORJMN,ZORSMX,ZORSMN,EPSZOR,
     3                RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
          CALL QCMXMN('PLNf',PLRFCS,SLIFCS,SNOFCS,
     1                PLRLMX,PLRLMN,PLROMX,PLROMN,PLRIMX,PLRIMN,
     2                PLRJMX,PLRJMN,PLRSMX,PLRSMN,EPSPLR,
     3                RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
          CALL QCMXMN('TG3f',TG3FCS,SLIFCS,SNOFCS,
     1                TG3LMX,TG3LMN,TG3OMX,TG3OMN,TG3IMX,TG3IMN,
     2                TG3JMX,TG3JMN,TG3SMX,TG3SMN,EPSTG3,
     3                RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
          CALL QCMXMN('SMC1f',SMCFCS(1,1),SLIFCS,SNOFCS,
     1                SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     2                SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     3                RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
          CALL QCMXMN('SMC2f',SMCFCS(1,2),SLIFCS,SNOFCS,
     1                SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     2                SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     3                RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
          CALL QCMXMN('STC1f',STCFCS(1,1),SLIFCS,SNOFCS,
     1                STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     2                STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     3                RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
          CALL QCMXMN('STC2f',STCFCS(1,2),SLIFCS,SNOFCS,
     1                STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     2                STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     3                RLA,RLO,IJDIM,1,PERCRIT,LGCHEK)
        ENDIF
      ENDIF
C
      PRINT *,' '
      PRINT *,'MONITOR OF GUESS'
      PRINT *,' '
      CALL COUNT(SLIFCS,SNOFCS,IJDIM)
      PRINT *,' '
      CALL MONITR('TSFFCS',TSFFCS,SLIFCS,SNOFCS,IJDIM)
      CALL MONITR('ALBFCS',ALBFCS,SLIFCS,SNOFCS,IJDIM)
      CALL MONITR('AISFCS',AISFCS,SLIFCS,SNOFCS,IJDIM)
      CALL MONITR('SNOFCS',SNOFCS,SLIFCS,SNOFCS,IJDIM)
      CALL MONITR('SMCFCS1',SMCFCS(1,1),SLIFCS,SNOFCS,IJDIM)
      CALL MONITR('SMCFCS2',SMCFCS(1,2),SLIFCS,SNOFCS,IJDIM)
      CALL MONITR('STCFCS1',STCFCS(1,1),SLIFCS,SNOFCS,IJDIM)
      CALL MONITR('STCFCS2',STCFCS(1,2),SLIFCS,SNOFCS,IJDIM)
      CALL MONITR('TG3FCS',TG3FCS,SLIFCS,SNOFCS,IJDIM)
      CALL MONITR('ZORFCS',ZORFCS,SLIFCS,SNOFCS,IJDIM)
      CALL MONITR('CVAFCS',CVFCS ,SLIFCS,SNOFCS,IJDIM)
      CALL MONITR('CVBFCS',CVBFCS,SLIFCS,SNOFCS,IJDIM)
      CALL MONITR('CVTFCS',CVTFCS,SLIFCS,SNOFCS,IJDIM)
      CALL MONITR('SLIFCS',SLIFCS,SLIFCS,SNOFCS,IJDIM)
      CALL MONITR('PLRFCS',PLRFCS,SLIFCS,SNOFCS,IJDIM)
      CALL MONITR('OROG  ',OROG  ,SLIFCS,SNOFCS,IJDIM)
C
      CALL ZNLSFC(SLIFCS,IDIM,JDIM,IJDIM,LSOIL,NRCZNL,
     1            TSFFCS,WETFCS,SNOFCS,STCFCS,TG3FCS,
     2            ZORFCS,PLRFCS,CVFCS,CVBFCS,CVTFCS,
     3            ALBFCS,AISFCS,SMCFCS,CNPFCS,
     4            ZNLSL,NZL1,NZL2,LSMSK,WEIS,GAUL)
      CALL ZNLODY(FH,ZNLSL,NZL1,NZL2,LSMSK,WEIS,
     1            IDIM,JDIM,IDIMT,NRCZNL)
C
C  Quality control analysis using forecast guess
C
      CALL QCBYFC(TSFFCS,SNOFCS,QCTSFS,QCSNOS,QCTSFI,IJDIM,LSOIL,
     1            SNOANL,AISANL,SLIANL,TSFANL,ALBANL,
     2            ZORANL,PLRANL,SMCANL,
     3            PLRCLM,SMCCLM,TSFSMX,ALBOMX,ZOROMX)
C
C  Debugging only
C
      IF(LDEBUG) THEN
        CALL BGSWRT(LUGI,FNDANL,LABEL,FH,IY,IM,ID,IH,
     1              TSFANL,SMCANL,SNOANL,STCANL,TG3ANL,ZORANL,
     2              CVANL,CVBANL,CVTANL,ALBANL,SLIANL,PLRANL,
     3              F10M,CNPANL,IJDIM,LSOIL)
      ENDIF
C
C  BLEND CLIMATOLOGY AND PREDICTED FIELDS
C
      WRITE(6,*) '=============='
      WRITE(6,*) '   MERGING'
      WRITE(6,*) '=============='
C
      PERCRIT=CRITP3
C
C  Merge analysis and forecast.  Note TG3, AIS are not merged
C
      CALL MERGE(IJDIM,LSOIL,IY,IM,ID,IH,FH,
     Z           TSFFCS,WETFCS,SNOFCS,ZORFCS,ALBFCS,
     1           PLRFCS,CVFCS ,CVBFCS,CVTFCS,
     2           CNPFCS,SMCFCS,STCFCS,SLIFCS,
     3           TSFANL,WETANL,SNOANL,ZORANL,ALBANL,
     4           PLRANL,CVANL ,CVBANL,CVTANL,
     5           CNPANL,SMCANL,STCANL,SLIANL,
     6           CTSFL,CALBL,CSNOL,CSMCL,CZORL,CPLRL,
     7           CTSFS,CALBS,CSNOS,CSMCS,CZORS,CPLRS,
     8           CCV,CCVB,CCVT,CCNP,
     7           IRTTSF,IRTWET,IRTSNO,IRTZOR,IRTALB,IRTAIS,
     8           IRTTG3,IRTPLR,IRTSCV,IRTACN,IRTSMC,IRTSTC)
      CALL SETZRO(SNOANL,EPSSNO,IJDIM)
C
C  New ice/Melted ice
C
      CALL NEWICE(SLIANL,SLIFCS,TSFANL,TSFFCS,IJDIM,LSOIL,
     1            ALBANL,SNOANL,ZORANL,PLRANL,SMCANL,STCANL,
     2            ALBOMX,SNOOMX,ZOROMX,SMCOMX,SMCIMX,PLROMX,
     3            TSFOMN,TSFIMX,ALBIMX,ZORIMX,PLRIMX,TGICE,
     4            RLA,RLO)
C
C  Set tsfc to TSNOW over snow
C
      CALL SNOSFC(SNOANL,TSFANL,TSFSMX,IJDIM)
C
      CALL QCMXMN('SnowM',SNOANL,SLIANL,SNOANL,
     1            SNOLMX,SNOLMN,SNOOMX,SNOOMN,SNOIMX,SNOIMN,
     2            SNOJMX,SNOJMN,SNOSMX,SNOSMN,EPSSNO,
     3            RLA,RLO,IJDIM,0,PERCRIT,LGCHEK)
      CALL QCMXMN('TsfM ',TSFANL,SLIANL,SNOANL,
     1            TSFLMX,TSFLMN,TSFOMX,TSFOMN,TSFIMX,TSFIMN,
     2            TSFJMX,TSFJMN,TSFSMX,TSFSMN,EPSTSF,
     3            RLA,RLO,IJDIM,0,PERCRIT,LGCHEK)
      CALL QCMXMN('AlbM ',ALBANL,SLIANL,SNOANL,
     1            ALBLMX,ALBLMN,ALBOMX,ALBOMN,ALBIMX,ALBIMN,
     2            ALBJMX,ALBJMN,ALBSMX,ALBSMN,EPSALB,
     3            RLA,RLO,IJDIM,0,PERCRIT,LGCHEK)
      CALL QCMXMN('WetM ',WETANL,SLIANL,SNOANL,
     1            WETLMX,WETLMN,WETOMX,WETOMN,WETIMX,WETIMN,
     2            WETJMX,WETJMN,WETSMX,WETSMN,EPSWET,
     3            RLA,RLO,IJDIM,0,PERCRIT,LGCHEK)
      CALL QCMXMN('ZorM ',ZORANL,SLIANL,SNOANL,
     1            ZORLMX,ZORLMN,ZOROMX,ZOROMN,ZORIMX,ZORIMN,
     2            ZORJMX,ZORJMN,ZORSMX,ZORSMN,EPSZOR,
     3            RLA,RLO,IJDIM,0,PERCRIT,LGCHEK)
      CALL QCMXMN('PlntM',PLRANL,SLIANL,SNOANL,
     1            PLRLMX,PLRLMN,PLROMX,PLROMN,PLRIMX,PLRIMN,
     2            PLRJMX,PLRJMN,PLRSMX,PLRSMN,EPSPLR,
     3            RLA,RLO,IJDIM,0,PERCRIT,LGCHEK)
      CALL QCMXMN('Stc1M',STCANL(1,1),SLIANL,SNOANL,
     1            STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     2            STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     3            RLA,RLO,IJDIM,0,PERCRIT,LGCHEK)
      CALL QCMXMN('Stc2M',STCANL(1,2),SLIANL,SNOANL,
     1            STCLMX,STCLMN,STCOMX,STCOMN,STCIMX,STCIMN,
     2            STCJMX,STCJMN,STCSMX,STCSMN,EPTSFC,
     3            RLA,RLO,IJDIM,0,PERCRIT,LGCHEK)
      CALL QCMXMN('Smc1M',SMCANL(1,1),SLIANL,SNOANL,
     1            SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     2            SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     3            RLA,RLO,IJDIM,0,PERCRIT,LGCHEK)
      CALL QCMXMN('Smc2M',SMCANL(1,2),SLIANL,SNOANL,
     1            SMCLMX,SMCLMN,SMCOMX,SMCOMN,SMCIMX,SMCIMN,
     2            SMCJMX,SMCJMN,SMCSMX,SMCSMN,EPSSMC,
     3            RLA,RLO,IJDIM,0,PERCRIT,LGCHEK)
C
      WRITE(6,*) '=============='
      WRITE(6,*) 'FINAL RESULTS'
      WRITE(6,*) '=============='
C
C  Foreward correction to TG3 and TSF at the last stage
C
      CALL TSFCOR(TG3ANL,OROG,SLMASK,1.,IJDIM,RLAPSE)
      CALL TSFCOR(TSFANL,OROG,SLMASK,0.,IJDIM,RLAPSE)
C
C  CHECK THE FINAL MERGED PRODUCT
C
      PRINT *,' '
      PRINT *,'MONITOR OF UPDATED SURFACE FIELDS'
      PRINT *,'   (Includes angulation correction)'
      PRINT *,' '
      CALL COUNT(SLIANL,SNOANL,IJDIM)
      PRINT *,' '
      CALL MONITR('TSFANL',TSFANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('ALBANL',ALBANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('AISANL',AISANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('SNOANL',SNOANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('SMCANL1',SMCANL(1,1),SLIANL,SNOANL,IJDIM)
      CALL MONITR('SMCANL2',SMCANL(1,2),SLIANL,SNOANL,IJDIM)
      CALL MONITR('STCANL1',STCANL(1,1),SLIANL,SNOANL,IJDIM)
      CALL MONITR('STCANL2',STCANL(1,2),SLIANL,SNOANL,IJDIM)
      CALL MONITR('TG3ANL',TG3ANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('ZORANL',ZORANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('CVAANL',CVANL ,SLIANL,SNOANL,IJDIM)
      CALL MONITR('CVBANL',CVBANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('CVTANL',CVTANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('SLIANL',SLIANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('PLRANL',PLRANL,SLIANL,SNOANL,IJDIM)
      CALL MONITR('OROG  ',OROG  ,SLIANL,SNOANL,IJDIM)
C
      DO IJ=1,IJDIM
        TSFFCS(IJ)=TSFANL(IJ)-TSFFCS(IJ)
        SNOFCS(IJ)=SNOANL(IJ)-SNOFCS(IJ)
        TG3FCS(IJ)=TG3ANL(IJ)-TG3FCS(IJ)
        ZORFCS(IJ)=ZORANL(IJ)-ZORFCS(IJ)
        PLRFCS(IJ)=PLRANL(IJ)-PLRFCS(IJ)
        ALBFCS(IJ)=ALBANL(IJ)-ALBFCS(IJ)
        SLIFCS(IJ)=SLIANL(IJ)-SLIFCS(IJ)
        AISFCS(IJ)=AISANL(IJ)-AISFCS(IJ)
      ENDDO
      DO K = 1, LSOIL
        DO IJ = 1,IJDIM
          SMCFCS(IJ,K) = SMCANL(IJ,K) - SMCFCS(IJ,K)
          STCFCS(IJ,K) = STCANL(IJ,K) - STCFCS(IJ,K)
        ENDDO
      ENDDO
C
C  MONITORING PRINTS
C
      PRINT *,' '
      PRINT *,'MONITOR OF DIFFERENCE'
      PRINT *,'   (Includes angulation correction)'
      PRINT *,' '
      CALL MONITR('TSFDIF',TSFFCS,SLIANL,SNOANL,IJDIM)
      CALL MONITR('ALBDIF',ALBFCS,SLIANL,SNOANL,IJDIM)
      CALL MONITR('AISDIF',AISFCS,SLIANL,SNOANL,IJDIM)
      CALL MONITR('SNODIF',SNOFCS,SLIANL,SNOANL,IJDIM)
      CALL MONITR('SMCANL1',SMCFCS(1,1),SLIANL,SNOANL,IJDIM)
      CALL MONITR('SMCANL2',SMCFCS(1,2),SLIANL,SNOANL,IJDIM)
      CALL MONITR('STCANL1',STCFCS(1,1),SLIANL,SNOANL,IJDIM)
      CALL MONITR('STCANL2',STCFCS(1,2),SLIANL,SNOANL,IJDIM)
      CALL MONITR('TG3DIF',TG3FCS,SLIANL,SNOANL,IJDIM)
      CALL MONITR('ZORDIF',ZORFCS,SLIANL,SNOANL,IJDIM)
      CALL MONITR('CVADIF',CVFCS ,SLIANL,SNOANL,IJDIM)
      CALL MONITR('CVBDIF',CVBFCS,SLIANL,SNOANL,IJDIM)
      CALL MONITR('CVTDIF',CVTFCS,SLIANL,SNOANL,IJDIM)
      CALL MONITR('SLIDIF',SLIFCS,SLIANL,SNOANL,IJDIM)
      CALL MONITR('PLRDIF',PLRFCS,SLIANL,SNOANL,IJDIM)
C
      CALL ZNLSFC(SLIANL,IDIM,JDIM,IJDIM,LSOIL,NRCZNL,
     1            TSFANL,WETANL,SNOANL,STCANL,TG3ANL,
     2            ZORANL,PLRANL,CVANL,CVBANL,CVTANL,
     3            ALBANL,AISANL,SMCANL,CNPANL,
     4            ZNLSL,NZL1,NZL2,LSMSK,WEIS,GAUL)
      CALL ZNLODY(FH,ZNLSL,NZL1,NZL2,LSMSK,WEIS,
     1            IDIM,JDIM,IDIMT,NRCZNL)
C
	write(*,*) '>>BGSWR1'
      CALL BGSWRT(LUGI,FNBGSO,LABEL,FH,IY,IM,ID,IH,
     1            TSFANL,SMCANL,SNOANL,STCANL,TG3ANL,ZORANL,
     2            CVANL,CVBANL,CVTANL,ALBANL,SLIANL,PLRANL,
     3            F10M,CNPANL,IJDIM,LSOIL)
C
	write(*,*) '<<BGSWR1'
      RETURN
      END
      SUBROUTINE BGSWRT(LUGI,FNBGSO,LABEL,FH,IY,IM,ID,IH,
     1                  TSFANL,SMCANL,SNOANL,STCANL,TG3ANL,ZORANL,
     2                  CVANL,CVBANL,CVTANL,ALBANL,SLIANL,PLRANL,
     3                  F10M,CNPANL,IJDIM,LSOIL)
C
	  common /y2k/ idat,jdat,rinc
	  integer idat(8),jdat(8)
	  real    rinc(5)
          integer*4 idate4(4)

      DIMENSION TSFANL(IJDIM),SNOANL(IJDIM),
     1          ZORANL(IJDIM),ALBANL(IJDIM),AISANL(IJDIM),
     2          TG3ANL(IJDIM),PLRANL(IJDIM),
     3          CVANL (IJDIM),CVBANL(IJDIM),CVTANL(IJDIM),
     4          CNPANL(IJDIM),
     5          SMCANL(IJDIM,LSOIL),STCANL(IJDIM,LSOIL),
     6          SLIANL(IJDIM)
C
      CHARACTER*32 LABEL
C
      DIMENSION F10M(IJDIM)
C
      CHARACTER*80 FNBGSO
C
c     CHARACTER*80 ASGNSTR
C
c     IF(LUGI.LT.10) THEN
c       WRITE(ASGNSTR,'(29Hassign -Fcos -Cascii -Nibm u:,I1,)') LUGI
c     ELSE
c       WRITE(ASGNSTR,'(29Hassign -Fcos -Cascii -Nibm u:,I2,)') LUGI
c     ENDIF
c     CALL ASSIGN(ASGNSTR)

      OPEN(UNIT=LUGI,FILE=FNBGSO,FORM='UNFORMATTED',ERR=900)
      GO TO 901
  900   CONTINUE
        WRITE(6,*) ' ERROR IN OPENING FILE ',FNBGSO(1:50)
        CALL ABORT
  901 CONTINUE
      WRITE(6,*) ' FILE ',FNBGSO(1:50),' opened. Unit=',LUGI
C
      WRITE(LUGI) LABEL
c     WRITE(LUGI) FH,IH,IM,ID,IY
      print*,'old date label',FH,IH,IM,ID,IY
      print*,'new date label',FH,jdat(5),jdat(2),jdat(3),jdat(1)
c     WRITE(LUGI) FH,jdat(5),jdat(2),jdat(3),jdat(1)
      idate4(1) = jdat(5)
      idate4(2) = jdat(2)
      idate4(3) = jdat(3)
      idate4(4) = jdat(1)
      WRITE(LUGI) FH,idate4

      WRITE(LUGI) TSFANL
      WRITE(LUGI) SMCANL
      WRITE(LUGI) SNOANL
      WRITE(LUGI) STCANL
      WRITE(LUGI) TG3ANL
      WRITE(LUGI) ZORANL
      WRITE(LUGI) CVANL
      WRITE(LUGI) CVBANL
      WRITE(LUGI) CVTANL
      WRITE(LUGI) ALBANL
      WRITE(LUGI) SLIANL
      WRITE(LUGI) PLRANL
      WRITE(LUGI) CNPANL
      WRITE(LUGI) F10M
	write(*,*) 'finished writing'
C
      RETURN
      END
      SUBROUTINE GAULAT(GAUL,K)                                         
C                                                                       
      DIMENSION A(500)                                                  
      DIMENSION GAUL(1)                                                 
C                                                                       
      ESP=1.E-14                                                        
      C=(1.E0-(2.E0/3.14159265358979E0)**2)*0.25E0                      
      FK=K                                                              
      KK=K/2                                                            
      CALL BSSLZ1(A,KK)                                                 
      DO 30 IS=1,KK                                                     
      XZ=COS(A(IS)/SQRT((FK+0.5E0)**2+C))                               
      ITER=0                                                            
   10 PKM2=1.E0                                                         
      PKM1=XZ                                                           
      ITER=ITER+1                                                       
      IF(ITER.GT.10) GO TO 70                                           
      DO 20 N=2,K                                                       
      FN=N                                                              
      PK=((2.E0*FN-1.E0)*XZ*PKM1-(FN-1.E0)*PKM2)/FN                     
      PKM2=PKM1                                                         
   20 PKM1=PK                                                           
      PKM1=PKM2                                                         
      PKMRK=(FK*(PKM1-XZ*PK))/(1.E0-XZ**2)                              
      SP=PK/PKMRK                                                       
      XZ=XZ-SP                                                          
      AVSP=ABS(SP)                                                      
      IF(AVSP.GT.ESP) GO TO 10                                          
      A(IS)=XZ                                                          
   30 CONTINUE                                                          
      IF(K.EQ.KK*2) GO TO 50                                            
      A(KK+1)=0.E0                                                      
      PK=2.E0/FK**2                                                     
      DO 40 N=2,K,2                                                     
      FN=N                                                              
   40 PK=PK*FN**2/(FN-1.E0)**2                                          
   50 CONTINUE                                                          
      DO 60 N=1,KK                                                      
      L=K+1-N                                                           
      A(L)=-A(N)                                                        
   60 CONTINUE                                                          
C                                                                       
      RADI=180./(4.*ATAN(1.))                                           
      DO 211 N=1,K                                                      
      GAUL(N)=ACOS(A(N))*RADI                                           
  211 CONTINUE                                                          
C
C     PRINT *,'GAUSSIAN LAT (DEG) FOR JMAX=',K 
C     PRINT *,(GAUL(N),N=1,K)                                           
C                                                                       
      RETURN                                                            
   70 WRITE(6,6000)                                                     
 6000 FORMAT(//5X,14HERROR IN GAUAW//)                                  
      STOP                                                              
      END                                                               
      SUBROUTINE BSSLZ1(BES,N)
C
      DIMENSION BES(N)
      DIMENSION BZ(50)
C
      DATA PI/3.14159265358979E0/
      DATA BZ         / 2.4048255577E0, 5.5200781103E0,
     $  8.6537279129E0,11.7915344391E0,14.9309177086E0,18.0710639679E0,
     $ 21.2116366299E0,24.3524715308E0,27.4934791320E0,30.6346064684E0,
     $ 33.7758202136E0,36.9170983537E0,40.0584257646E0,43.1997917132E0,
     $ 46.3411883717E0,49.4826098974E0,52.6240518411E0,55.7655107550E0,
     $ 58.9069839261E0,62.0484691902E0,65.1899648002E0,68.3314693299E0,
     $ 71.4729816036E0,74.6145006437E0,77.7560256304E0,80.8975558711E0,
     $ 84.0390907769E0,87.1806298436E0,90.3221726372E0,93.4637187819E0,
     $ 96.6052679510E0,99.7468198587E0,102.888374254E0,106.029930916E0,
     $ 109.171489649E0,112.313050280E0,115.454612653E0,118.596176630E0,
     $ 121.737742088E0,124.879308913E0,128.020877005E0,131.162446275E0,
     $ 134.304016638E0,137.445588020E0,140.587160352E0,143.728733573E0,
     $ 146.870307625E0,150.011882457E0,153.153458019E0,156.295034268E0/
      NN=N
      IF(N.LE.50) GO TO 12
      BES(50)=BZ(50)
      DO 5 J=51,N
    5 BES(J)=BES(J-1)+PI
      NN=49
   12 DO 15 J=1,NN
   15 BES(J)=BZ(J)
      RETURN
      END
      SUBROUTINE COUNT(SLIMSK,SNO,IJMAX)
C
      DIMENSION SLIMSK(1),SNO(1)
C
C  COUNT NUMBER OF POINTS FOR THE FOUR SURFACE CONDITIONS
C
      L0=0
      L1=0
      L2=0
      L3=0
      L4=0
      DO 350 IJ=1,IJMAX
      IF(SLIMSK(IJ).EQ.0.) L1=L1+1
      IF(SLIMSK(IJ).EQ.1. .AND. SNO(IJ).LE.0.) L0=L0+1
      IF(SLIMSK(IJ).EQ.2. .AND. SNO(IJ).LE.0.) L2=L2+1
      IF(SLIMSK(IJ).EQ.1. .AND. SNO(IJ).GT.0.) L3=L3+1
      IF(SLIMSK(IJ).EQ.2. .AND. SNO(IJ).GT.0.) L4=L4+1
  350 CONTINUE
      L5=L0+L3
      L6=L2+L4
      L7=L1+L6
      L8=L1+L5+L6
      RL0=FLOAT(L0)/FLOAT(L8)*100.
      RL3=FLOAT(L3)/FLOAT(L8)*100.
      RL1=FLOAT(L1)/FLOAT(L8)*100.
      RL2=FLOAT(L2)/FLOAT(L8)*100.
      RL4=FLOAT(L4)/FLOAT(L8)*100.
      RL5=FLOAT(L5)/FLOAT(L8)*100.
      RL6=FLOAT(L6)/FLOAT(L8)*100.
      RL7=FLOAT(L7)/FLOAT(L8)*100.
      PRINT *,'1) NO. OF NOT SNOW-COVERED LAND POINTS   ',L0,' ',RL0,'%'
      PRINT *,'2) NO. OF SNOW COVERED LAND POINTS       ',L3,' ',RL3,'%'
      PRINT *,'3) NO. OF OPEN SEA POINTS                ',L1,' ',RL1,'%'
      PRINT *,'4) NO. OF NOT SNOW-COVERED SEAICE POINTS ',L2,' ',RL2,'%'
      PRINT *,'5) NO. OF SNOW COVERED SEA ICE POINTS    ',L4,' ',RL4,'%'
      PRINT *,' '
      PRINT *,'6) NO. OF LAND POINTS                    ',L5,' ',RL5,'%'
      PRINT *,'7) NO. SEA POINTS (INCLUDING SEA ICE)    ',L7,' ',RL7,'%'
      PRINT *,'   (NO. OF SEA ICE POINTS)          (',L6,')',' ',RL6,'%'
      PRINT *,' '
      PRINT *,'9) NO. OF TOTAL GRID POINTS               ',L8
      PRINT *,' '
      PRINT *,' '
C
      RETURN
      END
      SUBROUTINE MONITR(LFLD,FLD,SLIMSK,SNO,IJMAX)
C
      DIMENSION FLD(IJMAX)
      DIMENSION SLIMSK(IJMAX),SNO(IJMAX)
C
      DIMENSION RMAX(5),RMIN(5)
C
      CHARACTER*8 LFLD
C
C  FIND MAX/MIN
C
      DO 10 N=1,5
      RMAX(N)=-9.E20
      RMIN(N)= 9.E20
   10 CONTINUE
C
      DO IJ=1,IJMAX
        IF(SLIMSK(IJ).EQ.0.) THEN
          N=1
        ELSEIF(SLIMSK(IJ).EQ.1. .AND. SNO(IJ).LE.0.) THEN
          N=2
        ELSEIF(SLIMSK(IJ).EQ.2. .AND. SNO(IJ).LE.0.) THEN
          N=3
        ELSEIF(SLIMSK(IJ).EQ.1. .AND. SNO(IJ).GT.0.) THEN
          N=4
        ELSEIF(SLIMSK(IJ).EQ.2. .AND. SNO(IJ).GT.0.) THEN
          N=5
        ENDIF
        IF(FLD(IJ).GE.RMAX(N)) RMAX(N)=FLD(IJ)
        IF(FLD(IJ).LE.RMIN(N)) RMIN(N)=FLD(IJ)
      ENDDO
C
      PRINT 100,LFLD
      PRINT 101,RMAX(1),RMIN(1)
      PRINT 102,RMAX(2),RMIN(2)
      PRINT 103,RMAX(3),RMIN(3)
      PRINT 104,RMAX(4),RMIN(4)
      PRINT 105,RMAX(5),RMIN(5)
  100 FORMAT(1H0,2X,'*** ',A8,' ***')
  101 FORMAT(2X,' OPEN SEA  ............. MAX=',E12.4,' MIN=',E12.4)
  102 FORMAT(2X,' LAND WITHOUT SNOW ..... MAX=',E12.4,' MIN=',E12.4)
  103 FORMAT(2X,' SEAICE WITHOUT SNOW ... MAX=',E12.4,' MIN=',E12.4)
  104 FORMAT(2X,' LAND WITH SNOW ........ MAX=',E12.4,' MIN=',E12.4)
  105 FORMAT(2X,' SEA ICE WITH SNOW ..... MAX=',E12.4,' MIN=',E12.4)
C
      RETURN
      END
      SUBROUTINE DAYOYR(IYR,IMO,IDY,LDY)
C
C  THIS ROUTINE FIGURES OUT THE DAY OF THE YEAR GIVEN IMO AND IDY
C
      DIMENSION MONTH(13)
      DATA MONTH/0,31,28,31,30,31,30,31,31,30,31,30,31/
      IF(MOD(IYR,4).EQ.0) MONTH(3) = 29
      LDY = IDY
      DO 10 I = 1, IMO
      LDY = LDY + MONTH(I)
 10   CONTINUE
      RETURN
      END
      SUBROUTINE ZNLSFC(SLIMSK,IDIM,JDIM,IJDIM,LSOIL,NRCZNL,
     &                  TSFANL,WETANL,SNOANL,STCANL,TG3ANL,
     &                  ZORANL,PLRANL,CVANL,CVBANL,CVTANL,
     &                  ALBANL,AISANL,SMCANL,CNPANL,
     &                  ZNLSL,NZL1,NZL2,LSMSK,WEIS,GAUL)
C
      DIMENSION TSFANL(IDIM,JDIM),WETANL(IDIM,JDIM),SNOANL(IDIM,JDIM),
     &          STCANL(IDIM,JDIM,LSOIL),TG3ANL(IDIM,JDIM),
     &          ZORANL(IDIM,JDIM),PLRANL(IDIM,JDIM),
     &          CVANL (IDIM,JDIM),CVBANL(IDIM,JDIM),CVTANL(IDIM,JDIM),
     &          ALBANL(IDIM,JDIM),AISANL(IDIM,JDIM),
     &          SMCANL(IDIM,JDIM,LSOIL),CNPANL(IDIM,JDIM)
C
      LOGICAL LSMSK(IDIM,2,6)
      DIMENSION ZNLSL(6,6,NRCZNL),WEIS(6,6)
C
      DIMENSION GAUL(JDIM)
C
      DIMENSION WGT(JDIM)
      DIMENSION WORK1(IDIM,2), WORK2(IDIM,2)
C
      DIMENSION SLIMSK(IDIM,JDIM)
C
      IDIMT=IDIM*2
      JDIMHF=JDIM/2
C
      DO J=1,JDIM
        WGT(J)=COS((90.-GAUL(J))*0.01745329)
      ENDDO
C
C     LAT LOOP
C
      DO 1000 LAT = 1,JDIMHF
      LATCO = JDIM + 1 - LAT
C
C   ZONAL AVERAGE MONITORING
C
      DO 10 I = 1, IDIM
      WORK1(I,1) = SLIMSK(I,LAT  )
      WORK1(I,2) = SLIMSK(I,LATCO)
      WORK2(I,1) = SNOANL(I,LAT )
      WORK2(I,2) = SNOANL(I,LATCO)
 10   CONTINUE
      CALL ZNLWGT(WORK1,WORK2,WGT(LAT),LAT,
     1            ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
C
      DO 20 I = 1, IDIM
      WORK1(I,1) = TSFANL(I,LAT  )
      WORK1(I,2) = TSFANL(I,LATCO)
 20   CONTINUE
      CALL ZNLAVS(WORK1  ,LAT,WGT(LAT), 1,
     1            ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
      DO 30 I = 1, IDIM
      WORK1(I,1) = SMCANL(I,LAT,1)
      WORK1(I,2) = SMCANL(I,LATCO,1)
 30   CONTINUE
      CALL ZNLAVS(WORK1  ,LAT,WGT(LAT), 2,
     1            ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
      DO 35 I = 1, IDIM
      WORK1(I,1) = SMCANL(I,LAT,LSOIL)
      WORK1(I,2) = SMCANL(I,LATCO,LSOIL)
 35   CONTINUE
      CALL ZNLAVS(WORK1  ,LAT,WGT(LAT), 3,
     1            ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
      DO 40 I = 1, IDIM
      WORK1(I,1) = SNOANL(I,LAT       )
      WORK1(I,2) = SNOANL(I,LATCO)
 40   CONTINUE
      CALL ZNLAVS(WORK1  ,LAT,WGT(LAT), 4,
     1            ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
      DO 50 I = 1, IDIM
      WORK1(I,1) = STCANL(I,LAT       ,1)
      WORK1(I,2) = STCANL(I,LATCO,1)
 50   CONTINUE
      CALL ZNLAVS(WORK1  ,LAT,WGT(LAT), 5,
     1            ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
      DO 60 I = 1, IDIM
      WORK1(I,1) = STCANL(I,LAT       ,lsoil)
      WORK1(I,2) = STCANL(I,LATCO,lsoil)
 60   CONTINUE
      CALL ZNLAVS(WORK1  ,LAT,WGT(LAT), 6,
     1            ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
      DO 70 I = 1, IDIM
      WORK1(I,1) = ZORANL(I,LAT       )
      WORK1(I,2) = ZORANL(I,LATCO)
 70   CONTINUE
      CALL ZNLAVS(WORK1  ,LAT,WGT(LAT), 7,
     1            ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
      DO 80 I = 1, IDIM
      WORK1(I,1) =   CVANL(I,LAT  )
      WORK1(I,2) =   CVANL(I,LATCO)
 80   CONTINUE
      CALL ZNLAVS(WORK1  ,LAT,WGT(LAT), 8,
     1            ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
      DO 90 I = 1, IDIM
      WORK1(I,1) = CVBANL(I,LAT  )
      WORK1(I,2) = CVBANL(I,LATCO)
 90   CONTINUE
      CALL ZNLAVS(WORK1  ,LAT,WGT(LAT), 9,
     1            ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
      DO 100 I = 1, IDIM
      WORK1(I,1) = CVTANL(I,LAT  )
      WORK1(I,2) = CVTANL(I,LATCO)
 100  CONTINUE
      CALL ZNLAVS(WORK1  ,LAT,WGT(LAT),10,
     1            ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
      DO 110 I = 1, IDIM
      WORK1(I,1) = ALBANL(I,LAT  )
      WORK1(I,2) = ALBANL(I,LATCO)
 110  CONTINUE
      CALL ZNLAVS(WORK1  ,LAT,WGT(LAT),11,
     1            ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
      DO 120 I = 1, IDIM
      WORK1(I,1) = PLRANL(I,LAT  )
      WORK1(I,2) = PLRANL(I,LATCO)
 120  CONTINUE
      CALL ZNLAVS(WORK1  ,LAT,WGT(LAT),12,
     1            ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
      DO 130 I = 1, IDIM
      WORK1(I,1) = CNPANL(I,LAT  )
      WORK1(I,2) = CNPANL(I,LATCO)
 130  CONTINUE
      CALL ZNLAVS(WORK1  ,LAT,WGT(LAT),13,
     1            ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
C
1000   CONTINUE
C
      RETURN
      END
      SUBROUTINE ZNLWGT(SLMSK,SHELEG,WGT,LAT,
     1                ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
C
      LOGICAL LSMSK(IDIMT,6)
      DIMENSION ZNLSL(6,6,NRCZNL),WEIS(6,6)
C
      DIMENSION SLMSK(1),SHELEG(1)
C
      JDIMHF=JDIM/2
C
C  ZONAL AVERAGE WEIGHT
C
      NZL1=JDIM/6+1
      NZL1P=NZL1+1
      NZL2=JDIM/3+1
      NZL2P=NZL2+1
C
      N=IDIM+1
C
      IF(LAT.EQ.1) THEN
      DO 100 J = 1, 6
      DO 100 I = 1, 6
      WEIS(I,J) = 0.0
 100  CONTINUE
      ENDIF
C
C  BARE/SNOW SEA/LAND/ICE AVERAGES
C
      DO 110 I = 1, IDIMT
      LSMSK(I,2)=(SLMSK (I).EQ.1.).AND.
     1           (SHELEG(I).LE.1.E-3)
      LSMSK(I,3)=(SLMSK (I).EQ.1.).AND.
     1           (SHELEG(I).GT.1.E-3)
      LSMSK(I,4)=(SLMSK (I).EQ.2.).AND.
     1           (SHELEG(I).LE.1.E-3)
      LSMSK(I,5)=(SLMSK (I).EQ.2.).AND.
     1           (SHELEG(I).GT.1.E-3)
      LSMSK(I,6)= SLMSK (I).EQ.0.0
 110  CONTINUE
C
      IF(LAT.GE.1.AND.LAT.LE.NZL1) THEN
C
C  NORTHERN AND SOUTHERN POLAR REGION
C
      WEIS(2,1)=WEIS(2,1)+         FLOAT(IDIM)*WGT
      WEIS(6,1)=WEIS(6,1)+         FLOAT(IDIM)*WGT
      DO 10 L=2,6
      ISUM = 0
      JSUM = 0
      DO 120 I = 1, IDIM
      IF(LSMSK(I,L)) ISUM = ISUM + 1
      IF(LSMSK(I+IDIM,L)) JSUM = JSUM + 1
 120  CONTINUE
      WEIS(2,L) = WEIS(2,L) + ISUM * WGT
      WEIS(6,L) = WEIS(6,L) + JSUM * WGT
   10 CONTINUE
      ENDIF
C
C  NORTHERN AND SOUTHERN MIDDLE LATITUDES
C
      IF(LAT.GE.NZL1P.AND.LAT.LE.NZL2) THEN
      WEIS(3,1)=WEIS(3,1)+         FLOAT(IDIM)*WGT
      WEIS(5,1)=WEIS(5,1)+         FLOAT(IDIM)*WGT
      DO 20 L=2,6
      ISUM = 0
      JSUM = 0
      DO 130 I = 1, IDIM
      IF(LSMSK(I,L)) ISUM = ISUM + 1
      IF(LSMSK(I+IDIM,L)) JSUM = JSUM + 1
 130  CONTINUE
      WEIS(3,L) = WEIS(3,L) + ISUM * WGT
      WEIS(5,L) = WEIS(5,L) + JSUM * WGT
   20 CONTINUE
      ENDIF
C
      IF(LAT.GE.NZL2P) THEN
      WEIS(4,1)=WEIS(4,1)+         FLOAT(IDIMT)*WGT
      DO 30 L=2,6
      ISUM = 0
      DO 140 I = 1, IDIMT
      IF(LSMSK(I,L)) ISUM = ISUM + 1
 140  CONTINUE
      WEIS(4,L) = WEIS(4,L) + ISUM * WGT
   30 CONTINUE
      ENDIF
C
      IF(LAT.EQ.JDIMHF) THEN
      ZNLSL(1,1,NRCZNL) = 0.
      DO 38 J=2,6
      ZNLSL(1,1,NRCZNL)=ZNLSL(1,1,NRCZNL)+WEIS(J,1)
   38 CONTINUE
C*** NORMALIZE LATITUDE BAND WEIGHTS WITH GLOBAL SUM
      DO 39 J=2,6
      ZNLSL(J,1,NRCZNL)=WEIS(J,1)/ZNLSL(1,1,NRCZNL)
   39 CONTINUE
C
      DO 40 J=2,6
      DO 50 L=2,6
   50 CONTINUE
      IF(WEIS(J,1) .NE. 0.0) THEN
        DO 60 L=2,6
        ZNLSL(J,L,NRCZNL)=WEIS(J,L)/WEIS(J,1)
   60   CONTINUE
      ELSE
        ZNLSL(J,L,NRCZNL)=999.0
      END IF
   40 CONTINUE
      ZNLSL(1,1,NRCZNL)=1.0
C*** COMPUTE GLOBAL COVERAGES BY LATITUDE BAND WEIGHTING
      DO 42 L = 2,6
      ZNLSL(1,L,NRCZNL) = 0.0
      DO 42 J = 2,6
      ZNLSL(1,L,NRCZNL) = ZNLSL(1,L,NRCZNL)
     1                   + ZNLSL(J,L,NRCZNL)*ZNLSL(J,1,NRCZNL)
   42 CONTINUE
      DO 43 L = 1,6
      DO 43 J = 1,6
      ZNLSL(J,L,NRCZNL) = ZNLSL(J,L,NRCZNL)*100.0
   43 CONTINUE
      DO 70 L=1,6
      WEIS(1,L)=0.0
      DO 80 J=2,6
      WEIS(1,L)=WEIS(1,L)+WEIS(J,L)
   80 CONTINUE
      DO 90 J=1,6
      IF(WEIS(J,L).NE.0.0) WEIS(J,L)=1.0/WEIS(J,L)
   90 CONTINUE
   70 CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE ZNLAVS(F,LAT,WGT,IND,
     1                ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
C
C
      LOGICAL LSMSK(IDIMT,6)
      DIMENSION ZNLSL(6,6,NRCZNL),WEIS(6,6)
C
      DIMENSION F(1)
C
      IF(IND.GT.NRCZNL) RETURN
C
      JDIMHF=JDIM/2
C
      N=IDIM+1
C
      IF(LAT.EQ.1) THEN
      DO 100 J = 1, 6
      DO 100 I = 1, 6
      ZNLSL(I,J,IND) = 0.0
 100  CONTINUE
      ENDIF
C
      IF(LAT.LE.NZL1) THEN
      DO 105 I = 1, IDIM
      ZNLSL(2,1,IND) = ZNLSL(2,1,IND) + F(I)      * WGT
      ZNLSL(6,1,IND) = ZNLSL(6,1,IND) + F(I+IDIM) * WGT
 105  CONTINUE
      DO 10 L=2,6
      DO 110 I = 1, IDIM
      IF(LSMSK(I,L)) THEN
        ZNLSL(2,L,IND) = ZNLSL(2,L,IND) + F(I) * WGT
      ENDIF
      IF(LSMSK(I+IDIM,L)) THEN
        ZNLSL(6,L,IND) = ZNLSL(6,L,IND) + F(I+IDIM) * WGT
      ENDIF
 110  CONTINUE
   10 CONTINUE
      ENDIF
C
      IF(LAT.GT.NZL1.AND.LAT.LE.NZL2) THEN
      DO 115 I = 1, IDIM
      ZNLSL(3,1,IND) = ZNLSL(3,1,IND) + F(I)      * WGT
      ZNLSL(5,1,IND) = ZNLSL(5,1,IND) + F(I+IDIM) * WGT
 115  CONTINUE
      DO 20 L=2,6
      DO 120 I = 1, IDIM
      IF(LSMSK(I,L)) THEN
        ZNLSL(3,L,IND) = ZNLSL(3,L,IND) + F(I) * WGT
      ENDIF
      IF(LSMSK(I+IDIM,L)) THEN
        ZNLSL(5,L,IND) = ZNLSL(5,L,IND) + F(I+IDIM) * WGT
      ENDIF
 120  CONTINUE
   20 CONTINUE
      ENDIF
C
      IF(LAT.GT.NZL2) THEN
      DO 125 I = 1, IDIMT
      ZNLSL(4,1,IND) = ZNLSL(4,1,IND) + F(I)      * WGT
 125  CONTINUE
      DO 30 L=2,6
      DO 130 I = 1, IDIMT
      IF(LSMSK(I,L)) THEN
        ZNLSL(4,L,IND) = ZNLSL(4,L,IND) + F(I) * WGT
      ENDIF
 130  CONTINUE
   30 CONTINUE
      ENDIF
C
      IF(LAT.EQ.JDIMHF) THEN
      DO 40 L=1,6
      ZNLSL(1,L,IND)=0.0
      DO 50 J=2,6
      ZNLSL(1,L,IND)=ZNLSL(1,L,IND)+ZNLSL(J,L,IND)
   50 CONTINUE
      DO 60 J=1,6
      ZNLSL(J,L,IND)=ZNLSL(J,L,IND)*WEIS(J,L)
   60 CONTINUE
   40 CONTINUE
      ENDIF
C
      RETURN
      END
      SUBROUTINE ZNLODY(FH,
     1                ZNLSL,NZL1,NZL2,LSMSK,WEIS,IDIM,JDIM,IDIMT,NRCZNL)
C
      LOGICAL LSMSK(IDIMT,6)
      DIMENSION ZNLSL(6,6,NRCZNL),WEIS(6,6)
C
C  THIS ROUTINE PRINTS OUT ZONAL AVERAGES
C
C
      CHARACTER*21 IFMT0
      CHARACTER*31 IFMT1
      CHARACTER*37 IFMT2
      CHARACTER*7  NLAT(6)
C
      CHARACTER*8  LTTLSL(100)
      CHARACTER*8  LBLNK
      DIMENSION  IPWRSL(30)
C
      DATA LBLNK/'        '/
C
      DATA NLAT/'90N-90S','90N-60N','60N-30N','30N-30S','30S-60S',
     1          '60S-90S'/
C
      LTTLSL( 1)='TSFC    '
      LTTLSL( 2)='SOILM1  '
      LTTLSL( 3)='SOILM2  '
      LTTLSL( 4)='SNOW    '
      LTTLSL( 5)='TG1     '
      LTTLSL( 6)='TG2     '
      LTTLSL( 7)='ZORL    '
      LTTLSL( 8)='CV      '
      LTTLSL( 9)='CVB     '
      LTTLSL(10)='CVT     '
      LTTLSL(11)='ALB     '
      LTTLSL(12)='PLANTR  '
      LTTLSL(13)='CNPWC   '
      LTTLSL(14)='SLIMSK  '
      DATA IPWRSL/0,0,0,1,0,0,0,0,0,0,-2,0,0,0,
     1   16*0/
C
      JDIMHF=JDIM/2
C
      PRINT *,'@@@@ START OF ZONAL DIAGNOSTIC PRINT @@@'
C
C  SINGLE LEVEL FIELD
C
      K1=1
      K2=6
      KXXX=K2-K1+1
      DO 300 ITM=1,NRCZNL
      IF(LTTLSL(ITM).EQ.LBLNK) GO TO 300
      WRITE(6,100) LTTLSL(ITM),IPWRSL(ITM),FH
      WRITE(6,110)
      KYYY=-IPWRSL(ITM)
      WRITE(IFMT0,130) KYYY,KXXX
      WRITE(6,IFMT0)(NLAT(J),(ZNLSL(J,K,ITM),K=K1,K2),J=1,6)
  300 CONTINUE
C
      PRINT *,'@@@@ END OF ZONAL DIAGNOSTIC PRINT @@@'
C
  100 FORMAT(1X,A8,5H(10**,I3,1H),' FH=',F7.1)
  110 FORMAT(2X,5H LAT ,7X,'MEAN',6X,'LND',3X,'SN-LND',6X,'ICE',
     1                  3X,'SN-ICE',6X,'SEA')
  130 FORMAT(10H(1X,A7,1X,,I2,1HP,I2,5HF9.2))
      RETURN
      END
      SUBROUTINE MSKRD(LUGI,LUGB,IDIM,JDIM,IJDIM,IY,IM,ID,IH,FH,
     1                 FNOROG,FNMASK,FIOROG,FIMASK,KPDORO,KPDMSK,
     2                 OROG,SLMASK)
C
      CHARACTER*80 FNOROG,FNMASK
      CHARACTER*80 FIOROG,FIMASK
      DIMENSION OROG(IJDIM),SLMASK(IJDIM)
C
      LOGICAL LCLIM
C
      LCLIM=.true.
C
C  OROGRAPHY
C
      CALL FIXRD(LUGI,LUGB,FNOROG,FIOROG,KPDORO,LCLIM,SLMASK,
     1           IY,IM,ID,IH,FH,OROG,IDIM,JDIM,IRET)
C
C  Land/sea mask
C
      CALL FIXRD(LUGI,LUGB,FNMASK,FIMASK,KPDMSK,LCLIM,SLMASK,
     1           IY,IM,ID,IH,FH,SLMASK,IDIM,JDIM,IRET)
      DO IJ=1,IJDIM
        SLMASK(IJ)=NINT(SLMASK(IJ))
      ENDDO
C
      RETURN
      END
      SUBROUTINE FIXRD(LUGI,LUGB,FNGRIB,FNINDX,KPDS5,LCLIM,SLMASK,
     1                 IY,IM,ID,IH,FH,GDATA,IGAUL,JGAUL,IRET)
C
C Read in grib climatology files.
C
C Interpolate climatology to the dates
C
C Grib file should allow all the necessary parameters to be extracted from
C the description records.
C
C
C  NREPMX is max number of days for going back date search
C
      PARAMETER(NREPMX=31)
C
C  NVALID:  Analysis later than (Current date - NVALID) is regarded as
C           valid for current analysis  
C
      PARAMETER(NVALID=4)
C
      CHARACTER*80 FNGRIB,FNINDX
      DIMENSION GDATA(1)
C
      DIMENSION SLMASK(IGAUL*JGAUL)
C
      LOGICAL LMASK
C
      PARAMETER(MDATA=1000*500)
      LOGICAL LBMS(MDATA)
      REAL DATA(MDATA*2)
      INTEGER KPDS(1000),KGDS(1000)
C
      DIMENSION RSLMSK(MDATA)
C
      DIMENSION KPDS0(1000)
C
      CHARACTER*80 ASGNSTR
C
      LOGICAL LCLIM
C
C JULIAN DAY OF THE MIDDLE OF EACH MONTH
C
	  common /y2k/ idat,jdat,rinc
	  integer idat(8),jdat(8)
	  real    rinc(5)

      DIMENSION DAYHF(13)
      DATA DAYHF/ 15.5, 45.0, 74.5,105.0,135.5,166.0,
     1           196.5,227.5,258.0,288.5,319.0,349.5,380.5/
C
C NUMBER OF DAYS IN A MONTH
C
      DIMENSION MJDAY(12)
      DATA MJDAY/31,28,31,30,31,30,31,31,30,31,30,31/
C
C JULIAN DAY OF THE FIRST DAY OF THE MONTH
C
      DIMENSION FJDAY(12)
C
      LOGICAL IJORDR
C
      IRET=0
C
      MONEND=9999
C
      FJDAY(1)=1.
      DO MON=2,12
        FJDAY(MON)=FJDAY(MON-1)+FLOAT(MJDAY(MON-1))
      ENDDO
C
C  GET JULIAN DAY of the iy/im/id/ih provided.
C
      RJDAY=0.
      IMM=IM-1
      IF(IMM.GE.1) THEN
        DO MON=1,IMM
          RJDAY=RJDAY+MJDAY(MON)
        ENDDO
      ENDIF
      RJDAY=RJDAY+ID+FLOAT(IH)/24.0
      RJDAY=RJDAY+FH/24.0
      RJDAY = MOD(RJDAY,365.)
      IF(RJDAY.EQ.0.) RJDAY = 365.
      IF(RJDAY.LE.0..OR.RJDAY.GT.365.) THEN
        PRINT *,'WRONG RJDAY',RJDAY
        CALL ABORT
      ENDIF
      DO MON=1,11
        IF(RJDAY.LT.FJDAY(MON+1)) THEN
          MFMON=MON
          GO TO 10
        ENDIF
      ENDDO
      MFMON=12
   10 CONTINUE
      IF(RJDAY.LT.DAYHF(1)) RJDAY=RJDAY+365.
C
C  Compute JY,JM,JD,JH of forecast
C
      if (iy .lt. 100) then
c		call w3fs11(xi,iy,im,id,ih,0)
c		mfft=nint(fh)
c		call w3fs15(xi,mfft,xf)
c		call w3fs11(xf,jy,jm,jd,jh,-1)
        WRITE(*,*) 'Forecast JY,JM,JD,JH=',JY,JM,JD,JH
        write(*,*) 'IY lt 100 .. fatal error'
      else
		idat=0
		idat(1)=iy
		idat(2)=im
		idat(3)=id
		idat(5)=ih
		rinc=0
		rinc(2)=fh
		call w3movdat(rinc,idat,jdat)
		jy4=jdat(1)
		jc=(jy4-1)/100+1
		jy=mod(jy4-1,100)+1
		jm=jdat(2)
		jd=jdat(3)
		jh=jdat(5)
        WRITE(6,*) 'Forecast JY4,JM,JD,JH=',JY4,JM,JD,JH
      endif
C
      WRITE(6,*) ' '
      WRITE(6,*) '************************************************'
C
c     IF(LUGI.LT.10) THEN
c       WRITE(ASGNSTR,'(22Hassign -s unblocked u:,I1,)') LUGI
c     ELSE
c       WRITE(ASGNSTR,'(22Hassign -s unblocked u:,I2,)') LUGI
c     ENDIF
c     CALL ASSIGN(ASGNSTR)

c     OPEN(UNIT=LUGI,FILE=FNINDX,FORM='UNFORMATTED',ERR=900)
c     GO TO 901
c 900   CONTINUE
c       WRITE(6,*) ' ERROR IN OPENING FILE ',FNINDX(1:50)
c       PRINT *,'ERROR IN OPENING FILE ',FNINDX(1:50)
c       CALL ABORT
c 901 CONTINUE

      call baopenr(lugi, trim(fnindx), ierr)
      if (ierr.ne.0) then
          WRITE(*,*) ' ERROR IN OPENING FILE ',FNINDX(1:50)
          call abort
      endif
      WRITE(6,*) ' FILE ',FNINDX(1:50),' opened. Unit=',LUGI

C
c     IF(LUGB.LT.10) THEN
c       WRITE(ASGNSTR,'(22Hassign -s unblocked u:,I1,)') LUGB
c     ELSE
c       WRITE(ASGNSTR,'(22Hassign -s unblocked u:,I2,)') LUGB
c     ENDIF
c     CALL ASSIGN(ASGNSTR)
c     OPEN(UNIT=LUGB,FILE=FNGRIB,FORM='UNFORMATTED',ERR=910)
c     GO TO 911
c 910   CONTINUE
c       WRITE(6,*) ' ERROR IN OPENING FILE ',FNGRIB(1:50)
c       PRINT *,'ERROR IN OPENING FILE ',FNGRIB(1:50)
c       CALL ABORT
c 911 CONTINUE

      call baopenr(lugb, trim(fngrib), ierr)
      if (ierr.ne.0) then
          WRITE(*,*) ' ERROR IN OPENING FILE ',FNGRIB(1:50)
          call abort
      endif
      WRITE(6,*) ' FILE ',FNGRIB(1:50),' opened. Unit=',LUGB

C
c     REWIND LUGI
C
C  Find file type climatology or analysis
C
      N=-1
      KPDS=-1
      KPDS(5)=KPDS5
      CALL PDSCAN(LUGI,KPDS,N,LSKIP,LGRIB)
      WRITE(6,*) ' First grib record.'
      WRITE(6,*) ' KPDS( 1-10)=',(KPDS(J),J= 1,10)
      WRITE(6,*) ' KPDS(11-20)=',(KPDS(J),J=11,20)
      WRITE(6,*) ' KPDS(21-  )=',(KPDS(J),J=21,22)
      KPDS0=KPDS
      KPDS0(4)=-1
      KPDS0(18)=-1
      IF(LGRIB.EQ.0) THEN
        WRITE(6,*) ' Error in PDSCAN.  Field not found.'
        N=0
        KPDS=-1
        CALL PDSCAN(LUGI,KPDS,N,LSKIP,LGRIB)
        WRITE(6,*) ' KPDS( 1-10)=',(KPDS(J),J= 1,10)
        WRITE(6,*) ' KPDS(11-20)=',(KPDS(J),J=11,20)
        WRITE(6,*) ' KPDS(21-  )=',(KPDS(J),J=21,22)
        CALL ABORT
      ENDIF
C
      IF(KPDS(16).EQ.51) THEN
        WRITE(6,*) ' Climatology file.'
        IF(.NOT.LCLIM) THEN
          WRITE(6,*) 'Error.  Not a climatology file.'
          CALL ABORT
        ENDIF
      ELSE
        WRITE(6,*) ' Analysis file.'
        IF(LCLIM) THEN
          WRITE(6,*) 'Error.  Not an analysis file.'
          CALL ABORT
        ENDIF
      ENDIF
C
C   Handling climatology file
C
      IF(LCLIM) THEN
C
C       Find average type 
C         WEEKLY,BIWEEKLY,MONTHLY,SEASONAL,ANNUAL
C
C  KPDS(13)=4 & KPDS(15)=1 .. Annual Mean
C  KPDS(13)=3 & KPDS(15)=1 .. Monthly Mean
C  KPDS(13)=2 & KPDS(15)=7 .. Weekly Mean
C  KPDS(13)=2 & KPDS(15)=14.. Bi-Weekly Mean
C
        IF(KPDS(13).EQ.2.AND.KPDS(15).EQ.7) THEN
          WRITE(6,*) ' This is weekly mean climatology'
          WRITE(6,*) ' Cannot process.'
          CALL ABORT
        ELSEIF(KPDS(13).EQ.2.AND.KPDS(15).EQ.14) THEN
          WRITE(6,*) ' This is bi-weekly mean climatology'
          MONEND=4
          IS=IM/3+1
          IF(IS.EQ.5) IS=1
          WRITE(6,*) ' Cannot process.'
          CALL ABORT
        ELSEIF(KPDS(13).EQ.3.AND.KPDS(15).LE.1) THEN
          WRITE(6,*) ' This is monthly mean climatology'
          MONEND=12
          DO MM=1,MONEND
            MMM=MM
            MMP=MM+1
            IF(RJDAY.GE.DAYHF(MMM).AND.RJDAY.LT.DAYHF(MMP)) THEN
              MON1=MMM
              MON2=MMP
              GO TO 20
            ENDIF
          ENDDO
          PRINT *,'WRONG RJDAY',RJDAY
          CALL ABORT
   20     CONTINUE
          IJMAX=0
          DO NN=1,2
            N=0
            KPDS=KPDS0
            IF(NN.EQ.1) KPDS( 9)=MON1
            IF(NN.EQ.2) KPDS( 9)=MON2
            IF(KPDS(9).EQ.13) KPDS(9)=1
            CALL PDSCAN(LUGI,KPDS,N,LSKIP,LGRIB)
            WRITE(6,*) ' Input grib file dates=',(KPDS(I),I=8,11)
            IF(LGRIB.NE.0) THEN
              WRITE(6,*) ' KPDS( 1-10)=',(KPDS(J),J= 1,10)
              WRITE(6,*) ' KPDS(11-20)=',(KPDS(J),J=11,20)
              WRITE(6,*) ' KPDS(21-  )=',(KPDS(J),J=21,22)
              CALL RDGB(LUGB,LGRIB,LSKIP,KPDS,KGDS,NDATA,LBMS,
     1                  DATA(IJMAX*(NN-1)+1))
              IF(NDATA.EQ.0) THEN
                WRITE(6,*) ' Error in RDGB'
                WRITE(6,*) ' KPDS=',KPDS
                WRITE(6,*) ' KGDS=',KGDS
                WRITE(6,*) ' LGRIB,LSKIP=',LGRIB,LSKIP
                CALL ABORT
              ENDIF
              IMAX=KGDS(2)
              JMAX=KGDS(3)
              IJMAX=IMAX*JMAX
              WRITE(6,*) 'IMAX,JMAX,IJMAX=',IMAX,JMAX,IJMAX
            ELSE
              WRITE(6,*) ' Error in PDSCAN'
              N=-1
              LGRIB=-1
              DOWHILE(LGRIB.NE.0) 
                KPDS=-1
                CALL PDSCAN(LUGI,KPDS,N,LSKIP,LGRIB)
                WRITE(6,*) ' KPDS( 1-10)=',(KPDS(J),J= 1,10)
                WRITE(6,*) ' KPDS(11-20)=',(KPDS(J),J=11,20)
                WRITE(6,*) ' KPDS(21-  )=',(KPDS(J),J=21,22)
              ENDDO
              CALL ABORT
            ENDIF
          ENDDO
          WEI1=(DAYHF(MON2)-RJDAY)/(DAYHF(MON2)-DAYHF(MON1))
          WEI2=(RJDAY-DAYHF(MON1))/(DAYHF(MON2)-DAYHF(MON1))
          IF(MON2.EQ.13) MON2=1
          PRINT *,'RJDAY,MON1,MON2,WEI1,WEI2=',
     1             RJDAY,MON1,MON2,WEI1,WEI2
          DO I=1,IJMAX
            DATA(I)=WEI1*DATA(I)+WEI2*DATA(IJMAX+I)
          ENDDO
        ELSEIF(KPDS(13).EQ.4.AND.KPDS(15).EQ.3) THEN
          WRITE(6,*) ' This is seasonal mean climatology'
          MONEND=4
          IS=IM/3+1
          IF(IS.EQ.5) IS=1
          IS1=MON1/3+1
          IF(IS1.EQ.5) IS1=1
          IS2=MON2/3+1
          IF(IS2.EQ.5) IS2=1
          DO MM=1,MONEND
            MMM=MM*3-2
            MMP=(MM+1)*3-2
            IF(RJDAY.GE.DAYHF(MMM).AND.RJDAY.LT.DAYHF(MMP)) THEN
              MON1=MMM
              MON2=MMP
              GO TO 30
            ENDIF
          ENDDO
          PRINT *,'WRONG RJDAY',RJDAY
          CALL ABORT
   30     CONTINUE
          IJMAX=0
          DO NN=1,2
            KPDS=KPDS0
            N=0
            IF(NN.EQ.1) THEN
              ISX=IS1
            ELSE
              ISX=IS2
            ENDIF
            IF(ISX.EQ.1) KPDS(9)=12
            IF(ISX.EQ.2) KPDS(9)=3
            IF(ISX.EQ.3) KPDS(9)=6
            IF(ISX.EQ.4) KPDS(9)=9
            IF(KPDS(9).EQ.13) KPDS(9)=1
            CALL PDSCAN(LUGI,KPDS,N,LSKIP,LGRIB)
            WRITE(6,*) ' Input grib file dates=',(KPDS(I),I=8,11)
            IF(LGRIB.NE.0) THEN
              CALL RDGB(LUGB,LGRIB,LSKIP,KPDS,KGDS,NDATA,LBMS,
     1                  DATA(IJMAX*(NN-1)+1))
              IF(NDATA.EQ.0) THEN
                WRITE(6,*) ' Error in RDGB'
                WRITE(6,*) ' KPDS=',KPDS
                WRITE(6,*) ' KGDS=',KGDS
                WRITE(6,*) ' LGRIB,LSKIP=',LGRIB,LSKIP
                CALL ABORT
              ENDIF
              IMAX=KGDS(2)
              JMAX=KGDS(3)
              IJMAX=IMAX*JMAX
            ELSE
              WRITE(6,*) ' Error in PDSCAN'
              CALL ABORT
            ENDIF
          ENDDO
          WEI1=(DAYHF(MON2)-RJDAY)/(DAYHF(MON2)-DAYHF(MON1))
          WEI2=(RJDAY-DAYHF(MON1))/(DAYHF(MON2)-DAYHF(MON1))
          IF(MON2.EQ.13) MON2=1
          PRINT *,'RJDAY=',RJDAY
          PRINT *,'MON1 =',MON1 ,' MON2=',MON2
          PRINT *,'WEI1 =',WEI1 ,' WEI2=',WEI2
          PRINT *,'SES1 =', IS1 ,' SES2=', IS2
          DO I=1,IJMAX
            DATA(I)=WEI1*DATA(I)+WEI2*DATA(IJMAX+I)
          ENDDO
        ELSEIF(KPDS(13).EQ.4.AND.KPDS(15).EQ.1) THEN
          WRITE(6,*) ' This is annual mean climatology'
          CALL RDGB(LUGB,LGRIB,LSKIP,KPDS,KGDS,NDATA,LBMS,DATA)
          IF(NDATA.EQ.0) THEN
            WRITE(6,*) ' Error in RDGB'
            WRITE(6,*) ' KPDS=',KPDS
            WRITE(6,*) ' KGDS=',KGDS
            WRITE(6,*) ' LGRIB,LSKIP=',LGRIB,LSKIP
            CALL ABORT
          ENDIF
          IMAX=KGDS(2)
          JMAX=KGDS(3)
          IJMAX=IMAX*JMAX
        ELSE
          WRITE(6,*) ' Climatology file average period unknown.'
          WRITE(6,*) ' KPDS(13)=',KPDS(13),' KPDS(15)=',KPDS(15)
          CALL ABORT
        ENDIF
      ELSE
C
C  Handling analysis file
C
C  Find record for the given hour/day/month/year
C
        NREPT=0
        KPDS=KPDS0
		icn=jc
        IYR=JY
        IMO=JM
        IDY=JD
        IHR=JH
   50   CONTINUE
        KPDS( 8)=IYR
        KPDS( 9)=IMO
        KPDS(10)=IDY
        KPDS(11)=IHR
		kpds(23)=icn
        N=0
        CALL PDSCAN(LUGI,KPDS,N,LSKIP,LGRIB)
        WRITE(6,*) ' Input grib file dates=',(KPDS(I),I=8,11)
        IF(LGRIB.NE.0) THEN
          CALL RDGB(LUGB,LGRIB,LSKIP,KPDS,KGDS,NDATA,LBMS,DATA)
          IF(NDATA.EQ.0) THEN
            WRITE(6,*) ' Error in RDGB'
            WRITE(6,*) ' KPDS=',KPDS
            WRITE(6,*) ' KGDS=',KGDS
            WRITE(6,*) ' LGRIB,LSKIP=',LGRIB,LSKIP
            CALL ABORT
          ENDIF
          IMAX=KGDS(2)
          JMAX=KGDS(3)
          IJMAX=IMAX*JMAX
        ELSE
          IF(NREPT.EQ.0) THEN
            WRITE(6,*) ' No matching dates found.  Start searching',
     1                 ' nearest matching dates (going back).'
          ENDIF
C
C  No matching IH found. Search nearest hour
C
c         IF(IHR.EQ.6) THEN
          IF(IHR.GT.0.AND.IHR.LE.12) THEN
c           IHR=0
c           GO TO 50
c         ELSEIF(IHR.EQ.12) THEN
            IHR=0
            GO TO 50
c         ELSEIF(IHR.EQ.18) THEN
          ELSEIF(IHR.GT.12.AND.IHR.LT.24) THEN
            IHR=12
            GO TO 50
          ELSEIF(IHR.EQ.0.OR.IHR.EQ.-1) THEN
            IDY=IDY-1
            IF(IDY.EQ.0) THEN
              IMO=IMO-1
              IF(IMO.EQ.0) THEN
                IYR=IYR-1
                IF(IYR.eq.0) then
				   iyr=100
				   icn=icn-1
			    endif
                IMO=12
              ENDIF
              IDY=31
              IF(IMO.EQ.4.OR.IMO.EQ.6.OR.IMO.EQ.9.OR.IMO.EQ.11) IDY=30
              IF(IMO.EQ.2) THEN
                IF(MOD(IYR,4).EQ.0) THEN
                  IDY=29
                ELSE
                  IDY=28
                ENDIF
              ENDIF
            ENDIF
            IHR=-1
            WRITE(6,*) ' Decremented dates=',IYR,IMO,IDY,IHR
            NREPT=NREPT+1
            IF(NREPT.GT.NVALID) IRET=-1
            IF(NREPT.GT.NREPMX) THEN
              WRITE(6,*) ' <WARNING:CYCL> Searching range exceeded.'
              WRITE(6,*) ' <WARNING:CYCL> FNGRIB=',FNGRIB(1:50)
              WRITE(6,*) ' <WARNING:CYCL> Terminating search and',
     1                   ' use guess.'
              WRITE(6,*) ' Range max=',NREPMX
              IMAX=KGDS(2)
              JMAX=KGDS(3)
              IJMAX=IMAX*JMAX
              DO IJ=1,IJMAX
                DATA(IJ)=0.
              ENDDO
              GO TO 80
            ENDIF
            GO TO 50
          ELSE
            WRITE(6,*) ' Search of analysis for IHR=',IHR,' failed.'
            WRITE(6,*) ' KPDS=',KPDS
            WRITE(6,*) ' IYR,IMO,IDY,IHR=',IYR,IMO,IDY,IHR
            GO TO 100
          ENDIF
        ENDIF
      ENDIF
C
   80 CONTINUE
      WRITE(6,*) ' MAXMIN of input as is'
      CALL MAXMIN(DATA,IJMAX,1,1)
C
      CALL GETAREA(KGDS,DLAT,DLON,RSLAT,RNLAT,WLON,ELON,IJORDR)
      WRITE(6,*) 'IMAX,JMAX,IJMAX,DLON,DLAT,IJORDR,WLON,RNLAT='
      WRITE(6,*)  IMAX,JMAX,IJMAX,DLON,DLAT,IJORDR,WLON,RNLAT
      CALL SUBST(DATA,IMAX,JMAX,IJMAX,DLON,DLAT,IJORDR)
C
      IF(KGDS(1).NE.4) THEN
C
C   First get SLMASK over input grid
C
        CALL SETRMSK(KPDS5,SLMASK,IGAUL,JGAUL,WLON,RNLAT,
     1               DATA,IMAX,JMAX,LMASK,RSLMSK)
        WRITE(6,*) ' KPDS5=',KPDS5,' LMASK=',LMASK
        CALL LA2GA(DATA,IMAX,JMAX,ABS(DLON),ABS(DLAT),WLON,RNLAT,
     1             GDATA,IGAUL,JGAUL,LMASK,RSLMSK,SLMASK)
      ELSE
        IF(IMAX.NE.IGAUL.OR.JMAX.NE.JGAUL) THEN
          WRITE(6,*) '  Input Gaussian grid file does not match ',
     1                   'the model resoultion.'
          CALL ABORT
        ELSE
          DO IJ=1,IGAUL*JGAUL
            GDATA(IJ)=DATA(IJ)
          ENDDO
        ENDIF
      ENDIF
C
      WRITE(6,*) ' '
      RETURN
C
  100 CONTINUE
      IRET=1
      DO IJ=1,IGAUL*JGAUL
        GDATA(IJ)=-999.
      ENDDO
C
      RETURN
      END
      SUBROUTINE GETAREA(KGDS,DLAT,DLON,RSLAT,RNLAT,WLON,ELON,IJORDR)
C
C  Get area of the grib record
C
      DIMENSION KGDS(25)
      LOGICAL IJORDR
C
      WRITE(6,*) ' KGDS( 1-10)=',(KGDS(J),J= 1,10)
      WRITE(6,*) ' KGDS(11-20)=',(KGDS(J),J=11,20)
      WRITE(6,*) ' KGDS(21-  )=',(KGDS(J),J=21,22)
C
      IF(KGDS(1).EQ.0) THEN
C
C  Lat/Lon grid
C
        WRITE(6,*) 'LAT/LON GRID'
        DLAT=FLOAT(KGDS(10))/1000.0
        DLON=FLOAT(KGDS( 9))/1000.0
        F0LON=FLOAT(KGDS(5))/1000.0
        F0LAT=FLOAT(KGDS(4))/1000.0
        KGDS11=KGDS(11)
        IF(KGDS11.GE.128) THEN
          WLON=F0LON-DLON*(KGDS(2)-1)
          ELON=F0LON
          IF(DLON*KGDS(2).GT.359.99) THEN
            WLON=F0LON-DLON*KGDS(2)
          ENDIF
          DLON=-DLON
          KGDS11=KGDS11-128
        ELSE
          WLON=F0LON
          ELON=F0LON+DLON*(KGDS(2)-1)
          IF(DLON*KGDS(2).GT.359.99) THEN 
            ELON=F0LON+DLON*KGDS(2)
          ENDIF
        ENDIF
        IF(KGDS11.GE.64) THEN
          RNLAT=F0LAT+DLAT*(KGDS(3)-1)
          RSLAT=F0LAT
          KGDS11=KGDS11-64
        ELSE
          RNLAT=F0LAT
          RSLAT=F0LAT-DLAT*(KGDS(3)-1)
          DLAT=-DLAT
        ENDIF
        IF(KGDS11.GE.32) THEN
          IJORDR=.false.
        ELSE
          IJORDR=.true.
        ENDIF

        IF(WLON.GT.180.) WLON=WLON-360.
        IF(ELON.GT.180.) ELON=ELON-360.
        WLON=NINT(WLON*1000.)/1000.
        ELON=NINT(ELON*1000.)/1000.
        RSLAT=NINT(RSLAT*1000.)/1000.
        RNLAT=NINT(RNLAT*1000.)/1000.
        RETURN
C
C  Mercator projection
C
      ELSEIF(KGDS(1).EQ.1) THEN
        WRITE(6,*) 'Mercator GRID'
        WRITE(6,*) 'Cannot process'
        CALL ABORT
C
C  Gnomonic projection
C
      ELSEIF(KGDS(1).EQ.2) THEN
        WRITE(6,*) 'Gnomonic GRID'
        WRITE(6,*) 'ERROR!! Gnomonic projection not coded'
        CALL ABORT
C
C  Lambert conformal
C
      ELSEIF(KGDS(1).EQ.3) THEN
        WRITE(6,*) 'Lambert conformal'
        WRITE(6,*) 'Cannot process'
        CALL ABORT
      ELSEIF(KGDS(1).EQ.4) THEN
C
C  Gaussian grid
C
        WRITE(6,*) 'Gaussian GRID'
        DLAT=99.
        DLON=FLOAT(KGDS( 9))/1000.0
        F0LON=FLOAT(KGDS(5))/1000.0
        F0LAT=99.
        KGDS11=KGDS(11)
        IF(KGDS11.GE.128) THEN
          WLON=F0LON
          ELON=F0LON
          IF(DLON*KGDS(2).GT.359.99) THEN
            WLON=F0LON-DLON*KGDS(2)
          ENDIF
          DLON=-DLON
          KGDS11=KGDS11-128
        ELSE
          WLON=F0LON
          ELON=F0LON+DLON*(KGDS(2)-1)
          IF(DLON*KGDS(2).GT.359.99) THEN 
            ELON=F0LON+DLON*KGDS(2)
          ENDIF
        ENDIF
        IF(KGDS11.GE.64) THEN
          RNLAT=99.
          RSLAT=99.
        ELSE
          RNLAT=99.
          RSLAT=99.
          DLAT=-99.
        ENDIF
        IF(KGDS11.GE.32) THEN
          IJORDR=.false.
        ELSE
          IJORDR=.true.
        ENDIF
        RETURN
C
C  Polar Strereographic
C
      ELSEIF(KGDS(1).EQ.5) THEN
        WRITE(6,*) 'Polar Stereographic GRID'
        WRITE(6,*) 'Cannot process'
        CALL ABORT
        RETURN
C
C  Oblique Lambert conformal
C
      ELSEIF(KGDS(1).EQ.13) THEN
        WRITE(6,*) 'Oblique Lambert conformal GRID'
        WRITE(6,*) 'Cannot process'
        CALL ABORT
C
C  Spherical Coefficient
C
      ELSEIF(KGDS(1).EQ.50) THEN
        WRITE(6,*) 'Spherical Coefficient'
        WRITE(6,*) 'Cannot process'
        CALL ABORT
        RETURN
C
C  Space view perspective (orthographic grid)
C
      ELSEIF(KGDS(1).EQ.90) THEN
        WRITE(6,*) 'Space view perspective GRID'
        WRITE(6,*) 'Cannot process'
        CALL ABORT
        RETURN
C
C  Unknown projection.  Abort.
C
      ELSE
        WRITE(6,*) 'ERROR!! Unknown map projection'
        WRITE(6,*) 'KGDS(1)=',KGDS(1)
        PRINT *,'ERROR!! Unknown map projection'
        PRINT *,'KGDS(1)=',KGDS(1)
        CALL ABORT
      ENDIF
C
      RETURN
      END
      SUBROUTINE SUBST(DATA,IMAX,JMAX,IJMAX,DLON,DLAT,IJORDR)
C
      LOGICAL IJORDR
C
      DIMENSION DATA(IJMAX)
C
      DIMENSION WORK(IJMAX)
C
      IF(.NOT.IJORDR.OR.
     1  (IJORDR.AND.(DLAT.GT.0..OR.DLON.LT.0.))) THEN
        IF(.NOT.IJORDR) THEN
          IJ=0
          DO J=1,JMAX
            DO I=1,IMAX
              IJ=(J-1)*IMAX+I
              JI=(I-1)*JMAX+J
              WORK(IJ)=DATA(JI)
            ENDDO
          ENDDO
        ELSE
          DO J=1,JMAX
            DO I=1,IMAX
              IJ=(J-1)*IMAX+I
              WORK(IJ)=DATA(IJ)
            ENDDO
          ENDDO
        ENDIF
        DO J=1,JMAX
          DO I=1,IMAX
            IF(DLAT.GT.0..AND.DLON.GT.0.) THEN
              IJ=IMAX*JMAX-IMAX*J+I
            ELSEIF(DLAT.GT.0..AND.DLON.LT.0.) THEN
              IJ=IMAX*JMAX-(J-1)*IMAX-IMAX+I-1
            ELSEIF(DLAT.LT.0..AND.DLON.LT.0.) THEN
              IJ=IMAX*(J-1)+IMAX-I+1
            ENDIF
            IJO=(J-1)*IMAX+I
            DATA(IJ)=WORK(IJO)
          ENDDO
        ENDDO
      ENDIF
      RETURN
      END
      SUBROUTINE LA2GA(REGIN ,IMXIN ,JMXIN , DLOIN,DLAIN, RLON, RLAT,
     1                 GAUOUT,IMXOUT,JMXOUT,LMASK,RSLMSK,SLMASK)
C
C  INTERPOLATION FROM LAT/LON OR GAUSSIAN GRID TO OTHER LAT/LON GRID
C
      SAVE
C
      DIMENSION REGIN (IMXIN ,JMXIN )
      DIMENSION GAUOUT(IMXOUT,JMXOUT)
C
      DIMENSION RSLMSK(IMXIN,JMXIN)
      DIMENSION SLMASK(IMXOUT,JMXOUT)
C
      DIMENSION GAULO (500)
      DIMENSION GAULI (500)
      DIMENSION RINLAT(500),OUTLAT(500)
      DIMENSION RINLON(1000)
C
      DIMENSION IINDX1(1000)
      DIMENSION IINDX2(1000)
      DIMENSION JINDX1(500)
      DIMENSION JINDX2(500)
C
      DIMENSION DDX(1000)
      DIMENSION DDY(500)
C
      LOGICAL LMASK
C
      DATA IFPI,JFPI,IFPO,JFPO,RFP1,RFP2/4*0,2*0./
C
      IF(IMXIN.EQ.1.OR.JMXIN.EQ.1) THEN
        DO J=1,JMXOUT
           DO I=1,IMXOUT
            GAUOUT(I,J)=0.
           ENDDO
        ENDDO
        RETURN
      ENDIF
C
      IF(DLOIN.EQ.0..OR.DLAIN.EQ.0.) THEN
        PRINT *,'DLOIN OR DLAIN IS ZERO .... CHECK DATA CARDS'
      ENDIF
C
      IF(IFPI.EQ.IMXIN .AND.JFPI.EQ.JMXIN .AND.
     1   IFPO.EQ.IMXOUT.AND.JFPO.EQ.JMXOUT.AND.
     2   RFP1.EQ.RLON.AND.RFP2.EQ.RLAT) GO TO 111
C
      IFPI=IMXIN
      JFPI=JMXIN
      IFPO=IMXOUT
      JFPO=JMXOUT
      RFP1=RLON
      RFP2=RLAT
C
C     PRINT *,'DLOIN=',DLOIN
C     PRINT *,'DLAIN=',DLAIN
C     PRINT *,'RLON=',RLON
C     PRINT *,'RLAT=',RLAT
C
      DO J=1,JMXIN
        IF(RLAT.GT.0.) THEN
          RINLAT(J)=RLAT-FLOAT(J-1)*DLAIN
        ELSE
          RINLAT(J)=RLAT+FLOAT(J-1)*DLAIN
        ENDIF
      ENDDO
C
C     PRINT *,'RINLAT='
C     PRINT *,(RINLAT(J),J=1,JMXIN)
C
C    COMPUTE GAUSSIAN LATITUDE FOR OUTPUT GRID
C
      CALL GAULAT(GAULO,JMXOUT)
C
      DO J=1,JMXOUT
        OUTLAT(J)=90.-GAULO(J)
      ENDDO
C
C     PRINT *,'OUTLAT='
C     PRINT *,(OUTLAT(J),J=1,JMXOUT)
C
      DO I=1,IMXIN
        RINLON(I)=RLON+FLOAT(I-1)*DLOIN
      ENDDO
C
C     PRINT *,'RINLON='
C     PRINT *,(RINLON(I),I=1,IMXIN)
C
C  FIND I-INDEX FOR INTERPLATION
C
      DO 30 I=1,IMXOUT
      ALAMD=FLOAT(I-1)*360./FLOAT(IMXOUT)
      IF(RLON.LT.0.) THEN
      IF(ALAMD.GT.180.) ALAMD=ALAMD-360.
      ENDIF
      DO 35 II=1,IMXIN
      IF(ALAMD.GT.RINLON(II)) GO TO 35
      IX=II
      GO TO 32
   35 CONTINUE
      I1=360./DLOIN+0.5
      I2=1
      GO TO 34
   32 CONTINUE
      IF(IX.GE.2) GO TO 33
      I1=360./DLOIN+0.5
      I2=1
      GO TO 34
   33 CONTINUE
      I2=IX
      I1=I2-1
   34 CONTINUE
      IINDX1(I)=I1
      IINDX2(I)=I2
      DENOM=RINLON(I2)-RINLON(I1)
      IF(DENOM.LT.0.) DENOM=DENOM+360.
      RNUME=ALAMD-RINLON(I1)
      IF(RNUME.LT.0.) RNUME=RNUME+360.
      DDX(I)=RNUME/DENOM
   30 CONTINUE
C
C  FIND J-INDEX FOR INTERPLATION
C
      JQ=1
      DO 40 J=1,JMXOUT
      APHI=OUTLAT(J)
      DO 50 JJ=1,JMXIN
      JX=JJ
      IF(RLAT.LT.0.) JX=JMXIN-JJ+1
      IF(APHI.LT.RINLAT(JX)) GO TO 50
      JQ=JX
      GO TO 42
   50 CONTINUE
      IF(RLAT.GT.0.) THEN
        J1=JMXIN
        J2=JMXIN
      ELSE
        J1=1
        J2=1
      ENDIF
      GO TO 44
   42 CONTINUE
      IF(RLAT.GT.0.) THEN
         IF(JQ.GE.2) GO TO 43
         J1=1
         J2=1
      ELSE
         IF(JQ.LT.JMXIN) GO TO 43
         J1=JMXIN
         J2=JMXIN
      ENDIF
      GO TO 44
   43 CONTINUE
      IF(RLAT.GT.0.) THEN
      J2=JQ
      J1=JQ-1
      ELSE
      J1=JQ
      J2=JQ+1
      ENDIF
   44 CONTINUE
      JINDX1(J)=J1
      JINDX2(J)=J2
      IF(J2.NE.J1) THEN
         DDY(J)=(APHI-RINLAT(J1))/(RINLAT(J2)-RINLAT(J1))
      ELSE
      IF(J1.EQ.1.AND.RLAT.GT.0..OR.J1.EQ.JMXIN.AND.RLAT.LT.0.) THEN
            IF(ABS(90.-RINLAT(J1)).GT.0.001) THEN
               DDY(J)=(APHI-RINLAT(J1))/(90.-RINLAT(J1))
            ELSE
               DDY(J)=0.0
            ENDIF
      ENDIF
      IF(J1.EQ.1.AND.RLAT.LT.0..OR.J1.EQ.JMXIN.AND.RLAT.GT.0.) THEN
            IF(ABS(-90.-RINLAT(J1)).GT.0.001) THEN
               DDY(J)=(APHI-RINLAT(J1))/(-90.-RINLAT(J1))
            ELSE
               DDY(J)=0.0
            ENDIF
         ENDIF
      ENDIF
   40 CONTINUE
C
C     PRINT *,'LA2GA'
C     PRINT *,'IINDX1'
C     PRINT *,(IINDX1(N),N=1,IMXOUT)
C     PRINT *,'IINDX2'
C     PRINT *,(IINDX2(N),N=1,IMXOUT)
C     PRINT *,'JINDX1'
C     PRINT *,(JINDX1(N),N=1,JMXOUT)
C     PRINT *,'JINDX2'
C     PRINT *,(JINDX2(N),N=1,JMXOUT)
C     PRINT *,'DDY'
C     PRINT *,(DDY(N),N=1,JMXOUT)
C     PRINT *,'DDX'
C     PRINT *,(DDX(N),N=1,JMXOUT)
C
  111 CONTINUE
C
      SUM1=0.
      SUM2=0.
      WEI1=0.
      WEI2=0.
      DO I=1,IMXIN
        SUM1=SUM1+REGIN(I,1) * RSLMSK(I,1)
        SUM2=SUM2+REGIN(I,JMXIN) * RSLMSK(I,JMXIN)
        WEI1=WEI1+RSLMSK(I,1)
        WEI2=WEI2+RSLMSK(I,JMXIN)
      ENDDO
      IF(RLAT.GT.0.) THEN
        IF(WEI1.GT.0.) THEN
          SUMN=SUM1/WEI1
        ELSE
          SUMN=0.
        ENDIF
        IF(WEI2.GT.0.) THEN
          SUMS=SUM2/WEI2
        ELSE
          SUMS=0.
        ENDIF
      ELSE
        IF(WEI1.GT.0.) THEN
          SUMS=SUM1/WEI1
        ELSE
          SUMS=0.
        ENDIF
        IF(WEI2.GT.0.) THEN
          SUMN=SUM2/WEI2
        ELSE
          SUMN=0.
        ENDIF
      ENDIF
C
C  QUASI-BILINEAR INTERPOLATION
C
      IFILL=0
      DO 70 J=1,JMXOUT
      Y=DDY(J)
      J1=JINDX1(J)
      J2=JINDX2(J)
      DO 70 I=1,IMXOUT
      X=DDX(I)
      I1=IINDX1(I)
      I2=IINDX2(I)
C
      IF(LMASK) THEN
        IF(SLMASK(I,J).EQ.RSLMSK(I1,J1).AND.
     1     SLMASK(I,J).EQ.RSLMSK(I2,J1).AND.
     2     SLMASK(I,J).EQ.RSLMSK(I1,J2).AND.
     3     SLMASK(I,J).EQ.RSLMSK(I2,J2)) THEN
          WI1J1=(1.-X)*(1.-Y)  
          WI2J1=    X *(1.-Y) 
          WI1J2=(1.-X)*      Y 
          WI2J2=    X *      Y
        ELSEIF(SLMASK(I,J).EQ.1.) THEN
          WI1J1=(1.-X)*(1.-Y)  *RSLMSK(I1,J1)
          WI2J1=    X *(1.-Y)  *RSLMSK(I2,J1)
          WI1J2=(1.-X)*      Y *RSLMSK(I1,J2)
          WI2J2=    X *      Y *RSLMSK(I2,J2)
        ELSEIF(SLMASK(I,J).EQ.0.) THEN
          WI1J1=(1.-X)*(1.-Y)  *(1.-RSLMSK(I1,J1))
          WI2J1=    X *(1.-Y)  *(1.-RSLMSK(I2,J1))
          WI1J2=(1.-X)*      Y *(1.-RSLMSK(I1,J2))
          WI2J2=    X *      Y *(1.-RSLMSK(I2,J2))
        ENDIF
      ELSE
        WI1J1=(1.-X)*(1.-Y)  
        WI2J1=    X *(1.-Y) 
        WI1J2=(1.-X)*      Y 
        WI2J2=    X *      Y
      ENDIF
C
      WSUM  =WI1J1+WI2J1+WI1J2+WI2J2
      IF(WSUM.NE.0.) THEN
        WSUMIV = 1./WSUM
C
        IF(J1.NE.J2) THEN
            GAUOUT(I,J)=(WI1J1*REGIN(I1,J1)+WI2J1*REGIN(I2,J1)+
     1                   WI1J2*REGIN(I1,J2)+WI2J2*REGIN(I2,J2))*WSUMIV
        ELSE
           IF(J1.EQ.1.AND.RLAT.GT.0..OR.J1.EQ.JMXIN.AND.RLAT.LT.0.) THEN
            GAUOUT(I,J)=(WI1J1*SUMN        +WI2J1*SUMN        +
     1                   WI1J2*REGIN(I1,J2)+WI2J2*REGIN(I2,J2))*WSUMIV
           ENDIF
           IF(J1.EQ.1.AND.RLAT.LT.0..OR.J1.EQ.JMXIN.AND.RLAT.GT.0.) THEN
            GAUOUT(I,J)=(WI1J1*REGIN(I1,J1)+WI2J1*REGIN(I2,J1)+
     1                   WI1J2*SUMS        +WI2J2*SUMS        )*WSUMIV
           ENDIF
        ENDIF
      ELSE
        IF(.NOT.LMASK) THEN
          WRITE(6,*) ' LA2GA called with LMASK=.true. but bad',
     1               ' RSLMSK or SLMASK given'
          CALL ABORT
        ENDIF
        IFILL=IFILL+1
        IF(IFILL.LE.2) THEN
          WRITE(6,*) 'I1,I2,J1,J2=',I1,I2,J1,J2
          WRITE(6,*) 'RSLMSK=',RSLMSK(I1,J1),RSLMSK(I1,J2),
     1                         RSLMSK(I2,J1),RSLMSK(I2,J2)
          WRITE(6,*) 'I,J=',I,J,' SLMASK(I,J)=',SLMASK(I,J)
        ENDIF
        DO JX=J1,JMXIN
          DO IX=I1,IMXIN
            IF((SLMASK(I,J).EQ.1..AND.
     1          SLMASK(I,J).EQ.RSLMSK(IX,JX)).OR.
     2         (SLMASK(I,J).EQ.0..AND.
     3          SLMASK(I,J).EQ.RSLMSK(IX,JX))) THEN
              GAUOUT(I,J)=REGIN(IX,JX)
              GO TO 71
            ENDIF
          ENDDO
          DO IX=I1,1,-1
            IF((SLMASK(I,J).EQ.1..AND.
     1          SLMASK(I,J).EQ.RSLMSK(IX,JX)).OR.
     2         (SLMASK(I,J).EQ.0..AND.
     3          SLMASK(I,J).EQ.RSLMSK(IX,JX))) THEN
              GAUOUT(I,J)=REGIN(IX,JX)
              GO TO 71
            ENDIF
          ENDDO
        ENDDO
        DO JX=J1,1,-1
          DO IX=I1,IMXIN
            IF((SLMASK(I,J).EQ.1..AND.
     1          SLMASK(I,J).EQ.RSLMSK(IX,JX)).OR.
     2         (SLMASK(I,J).EQ.0..AND.
     3          SLMASK(I,J).EQ.RSLMSK(IX,JX))) THEN
              GAUOUT(I,J)=REGIN(IX,JX)
              GO TO 71
            ENDIF
          ENDDO
          DO IX=I1,1,-1
            IF((SLMASK(I,J).EQ.1..AND.
     1          SLMASK(I,J).EQ.RSLMSK(IX,JX)).OR.
     2         (SLMASK(I,J).EQ.0..AND.
     3          SLMASK(I,J).EQ.RSLMSK(IX,JX))) THEN
              GAUOUT(I,J)=REGIN(IX,JX)
              GO TO 71
            ENDIF
          ENDDO
        ENDDO
        WRITE(6,*) ' ERROR!!! No filling value found in LA2GA'
        CALL ABORT
      ENDIF
C
   71 CONTINUE
   70 CONTINUE
C
      IF(IFILL.GT.1) THEN
        WRITE(6,*) ' Unable to interpolate.  Filled with nearest',
     1             ' point value at ',IFILL,' points'
      ENDIF
C
      CALL MAXMIN(GAUOUT,IMXOUT,JMXOUT,1)
C
      RETURN
      END
      SUBROUTINE MAXMIN(F,IMAX,JMAX,KMAX)
C
      DIMENSION F(IMAX,JMAX,KMAX)
C
      DO 10 K=1,KMAX
C
      FMAX=F(1,1,K)
      FMIN=F(1,1,K)
C
      DO 20 J=1,JMAX
      DO 20 I=1,IMAX
      IF(FMAX.LE.F(I,J,K)) THEN
      FMAX=F(I,J,K)
      IIMAX=I
      JJMAX=J
      ENDIF
      IF(FMIN.GE.F(I,J,K)) THEN
      FMIN=F(I,J,K)
      IIMIN=I
      JJMIN=J
      ENDIF
   20 CONTINUE
C
      WRITE(6,100) K,FMAX,IIMAX,JJMAX,FMIN,IIMIN,JJMIN
  100 FORMAT(2X,'LEVEL=',I2,' MAX=',E10.4,' AT I=',I5,' J=',I5,
     1                      ' MIN=',E10.4,' AT I=',I5,' J=',I5)
C
   10 CONTINUE
C
      RETURN
      END
      SUBROUTINE BGSRD(LUGI,FNBGSI,IDIM,JDIM,IJDIM,LSOIL,
     1                 TSFFCS,WETFCS,SNOFCS,ZORFCS,ALBFCS,AISFCS,
     2                 TG3FCS,PLRFCS,CVFCS ,CVBFCS,CVTFCS,
     3                 CNPFCS,SMCFCS,STCFCS,SLIFCS,F10M,LABEL)
C
      CHARACTER*80 FNBGSI
      CHARACTER*32 LABEL
      DIMENSION IDATE(4)
      integer*4 IDATE4(4)
C
      DIMENSION TSFFCS(IJDIM),WETFCS(IJDIM),SNOFCS(IJDIM),
     1          ZORFCS(IJDIM),ALBFCS(IJDIM),AISFCS(IJDIM),
     2          TG3FCS(IJDIM),PLRFCS(IJDIM),
     3          CVFCS (IJDIM),CVBFCS(IJDIM),CVTFCS(IJDIM),
     4          CNPFCS(IJDIM),
     5          SMCFCS(IJDIM,LSOIL),STCFCS(IJDIM,LSOIL),
     6          SLIFCS(IJDIM)
      DIMENSION F10M  (IJDIM)
C
      CHARACTER*80 ASGNSTR
C
c     IF(LUGI.LT.10) THEN
c       WRITE(ASGNSTR,'(29Hassign -Fcos -Cascii -Nibm u:,I1,)') LUGI
c     ELSE
c       WRITE(ASGNSTR,'(29Hassign -Fcos -Cascii -Nibm u:,I2,)') LUGI
c     ENDIF
c     CALL ASSIGN(ASGNSTR)
      OPEN(UNIT=LUGI,FILE=FNBGSI,FORM='UNFORMATTED',ERR=920)
      GO TO 921
  920 CONTINUE
      WRITE(6,*) ' ERROR IN OPENING FILE ',FNBGSI(1:50)
      CALL ABORT
  921 CONTINUE
      WRITE(6,*) ' FILE ',FNBGSI(1:50),' opened. Unit=',LUGI
      READ(LUGI,END=1010) LABEL
      GO TO 1020
 1010 CONTINUE
      WRITE(6,*) ' EOF on BGES INPUT file.'
      CALL ABORT
 1020 CONTINUE
c     READ(LUGI) FH,IDATE
      READ(LUGI) FH,idate4
      idate=idate4
      WRITE(6,*) ' BGUESS INPUT DATE ',FH,
     1           IDATE(4),IDATE(2),IDATE(3),IDATE(1)
      READ (LUGI) TSFFCS
      READ (LUGI,ERR=1100) SMCFCS
      READ (LUGI) SNOFCS
      READ (LUGI) STCFCS
      READ (LUGI) TG3FCS
      READ (LUGI) ZORFCS
      READ (LUGI) CVFCS
      READ (LUGI) CVBFCS
      READ (LUGI) CVTFCS
      READ (LUGI) ALBFCS
      READ (LUGI) SLIFCS
      F10M=0.
      CNPFCS=0.
      READ (LUGI,END=1200) PLRFCS
      READ (LUGI,END=1200) CNPFCS
      READ (LUGI,END=1200) F10M
      GOTO 1200
C
C  Read bucket model bges
C
 1100 CONTINUE
      WRITE(6,*) ' ENCOUNTERED BUCKET MODEL BGES FIELD'
      READ (LUGI) SNOFCS
      READ (LUGI) (STCFCS(IJ,1),IJ=1,IJDIM)
      READ (LUGI) (STCFCS(IJ,2),IJ=1,IJDIM)
      READ (LUGI) TG3FCS
      READ (LUGI) ZORFCS
      READ (LUGI) CVFCS
      READ (LUGI) CVBFCS
      READ (LUGI) CVTFCS
      READ (LUGI) ALBFCS
      READ (LUGI) SLIFCS
      F10M=0.
      CNPFCS=0.
      READ (LUGI,END=1200) PLRFCS
      READ (LUGI,END=1200) F10M
      CALL BKTGES(SMCFCS,SLIANL,STCFCS,IJDIM,LSOIL)
 1200 CONTINUE
C
      DO IJ=1,IJDIM
        WETFCS(IJ)=0.
        AISFCS(IJ)=0.
        IF(SLIFCS(IJ).EQ.2) AISFCS(IJ)=1.
      ENDDO
C
      RETURN
      END
      SUBROUTINE PEREX(LUGI,LUGB,IDIM,JDIM,IJDIM,SLMASK,IY,IM,ID,IH,FH,
     1                 FNGLAC,FNMXIC,FIGLAC,FIMXIC,
     1                 KPDGLA,KPDMXI,GLACIR,AMXICE)
C
      DIMENSION SLMASK(IJDIM)
C
      CHARACTER*80 FNGLAC,FNMXIC
      CHARACTER*80 FIGLAC,FIMXIC
      DIMENSION GLACIR(IJDIM),AMXICE(IJDIM)
C
      LOGICAL LCLIM
C
      LCLIM=.true.
C
C  Glacier
C
      CALL FIXRD(LUGI,LUGB,FNGLAC,FIGLAC,KPDGLA,LCLIM,SLMASK,
     1           IY,IM,ID,IH,FH,GLACIR,IDIM,JDIM,IRET)
C
C  Maximum ice extent
C
      CALL FIXRD(LUGI,LUGB,FNMXIC,FIMXIC,KPDMXI,LCLIM,SLMASK,
     1           IY,IM,ID,IH,FH,AMXICE,IDIM,JDIM,IRET)
C
      RETURN
      END
      SUBROUTINE CLIMA(LUGI,LUGB,IY,IM,ID,IH,FH,IDIM,JDIM,IJDIM,LSOIL,
     Z                 SLMASK,FNTSFC,FNWETC,FNSNOC,FNZORC,FNALBC,FNAISC,
     1                 FNPLRC,FNTG3C,FNSCVC,FNSMCC,FNSTCC,FNACNC,
     2                 FITSFC,FIWETC,FISNOC,FIZORC,FIALBC,FIAISC,
     3                 FIPLRC,FITG3C,FISCVC,FISMCC,FISTCC,FIACNC,
     4                 TSFCLM,WETCLM,SNOCLM,ZORCLM,ALBCLM,AISCLM,
     5                 TG3CLM,PLRCLM,CVCLM ,CVBCLM,CVTCLM,
     6                 CNPCLM,SMCCLM,STCCLM,SLICLM,SCVCLM,ACNCLM,
     7                 KPDTSF,KPDWET,KPDSNO,KPDZOR,KPDALB,KPDAIS,
     8                 KPDTG3,KPDPLR,KPDSCV,KPDACN,TSFCL0)
C
      DIMENSION SLMASK(IJDIM)
C
      CHARACTER*80 FNTSFC,FNWETC,FNSNOC,FNZORC,FNALBC,FNAISC,
     1             FNPLRC,FNTG3C,FNSCVC,FNSMCC,FNSTCC,FNACNC
      CHARACTER*80 FITSFC,FIWETC,FISNOC,FIZORC,FIALBC,FIAISC,
     1             FIPLRC,FITG3C,FISCVC,FISMCC,FISTCC,FIACNC
      DIMENSION TSFCLM(IJDIM),WETCLM(IJDIM),SNOCLM(IJDIM),
     1          ZORCLM(IJDIM),ALBCLM(IJDIM),AISCLM(IJDIM),
     2          TG3CLM(IJDIM),PLRCLM(IJDIM),ACNCLM(IJDIM),
     3          CVCLM (IJDIM),CVBCLM(IJDIM),CVTCLM(IJDIM),
     4          CNPCLM(IJDIM),
     5          SMCCLM(IJDIM,LSOIL),STCCLM(IJDIM,LSOIL),
     6          SLICLM(IJDIM),SCVCLM(IJDIM)
C
      DIMENSION TSFCL0(IJDIM) 
C
      LOGICAL LCLIM
C
      LCLIM=.true.
C
C  START READING IN CLIMATOLOGY AND INTERPOLATE TO THE DATE
C
C  TSF
C
      CALL FIXRD(LUGI,LUGB,FNTSFC,FITSFC,KPDTSF,LCLIM,SLMASK,
     1           IY,IM,ID,IH,FH,TSFCLM,IDIM,JDIM,IRET)
C
C  ALBEDO
C
      CALL FIXRD(LUGI,LUGB,FNALBC,FIALBC,KPDALB,LCLIM,SLMASK,
     1           IY,IM,ID,IH,FH,ALBCLM,IDIM,JDIM,IRET)
C
C  SOIL WETNESS
C
      IF(FNWETC(1:8).NE.'        ') THEN
        CALL FIXRD(LUGI,LUGB,FNWETC,FIWETC,KPDWET,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,WETCLM,IDIM,JDIM,IRET)
      ELSEIF(FNSMCC(1:8).NE.'        ') THEN
        CALL FIXRD(LUGI,LUGB,FNSMCC,FISMCC,KPDSMC,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,SMCCLM(1,1),IDIM,JDIM,IRET)
        CALL FIXRD(LUGI,LUGB,FNSMCC,FISMCC,KPDSMC,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,SMCCLM(1,2),IDIM,JDIM,IRET)
      ELSE
        WRITE(6,*) 'Climatological Soil wetness file not given'
        CALL ABORT
      ENDIF
C
C  SEA ICE
C
      IF(FNACNC(1:8).NE.'        ') THEN
        CALL FIXRD(LUGI,LUGB,FNACNC,FIACNC,KPDACN,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,ACNCLM,IDIM,JDIM,IRET)
      ELSEIF(FNAISC(1:8).NE.'        ') THEN
        CALL FIXRD(LUGI,LUGB,FNAISC,FIAISC,KPDAIS,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,AISCLM,IDIM,JDIM,IRET)
      ELSE
        WRITE(6,*) 'Climatological ice cover file not given'
        CALL ABORT
      ENDIF
C
C  SNOW DEPTH
C
      CALL FIXRD(LUGI,LUGB,FNSNOC,FISNOC,KPDSNO,LCLIM,SLMASK,
     1           IY,IM,ID,IH,FH,SNOCLM,IDIM,JDIM,IRET)
C
C  SNOW COVER
C
      IF(FNSCVC(1:8).NE.'        ') THEN
        CALL FIXRD(LUGI,LUGB,FNSCVC,FISCVC,KPDSCV,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,SCVCLM,IDIM,JDIM,IRET)
        WRITE(6,*) 'Climatological snow cover read in.'
      ENDIF
C
C  SURFACE ROUGHNESS
C
      CALL FIXRD(LUGI,LUGB,FNZORC,FIZORC,KPDZOR,LCLIM,SLMASK,
     1           IY,IM,ID,IH,FH,ZORCLM,IDIM,JDIM,IRET)
C
C  MINIMUM STOMATAL RESISTANCE
C
      CALL FIXRD(LUGI,LUGB,FNPLRC,FIPLRC,KPDPLR,LCLIM,SLMASK,
     1           IY,IM,ID,IH,FH,PLRCLM,IDIM,JDIM,IRET)
C
C  Set clouds climatology to zero
C
      DO IJ = 1, IJDIM
        CVCLM (IJ) = 0.
        CVBCLM(IJ) = 0.
        CVTCLM(IJ) = 0.
      ENDDO
C
C  Set canopy water content climatology to zero
C
      DO IJ = 1, IJDIM
        CNPCLM(IJ) = 0.
      ENDDO
C
C  Deep Soil Temperature
C
      IF(FNTG3C(1:8).NE.'        ') THEN
        CALL FIXRD(LUGI,LUGB,FNTG3C,FITG3C,KPDTG3,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,TG3CLM,IDIM,JDIM,IRET)
C
C  Layer Soil temperature
C
      ELSEIF(FNSTCC(1:8).NE.'        ') THEN
        CALL FIXRD(LUGI,LUGB,FNSTCC,FISTCC,KPDSTC,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,STCCLM(1,1),IDIM,JDIM,IRET)
        CALL FIXRD(LUGI,LUGB,FNSTCC,FISTCC,KPDSTC,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,STCCLM(1,2),IDIM,JDIM,IRET)
      ELSE
        WRITE(6,*) 'Climatological Soil temp file not given'
        CALL ABORT
      ENDIF
C
C  TSF at forecast hour=0.
C
      CALL FIXRD(LUGI,LUGB,FNTSFC,FITSFC,KPDTSF,LCLIM,SLMASK,
     1           IY,IM,ID,IH,0.,TSFCL0,IDIM,JDIM,IRET)
C
C  END OF CLIMATOLOGY READS
C
      RETURN
      END
      SUBROUTINE FILANL(TSFANL,WETANL,SNOANL,ZORANL,ALBANL,AISANL,
     1                  TG3ANL,PLRANL,CVANL ,CVBANL,CVTANL,
     2                  CNPANL,SMCANL,STCANL,SLIANL,SCVANL,
     3                  TSFCLM,WETCLM,SNOCLM,ZORCLM,ALBCLM,AISCLM,
     4                  TG3CLM,PLRCLM,CVCLM ,CVBCLM,CVTCLM,
Cfix 5                  CNPCLM,SMCCLM,STCCLM,ALICLM,SCVCLM,
     5                  CNPCLM,SMCCLM,STCCLM,SLICLM,SCVCLM,
     6                  IJDIM,LSOIL)
C
      DIMENSION TSFANL(IJDIM),WETANL(IJDIM),SNOANL(IJDIM),
     1          ZORANL(IJDIM),ALBANL(IJDIM),AISANL(IJDIM),
     2          TG3ANL(IJDIM),PLRANL(IJDIM),
     3          CVANL (IJDIM),CVBANL(IJDIM),CVTANL(IJDIM),
     4          CNPANL(IJDIM),
     5          SMCANL(IJDIM,LSOIL),STCANL(IJDIM,LSOIL),
     6          SLIANL(IJDIM),SCVANL(IJDIM)
      DIMENSION TSFCLM(IJDIM),WETCLM(IJDIM),SNOCLM(IJDIM),
     1          ZORCLM(IJDIM),ALBCLM(IJDIM),AISCLM(IJDIM),
     2          TG3CLM(IJDIM),PLRCLM(IJDIM),
     3          CVCLM (IJDIM),CVBCLM(IJDIM),CVTCLM(IJDIM),
     4          CNPCLM(IJDIM),
     5          SMCCLM(IJDIM,LSOIL),STCCLM(IJDIM,LSOIL),
     6          SLICLM(IJDIM),SCVCLM(IJDIM)
C
C  Tsf
C
      DO IJ=1,IJDIM
        TSFANL(IJ)=TSFCLM(IJ)
      ENDDO
C
C  Albedo
C
      DO IJ=1,IJDIM
        ALBANL(IJ)=ALBCLM(IJ)
      ENDDO
C
C  Soil Wetness
C
      DO IJ=1,IJDIM
        WETANL(IJ)=WETCLM(IJ)
      ENDDO
C
C  Layer soil wetness
C
      DO K=1,LSOIL
        DO IJ=1,IJDIM
          SMCANL(IJ,K)=SMCCLM(IJ,K)
        ENDDO
      ENDDO
C
C  SNOW
C
      DO IJ=1,IJDIM
        SNOANL(IJ)=SNOCLM(IJ)
      ENDDO
C
C  SNOW COVER
C
      DO IJ=1,IJDIM
        SCVANL(IJ)=SCVCLM(IJ)
      ENDDO
C
C  SEAICE
C
      DO IJ=1,IJDIM
        AISANL(IJ)=AISCLM(IJ)
      ENDDO
C
C  LAND/SEA/SNOW mask
C
      DO IJ=1,IJDIM
        SLIANL(IJ)=SLICLM(IJ)
      ENDDO
C
C  Surface roughness
C
      DO IJ=1,IJDIM
        ZORANL(IJ)=ZORCLM(IJ)
      ENDDO
C
C  Maximum stomatal resistance
C
      DO IJ=1,IJDIM
        PLRANL(IJ)=PLRCLM(IJ)
      ENDDO
C
C  Cloud Cover
C
      DO IJ=1,IJDIM
        CVANL(IJ)=0.
        CVBANL(IJ)=0.
        CVTANL(IJ)=0.
      ENDDO
C
C  Deep soil temperature
C
      DO IJ=1,IJDIM
        TG3ANL(IJ)=TG3CLM(IJ)
      ENDDO
C
C  Soil temperature
C
      DO K=1,LSOIL
        DO IJ=1,IJDIM
          STCANL(IJ,K)=STCCLM(IJ,K)
        ENDDO
      ENDDO
C
C  Canopy water content
C
      DO IJ = 1, IJDIM
        CNPANL(IJ)=CNPCLM(IJ)
      ENDDO
C
      RETURN
      END
      SUBROUTINE ANALY(LUGI,LUGB,IY,IM,ID,IH,FH,IDIM,JDIM,IJDIM,LSOIL,
     Z                 SLMASK,FNTSFA,FNWETA,FNSNOA,FNZORA,FNALBA,FNAISA,
     1                 FNPLRA,FNTG3A,FNSCVA,FNSMCA,FNSTCA,FNACNA,
     2                 FITSFA,FIWETA,FISNOA,FIZORA,FIALBA,FIAISA,
     3                 FIPLRA,FITG3A,FISCVA,FISMCA,FISTCA,FIACNA,
     4                 TSFANL,WETANL,SNOANL,ZORANL,ALBANL,AISANL,
     5                 TG3ANL,PLRANL,CVANL ,CVBANL,CVTANL,
     6                 SMCANL,STCANL,SLIANL,SCVANL,ACNANL,
     7                 KPDTSF,KPDWET,KPDSNO,KPDZOR,KPDALB,KPDAIS,
     8                 KPDTG3,KPDPLR,KPDSCV,KPDACN,KPDSMC,KPDSTC,
     7                 IRTTSF,IRTWET,IRTSNO,IRTZOR,IRTALB,IRTAIS,
     8                 IRTTG3,IRTPLR,IRTSCV,IRTACN,IRTSMC,IRTSTC)
C
      DIMENSION SLMASK(IJDIM)
C
      CHARACTER*80 FNTSFA,FNWETA,FNSNOA,FNZORA,FNALBA,FNAISA,
     1             FNPLRA,FNTG3A,FNSCVA,FNSMCA,FNSTCA,FNACNA 
      CHARACTER*80 FITSFA,FIWETA,FISNOA,FIZORA,FIALBA,FIAISA,
     1             FIPLRA,FITG3A,FISCVA,FISMCA,FISTCA,FIACNA
      DIMENSION TSFANL(IJDIM),WETANL(IJDIM),SNOANL(IJDIM),
     1          ZORANL(IJDIM),ALBANL(IJDIM),AISANL(IJDIM),
     2          TG3ANL(IJDIM),PLRANL(IJDIM),ACNANL(IJDIM),
     3          CVANL (IJDIM),CVBANL(IJDIM),CVTANL(IJDIM),
     5          SMCANL(IJDIM,LSOIL),STCANL(IJDIM,LSOIL),
     6          SLIANL(IJDIM),SCVANL(IJDIM)
C
      LOGICAL LCLIM
C
      LCLIM=.false.
C
C TSF
C
      IRTTSF=0
      IF(FNTSFA(1:8).NE.'        ') THEN
        CALL FIXRD(LUGI,LUGB,FNTSFA,FITSFA,KPDTSF,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,TSFANL,IDIM,JDIM,IRET)
        IRTTSF=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'T SURFACE ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          PRINT *,'OLD T SURFACE ANALYSIS PROVIDED, Indicating proper',
     1            ' file name is given.  No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
        ELSE
          PRINT *,'T SURFACE ANALYSIS PROVIDED.'
        ENDIF
      ELSE
        PRINT *,'************************************************'
        PRINT *,'NO TSF ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
        PRINT *,'NO TSF ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
        PRINT *,'NO TSF ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
      ENDIF
C
C  ALBEDO
C
      IRTALB=0
      IF(FNALBA(1:8).NE.'        ') THEN
        CALL FIXRD(LUGI,LUGB,FNALBA,FIALBA,KPDALB,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,ALBANL,IDIM,JDIM,IRET)
        IRTALB=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'ALBEDO ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          PRINT *,'OLD ALBEDO ANALYSIS PROVIDED, Indicating proper',
     1            ' file name is given.  No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
        ELSE
          PRINT *,'ALBEDO ANALYSIS PROVIDED.'
        ENDIF
      ELSE
        PRINT *,'************************************************'
        PRINT *,'NO ALBEDO ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
        PRINT *,'NO ALBEDO ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
        PRINT *,'NO ALBEDO ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
      ENDIF
C
C  Soil Wetness
C
      IRTWET=0
      IRTSMC=0
      IF(FNWETA(1:8).NE.'        ') THEN
        CALL FIXRD(LUGI,LUGB,FNWETA,FIWETA,KPDWET,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,WETANL,IDIM,JDIM,IRET)
        IRTWET=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'BUCKET WETNESS ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          PRINT *,'OLD WETNESS ANALYSIS PROVIDED, Indicating proper',
     1            ' file name is given.  No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
        ELSE
          PRINT *,'BUCKET WETNESS ANALYSIS PROVIDED.'
        ENDIF
      ELSEIF(FNSMCA(1:8).NE.'        ') THEN
        CALL FIXRD(LUGI,LUGB,FNSMCA,FISMCA,KPDSMC,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,SMCANL(1,1),IDIM,JDIM,IRET)
        CALL FIXRD(LUGI,LUGB,FNSMCA,FISMCA,KPDSMC,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,SMCANL(1,2),IDIM,JDIM,IRET)
        IRTSMC=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'LAYER SOIL WETNESS ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          PRINT *,'OLD LAYER SOIL WETNESS ANALYSIS PROVIDED',
     1            ' Indicating proper file name is given.'
          PRINT *,' No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
        ELSE
          PRINT *,'LAYER SOIL WETNESS ANALYSIS PROVIDED.'
        ENDIF
      ELSE
        PRINT *,'************************************************'
        PRINT *,'NO SOIL WETNESS ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
        PRINT *,'NO SOIL WETNESS ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
        PRINT *,'NO SOIL WETNESS ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
      ENDIF
C
C  READ IN SNOW DEPTH/SNOW COVER
C
      IRTSCV=0
      IF(FNSNOA(1:8).NE.'        ') THEN
        SCVANL=0.
        CALL FIXRD(LUGI,LUGB,FNSNOA,FISNOA,KPDSNO,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,SNOANL,IDIM,JDIM,IRET)
        IRTSCV=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'SNOW DEPTH ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          PRINT *,'OLD SNOW DEPTH ANALYSIS PROVIDED, Indicating proper',
     1            ' file name is given.  No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
        ELSE
          PRINT *,'SNOW DEPTH ANALYSIS PROVIDED.'
        ENDIF
        IRTSNO=0
      ELSEIF(FNSCVA(1:8).NE.'        ') THEN
        SNOANL=0.
        CALL FIXRD(LUGI,LUGB,FNSCVA,FISCVA,KPDSCV,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,SCVANL,IDIM,JDIM,IRET)
        IRTSNO=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'SNOW COVER ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          PRINT *,'OLD SNOW COVER ANALYSIS PROVIDED, Indicating proper',
     1            ' file name is given.  No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
        ELSE
          PRINT *,'SNOW COVER ANALYSIS PROVIDED.'
        ENDIF
      ELSE
        PRINT *,'************************************************'
        PRINT *,'NO SNOW/SNOCOV ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
        PRINT *,'NO SNOW/SNOCOV ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
        PRINT *,'NO SNOW/SNOCOV ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
      ENDIF
C
C  Sea ice mask
C
      IRTACN=0
      IRTAIS=0
      IF(FNACNA(1:8).NE.'        ') THEN
        CALL FIXRD(LUGI,LUGB,FNACNA,FIACNA,KPDACN,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,ACNANL,IDIM,JDIM,IRET)
        IRTACN=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'ICE CONCENTRATION ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          PRINT *,'OLD ICE CONCENTRATION ANALYSIS PROVIDED',
     1            ' Indicating proper file name is given' 
          PRINT *,' No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
        ELSE
          PRINT *,'ICE CONCENTRATION ANALYSIS PROVIDED.'
        ENDIF
      ELSEIF(FNAISA(1:8).NE.'        ') THEN
        CALL FIXRD(LUGI,LUGB,FNAISA,FIAISA,KPDAIS,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,AISANL,IDIM,JDIM,IRET)
        IRTAIS=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'ICE MASK ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          PRINT *,'OLD ICE-MASK ANALYSIS PROVIDED, Indicating proper',
     1            ' file name is given.  No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
        ELSE
          PRINT *,'ICE MASK ANALYSIS PROVIDED.'
        ENDIF
      ELSE
        PRINT *,'************************************************'
        PRINT *,'NO SEA-ICE ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
        PRINT *,'NO SEA-ICE ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
        PRINT *,'NO SEA-ICE ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
      ENDIF
C
C  Surface Roughness
C
      IRTZOR=0
      IF(FNZORA(1:8).NE.'        ') THEN
        CALL FIXRD(LUGI,LUGB,FNZORA,FIZORA,KPDZOR,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,ZORANL,IDIM,JDIM,IRET)
        IRTZOR=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'ROUGHNESS ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          PRINT *,'OLD ROUGHNESS ANALYSIS PROVIDED, Indicating proper',
     1            ' file name is given.  No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
        ELSE
          PRINT *,'ROUGHNESS ANALYSIS PROVIDED.'
        ENDIF
      ELSE
        PRINT *,'************************************************'
        PRINT *,'NO SRFC ROUGHNESS ANALYSIS AVAILABLE. CLIMATOLOGY USED'
        PRINT *,'NO SRFC ROUGHNESS ANALYSIS AVAILABLE. CLIMATOLOGY USED'
        PRINT *,'NO SRFC ROUGHNESS ANALYSIS AVAILABLE. CLIMATOLOGY USED'
      ENDIF
C
C  MAXIMUM STOMATAL RESISTANCE
C
      IRTPLR=0
      IF(FNPLRA(1:8).NE.'        ') THEN
        CALL FIXRD(LUGI,LUGB,FNPLRA,FIPLRA,KPDPLR,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,PLRANL,IDIM,JDIM,IRET)
        IRTPLR=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'PLANT RESISTNCE ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          PRINT *,'OLD PLANT RESISTANCE ANALYSIS PROVIDED',
     1            ' Indicating proper file name is given.'
          PRINT *,' No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
        ELSE
          PRINT *,'PLANT RESISTNCE ANALYSIS PROVIDED.'
        ENDIF
      ELSE
        PRINT *,'************************************************'
        PRINT *,'NO PLANT RSISTNC ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
        PRINT *,'NO PLANT RSISTNC ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
        PRINT *,'NO PLANT RSISTNC ANALYSIS AVAILABLE.  CLIMATOLOGY USED'
      ENDIF
C
C  Deep Soil Temperature
C
      IRTTG3=0
      IRTSTC=0
      IF(FNTG3A(1:8).NE.'        ') THEN
        CALL FIXRD(LUGI,LUGB,FNTG3A,FITG3A,KPDTG3,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,TG3ANL,IDIM,JDIM,IRET)
        IRTTG3=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'DEEP SOIL TMP ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          PRINT *,'OLD DEEP SOIL TEMP ANALYSIS PROVIDED',
     1            ' Indicating proper file name is given.'
          PRINT *,' No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
        ELSE
          PRINT *,'DEEP SOIL TMP ANALYSIS PROVIDED.'
        ENDIF
      ELSEIF(FNSTCA(1:8).NE.'        ') THEN
        CALL FIXRD(LUGI,LUGB,FNSTCA,FISTCA,KPDSTC,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,STCANL(1,1),IDIM,JDIM,IRET)
        CALL FIXRD(LUGI,LUGB,FNSTCA,FISTCA,KPDSTC,LCLIM,SLMASK,
     1             IY,IM,ID,IH,FH,STCANL(1,2),IDIM,JDIM,IRET)
        IRTSTC=IRET
        IF(IRET.EQ.1) THEN
          WRITE(6,*) 'LAYER SOIL TMP ANALYSIS READ ERROR'
          CALL ABORT
        ELSEIF(IRET.EQ.-1) THEN
          PRINT *,'OLD DEEP SOIL TEMP ANALYSIS PROVIDED', 
     1            'iIndicating proper file name is given.'
          PRINT *,' No error suspected.'
          WRITE(6,*) 'FORECAST GUESS WILL BE USED'
        ELSE
          PRINT *,'LAYER SOIL TMP ANALYSIS PROVIDED.'
        ENDIF
      ELSE
        PRINT *,'************************************************'
        PRINT *,'NO DEEP SOIL TEMP ANALY AVAILABLE.  CLIMATOLOGY USED'
        PRINT *,'NO DEEP SOIL TEMP ANALY AVAILABLE.  CLIMATOLOGY USED'
        PRINT *,'NO DEEP SOIL TEMP ANALY AVAILABLE.  CLIMATOLOGY USED'
      ENDIF
C
      RETURN
      END
      SUBROUTINE FILFCS(TSFFCS,WETFCS,SNOFCS,ZORFCS,ALBFCS,
     1                  TG3FCS,PLRFCS,CVFCS ,CVBFCS,CVTFCS,
     2                  CNPFCS,SMCFCS,STCFCS,SLIFCS,AISFCS,
     3                  TSFANL,WETANL,SNOANL,ZORANL,ALBANL,
     4                  TG3ANL,PLRANL,CVANL ,CVBANL,CVTANL,
     5                  CNPANL,SMCANL,STCANL,SLIANL,AISANL,
     6                  IJDIM,LSOIL)
C
      DIMENSION TSFFCS(IJDIM),WETFCS(IJDIM),SNOFCS(IJDIM),
     1          ZORFCS(IJDIM),ALBFCS(IJDIM),AISFCS(IJDIM),
     2          TG3FCS(IJDIM),PLRFCS(IJDIM),
     3          CVFCS (IJDIM),CVBFCS(IJDIM),CVTFCS(IJDIM),
     4          CNPFCS(IJDIM),
     5          SMCFCS(IJDIM,LSOIL),STCFCS(IJDIM,LSOIL),
     6          SLIFCS(IJDIM)
      DIMENSION TSFANL(IJDIM),WETANL(IJDIM),SNOANL(IJDIM),
     1          ZORANL(IJDIM),ALBANL(IJDIM),AISANL(IJDIM),
     2          TG3ANL(IJDIM),PLRANL(IJDIM),
     3          CVANL (IJDIM),CVBANL(IJDIM),CVTANL(IJDIM),
     4          CNPANL(IJDIM),
     5          SMCANL(IJDIM,LSOIL),STCANL(IJDIM,LSOIL),
     6          SLIANL(IJDIM)
C
      WRITE(6,*) '  THIS IS A DEAD START RUN, TSFC OVER LAND IS',
     &           ' SET AS LOWEST SIGMA LEVEL TEMPERTURE IF GIVEN.'
      WRITE(6,*) '  IF NOT, SET TO CLIMATOLOGICAL TSF OVER LAND IS USED'
C
C  Tsf
C
      DO IJ=1,IJDIM
        TSFFCS(IJ)=TSFANL(IJ)
      ENDDO
C
C  Albedo
C
      DO IJ=1,IJDIM
        ALBFCS(IJ)=ALBANL(IJ)
      ENDDO
C
C  Soil Wetness
C
      DO IJ=1,IJDIM
        WETFCS(IJ)=WETANL(IJ)
      ENDDO
C
C  Layer soil wetness
C
      DO K=1,LSOIL
        DO IJ=1,IJDIM
          SMCFCS(IJ,K)=SMCANL(IJ,K)
        ENDDO
      ENDDO
C
C  SNOW
C
      DO IJ=1,IJDIM
        SNOFCS(IJ)=SNOANL(IJ)
      ENDDO
C
C  SEAICE
C
      DO IJ=1,IJDIM
        AISFCS(IJ)=AISANL(IJ)
      ENDDO
C
C  LAND/SEA/SNOW mask
C
      DO IJ=1,IJDIM
        SLIFCS(IJ)=SLIANL(IJ)
      ENDDO
C
C  Surface roughness
C
      DO IJ=1,IJDIM
        ZORFCS(IJ)=ZORANL(IJ)
      ENDDO
C
C  Maximum stomatal resistance
C
      DO IJ=1,IJDIM
        PLRFCS(IJ)=PLRANL(IJ)
      ENDDO
C
C  Deep soil temperature
C
      DO IJ=1,IJDIM
        TG3FCS(IJ)=TG3ANL(IJ)
      ENDDO
C
C  Layer soil temperature
C
      DO K=1,LSOIL
        DO IJ=1,IJDIM
          STCFCS(IJ,K)=STCANL(IJ,K)
        ENDDO
      ENDDO
C
C  Canopy water content
C
      DO IJ = 1, IJDIM
        CNPFCS(IJ)=CNPANL(IJ)
      ENDDO
C
      RETURN
      END
      SUBROUTINE BKTGES(SMCFCS,SLIANL,STCFCS,IJDIM,LSOIL)
C
      DIMENSION SMCFCS(IJDIM,LSOIL),STCFCS(IJDIM,LSOIL)
      DIMENSION SLIANL(IJDIM)
C
C  Note that SMFCS comes in with the original unit (cm?) (not GRIB file)
C
      DO IJ = 1, IJDIM
        SMCFCS(IJ,1) = (SMCFCS(IJ,1)/150.) * .37 + .1
      ENDDO
      DO K = 2, LSOIL
        DO IJ = 1, IJDIM
          SMCFCS(IJ,K) = SMCFCS(IJ,1)
        ENDDO
      ENDDO
      IF(LSOIL.GT.2) THEN
        DO K = 3, LSOIL
          DO IJ = 1, IJDIM
            STCFCS(IJ,K) = STCFCS(IJ,2)
          ENDDO
        ENDDO
      ENDIF
C
      RETURN
      END
      SUBROUTINE ROF01(AISFLD,IJDIM,OP,CRIT)
      DIMENSION AISFLD(IJDIM)
      CHARACTER*2 OP
C
      IF(OP.EQ.'GE') THEN
        DO IJ=1,IJDIM
          IF(AISFLD(IJ).GE.CRIT) THEN
            AISFLD(IJ)=1.
          ELSE
            AISFLD(IJ)=0.
          ENDIF
        ENDDO
      ELSEIF(OP.EQ.'GT') THEN
        DO IJ=1,IJDIM
          IF(AISFLD(IJ).GT.CRIT) THEN
            AISFLD(IJ)=1.
          ELSE
            AISFLD(IJ)=0.
          ENDIF
        ENDDO
      ELSEIF(OP.EQ.'LE') THEN
        DO IJ=1,IJDIM
          IF(AISFLD(IJ).LE.CRIT) THEN
            AISFLD(IJ)=1.
          ELSE
            AISFLD(IJ)=0.
          ENDIF
        ENDDO
      ELSEIF(OP.EQ.'LT') THEN
        DO IJ=1,IJDIM
          IF(AISFLD(IJ).LT.CRIT) THEN
            AISFLD(IJ)=1.
          ELSE
            AISFLD(IJ)=0.
          ENDIF
        ENDDO
      ELSE
        WRITE(6,*) ' Illegal operator in ROF01.  OP=',OP
        CALL ABORT
      ENDIF
C
      RETURN
      END
      SUBROUTINE TSFCOR(TSFC,OROG,SLMASK,UMASK,IJDIM,RLAPSE)
C
      DIMENSION TSFC(IJDIM),OROG(IJDIM),SLMASK(IJDIM)
C
      DO IJ=1,IJDIM
        IF(SLMASK(IJ).EQ.UMASK) THEN
          TSFC(IJ)=TSFC(IJ)-OROG(IJ)*RLAPSE
        ENDIF 
      ENDDO
      RETURN
      END
      SUBROUTINE ANOMINT(TSFANL,TSFCLM,TSFCL0,IJDIM)
C
      DIMENSION TSFANL(IJDIM),TSFCLM(IJDIM),TSFCL0(IJDIM)
C
C  Time interpolation of anomalies
C  Add initial anomaly to date interpolated climatology
C
      WRITE(6,*) 'ANOMINT'
      DO IJ=1,IJDIM
        TSFANL(IJ)=TSFANL(IJ)-TSFCL0(IJ)+TSFCLM(IJ)
      ENDDO
      RETURN
      END
      SUBROUTINE SNODPTH(SCVANL,SLIANL,TSFANL,SNOCLM,
     1                   GLACIR,SNWMAX,SNWMIN,IJDIM,SNOANL)
C
      DIMENSION SCVANL(IJDIM),SLIANL(IJDIM),TSFANL(IJDIM),
     1          SNOCLM(IJDIM),SNOANL(IJDIM),GLACIR(IJDIM)
C
      WRITE(6,*) 'SNODPTH'
C
C  USE SURFACE TEMPERATURE TO GET SNOW DEPTH ESTIMATE
C
      DO IJ=1,IJDIM
        SNO = 0.0
C
C  OVER LAND
C
        IF(SLIANL(IJ).EQ.1.) THEN
          IF(SCVANL(IJ).EQ.1.0) THEN
            IF(TSFANL(IJ).LT.243.0) THEN
              SNO = SNWMAX
            ELSEIF(TSFANL(IJ).LT.273.0) THEN
              SNO = SNWMIN+(SNWMAX-SNWMIN)*(273.0-TSFANL(IJ))/30.0
            ELSE
              SNO = SNWMIN
            ENDIF
          ENDIF
C
C  IF GLACIAL POINTS HAS SNOW IN CLIMATOLOGY, SET SNO TO SNOMAX
C
          IF(GLACIR(IJ).EQ.1.0) THEN
            SNO = SNOCLM(IJ)
            IF(SNO.EQ.0.) SNO=SNWMAX
          ENDIF
        ENDIF
C
C  OVER SEA ICE
C
C  Snow over sea ice is cycled as of 01/01/94.....Hua-Lu Pan
C
        IF(SLIANL(IJ).EQ.2.0) THEN
          SNO=SNOCLM(IJ)
          IF(SNO.EQ.0.) SNO=SNWMAX
        ENDIF
C
        SNOANL(IJ) = SNO
      ENDDO
      RETURN
      END
      SUBROUTINE MERGE(IJDIM,LSOIL,IY,IM,ID,IH,FH,
     Z                 TSFFCS,WETFCS,SNOFCS,ZORFCS,ALBFCS,
     1                 PLRFCS,CVFCS ,CVBFCS,CVTFCS,
     2                 CNPFCS,SMCFCS,STCFCS,SLIFCS,
     3                 TSFANL,WETANL,SNOANL,ZORANL,ALBANL,
     4                 PLRANL,CVANL ,CVBANL,CVTANL,
     5                 CNPANL,SMCANL,STCANL,SLIANL,
     6                 CTSFL,CALBL,CSNOL,CSMCL,CZORL,CPLRL,
     8                 CTSFS,CALBS,CSNOS,CSMCS,CZORS,CPLRS,
     9                 CCV,CCVB,CCVT,CCNP,
     7                 IRTTSF,IRTWET,IRTSNO,IRTZOR,IRTALB,IRTAIS,
     8                 IRTTG3,IRTPLR,IRTSCV,IRTACN,IRTSMC,IRTSTC)
C
      DIMENSION TSFFCS(IJDIM),WETFCS(IJDIM),SNOFCS(IJDIM),
     1          ZORFCS(IJDIM),ALBFCS(IJDIM),AISFCS(IJDIM),
     2          PLRFCS(IJDIM),
     3          CVFCS (IJDIM),CVBFCS(IJDIM),CVTFCS(IJDIM),
     4          CNPFCS(IJDIM),
     5          SMCFCS(IJDIM,LSOIL),STCFCS(IJDIM,LSOIL),
     6          SLIFCS(IJDIM)
      DIMENSION TSFANL(IJDIM),WETANL(IJDIM),SNOANL(IJDIM),
     1          ZORANL(IJDIM),ALBANL(IJDIM),AISANL(IJDIM),
     2          PLRANL(IJDIM),
     3          CVANL (IJDIM),CVBANL(IJDIM),CVTANL(IJDIM),
     4          CNPANL(IJDIM),
     5          SMCANL(IJDIM,LSOIL),STCANL(IJDIM,LSOIL),
     6          SLIANL(IJDIM)
C
C  COEEFICIENTS OF BLENDING FORECAST AND INTERPOLATED CLIM
C  (OR ANALYZED) FIELDS OVER SEA OR LAND(L) (NOT FOR CLOUDS)
C  1.0 = USE OF FORECAST
C  0.0 = REPLACE WITH INTERPOLATED ANALYSIS
C
C  Merging coefficients are defined by PARAMETER statement in calling program  
C  and therefore they should not be modified in this program.
C
      RTSFL=CTSFL
      RALBL=CALBL
      RSNOL=CSNOL
      RSMCL=CSMCL
      RZORL=CZORL
      RPLRL=CPLRL
      RTSFS=CTSFS
      RALBS=CALBS
      RSNOS=CSNOS
      RSMCS=CSMCS
      RZORS=CZORS
      RPLRS=CPLRS
      RCV  =CCV
      RCVB =CCVB
      RCVT =CCVT
      RCNP =CCNP
C
C  If analysis file name is given but no matching analysis date found,
C  use guess (these are flagged by IRT???=1).
C
      IF(IRTTSF.EQ.-1) THEN
        RTSFL=1.
        RTSFS=1.
      ENDIF
      IF(IRTALB.EQ.-1) THEN
        RALBL=1.
        RALBS=1.
      ENDIF
      IF(IRTSNO.EQ.-1.OR.IRTSCV.EQ.-1) THEN
        RSNOL=1.
        RSNOS=1.
      ENDIF
      IF(IRTSMC.EQ.-1.OR.IRTWET.EQ.-1) THEN
        RSMCL=1.
        RSMCS=1.
      ENDIF
      IF(IRTZOR.EQ.-1) THEN
        RZORL=1.
        RZORS=1.
      ENDIF
      IF(IRTPLR.EQ.-1) THEN
        RPLRL=1.
        RPLRS=1.
      ENDIF
C
      WRITE(6,100) RTSFL,RALBL,RSNOL,RSMCL,RZORL,RPLRL
  100 FORMAT('RTSFL,RALBL,RSNOL,RSMCL,RZORL,RPLRL=',6F7.3)
      WRITE(6,101) RTSFS,RALBS,RSNOS,RSMCS,RZORS,RPLRS
  101 FORMAT('RTSFS,RALBS,RSNOS,RSMCS,RZORS,RPLRS=',6F7.3)
C
      QTSFL=1.-RTSFL
      QALBL=1.-RALBL
      QSNOL=1.-RSNOL
      QSMCL=1.-RSMCL
      QZORL=1.-RZORL
      QPLRL=1.-RPLRL
C
      QTSFS=1.-RTSFS
      QALBS=1.-RALBS
      QSNOS=1.-RSNOS
      QSMCS=1.-RSMCS
      QZORS=1.-RZORS
      QPLRS=1.-RPLRS
C
      QCV  =1.-RCV
      QCVB =1.-RCVB
      QCVT =1.-RCVT
      QCNP =1.-RCNP
C
C  Merging
C
      DO IJ=1,IJDIM
        IF(SLIANL(IJ).EQ.0.) THEN
          TSFANL(IJ)=TSFFCS(IJ)*RTSFS+TSFANL(IJ)*QTSFS
          ALBANL(IJ)=ALBFCS(IJ)*RALBS+ALBANL(IJ)*QALBS
          SNOANL(IJ)=SNOFCS(IJ)*RSNOS+SNOANL(IJ)*QSNOS
          ZORANL(IJ)=ZORFCS(IJ)*RZORS+ZORANL(IJ)*QZORS
          PLRANL(IJ)=PLRFCS(IJ)*RPLRS+PLRANL(IJ)*QPLRS
        ELSE
          TSFANL(IJ)=TSFFCS(IJ)*RTSFL+TSFANL(IJ)*QTSFL
          ALBANL(IJ)=ALBFCS(IJ)*RALBL+ALBANL(IJ)*QALBL
          SNOANL(IJ)=SNOFCS(IJ)*RSNOL+SNOANL(IJ)*QSNOL
          ZORANL(IJ)=ZORFCS(IJ)*RZORL+ZORANL(IJ)*QZORL
          PLRANL(IJ)=PLRFCS(IJ)*RPLRL+PLRANL(IJ)*QPLRL
        ENDIF
        CVANL(IJ)=CVFCS(IJ)*RCV+CVANL(IJ)*QCV
        CVBANL(IJ)=CVBFCS(IJ)*RCVB+CVBANL(IJ)*QCVB
        CVTANL(IJ)=CVTFCS(IJ)*RCVT+CVTANL(IJ)*QCVT
        CNPANL(IJ)=CNPFCS(IJ)*RCNP+CNPANL(IJ)*QCNP
      ENDDO
      DO K = 1, LSOIL
        DO I = 1, IJDIM
          IF(SLIANL(I).EQ.0.) THEN
            SMCANL(I,K) = SMCFCS(I,K)*RSMCS+SMCANL(I,K)*QSMCS
          ELSE
            SMCANL(I,K) = SMCFCS(I,K)*RSMCL+SMCANL(I,K)*QSMCL
          ENDIF
          STCANL(I,K) = STCFCS(I,K)
        ENDDO
      ENDDO
      RETURN
      END
      SUBROUTINE NEWICE(SLIANL,SLIFCS,TSFANL,TSFFCS,IJDIM,LSOIL,
     Z                   ALBANL,SNOANL,ZORANL,PLRANL,SMCANL,STCANL,
     1                   ALBSEA,SNOSEA,ZORSEA,SMCSEA,SMCICE,PLRSEA,
     2                   TSFMIN,TSFICE,ALBICE,ZORICE,PLRICE,TGICE,
     3                   RLA,RLO)
C
      DIMENSION SLIANL(IJDIM),SLIFCS(IJDIM),TSFFCS(IJDIM),TSFANL(IJDIM)
      DIMENSION ALBANL(IJDIM),SNOANL(IJDIM),ZORANL(IJDIM),PLRANL(IJDIM)
      DIMENSION SMCANL(IJDIM,LSOIL),STCANL(IJDIM,LSOIL)
C
      DIMENSION RLA(IJDIM),RLO(IJDIM)
C
      WRITE(6,*) 'NEWICE'
C
      KOUNT1=0
      KOUNT2=0
      DO IJ=1,IJDIM
        IF(SLIFCS(IJ).NE.SLIANL(IJ)) THEN
          IF(SLIFCS(IJ).EQ.1..OR.SLIANL(IJ).EQ.1.) THEN
            PRINT *,'INCONSISTENCY IN SLIFCS OR SLIANL'
            PRINT 910,RLA(IJ),RLO(IJ),SLIFCS(IJ),SLIANL(IJ),
     1                TSFFCS(IJ),TSFANL(IJ)
  910       FORMAT(2X,'AT LAT=',F5.1,' LON=',F5.1,' SLIFCS=',F3.1,
     1          ' SLIMSK=',F3.1,' TSFFCS=',F5.1,' SET TO TSFANL=',F5.1)
            CALL ABORT
          ENDIF
C
C  INTERPOLATED CLIMATOLOGY INDICATES MELTED SEA ICE
C
          IF(SLIANL(IJ).EQ.0..AND.SLIFCS(IJ).EQ.2.) THEN
            TSFANL(IJ)=TSFMIN
            ALBANL(IJ)=ALBSEA
            SNOANL(IJ)=SNOSEA
            ZORANL(IJ)=ZORSEA
            PLRANL(IJ)=PLRSEA
            DO K = 1, LSOIL
              SMCANL(IJ,K) = SMCSEA
            ENDDO
            KOUNT1=KOUNT1+1
          ENDIF
C
C  INTERPLATED CLIMATOLOYG/ANALYSIS INDICATES NEW SEA ICE
C
          IF(SLIANL(IJ).EQ.2..AND.SLIFCS(IJ).EQ.0.) THEN
            TSFANL(IJ)=TSFICE
            ALBANL(IJ)=ALBICE
            SNOANL(IJ)=0.
            ZORANL(IJ)=ZORICE
            PLRANL(IJ)=PLRICE
            DO K = 1, LSOIL
              SMCANL(IJ,K) = SMCICE
              STCANL(IJ,K) = TGICE
            ENDDO
            KOUNT2=KOUNT2+1
          ENDIF
        ENDIF
      ENDDO
C
      IF(KOUNT1.GT.0) THEN
        WRITE(6,*) 'Sea ice melted.  TSF,ALB,ZOR,PLR are filled',
     1             ' at ',KOUNT1,' points'
      ENDIF
      IF(KOUNT2.GT.0) THEN
        WRITE(6,*) 'Sea ice formed.  TSF,ALB,ZOR,PLR are filled',
     1             ' at ',KOUNT2,' points'
      ENDIF
C
      RETURN
      END
      SUBROUTINE QCMASK(SLMASK,SLLND,SLSEA,IDIM,JDIM,RLA,RLO)
C
      DIMENSION SLMASK(IDIM,JDIM),RLA(IDIM,JDIM),RLO(IDIM,JDIM)
C
      WRITE(6,*) ' QCMASK'
C
C  CHECK LAND-SEA MASK
C
      DO J=1,JDIM
      DO I=1,IDIM
        IF(SLMASK(I,J).NE.SLLND.AND.SLMASK(I,J).NE.SLSEA) THEN
          PRINT *,'*** LAND-SEA MASK NOT ',SLLND,' OR ',SLSEA,
     1          ' AT LAT=',RLA(I,J),' LON=',RLO(I,J),' IT IS ',
     2          SLMASK(I,J)
          CALL ABORT
        ENDIF
      ENDDO
      ENDDO
C
C  Remove isolated sea point
C
      DO J=1,JDIM
        DO I=1,IDIM
          IP=I+1
          IM=I-1
          JP=J+1
          JM=J-1
          IF(JP.GT.JDIM) JP=JDIM-1
          IF(JM.LT.1) JM=2
          IF(IP.GT.IDIM) IP=1
          IF(IM.LT.1) IM=IDIM
          IF(SLMASK(I,J).EQ.0.) THEN
            IF(SLMASK(IP,JP).EQ.1..AND.SLMASK(I ,JP).EQ.1..AND.
     1         SLMASK(IM,JP).EQ.1..AND.
     2         SLMASK(IP,J ).EQ.1..AND.SLMASK(IM,J ).EQ.1..AND.
     3         SLMASK(IP,JM).EQ.1..AND.SLMASK(I ,JM).EQ.1..AND.
     4         SLMASK(IM,JM).EQ.1) THEN
              SLMASK(I,J)=1.
              WRITE(6,*) ' Isolated open sea point modified to land',
     1                   ' at LAT=',RLA(I,J),' LON=',RLO(I,J)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C
C  Remove isolated land point
C
      DO J=1,JDIM
        DO I=1,IDIM
          IP=I+1
          IM=I-1
          JP=J+1
          JM=J-1
          IF(JP.GT.JDIM) JP=JDIM-1
          IF(JM.LT.1) JM=2
          IF(IP.GT.IDIM) IP=1
          IF(IM.LT.1) IM=IDIM
          IF(SLMASK(I,J).EQ.1.) THEN
            IF(SLMASK(IP,JP).EQ.0..AND.SLMASK(I ,JP).EQ.0..AND.
     1         SLMASK(IM,JP).EQ.0..AND.
     2         SLMASK(IP,J ).EQ.0..AND.SLMASK(IM,J ).EQ.0..AND.
     3         SLMASK(IP,JM).EQ.0..AND.SLMASK(I ,JM).EQ.0..AND.
     4         SLMASK(IM,JM).EQ.0) THEN
              SLMASK(I,J)=0.
              WRITE(6,*) ' Isolated land point modified to sea',
     1                   ' at LAT=',RLA(I,J),' LON=',RLO(I,J)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      RETURN
      END
      SUBROUTINE QCSNOW(SNOANL,SLMASK,AISANL,GLACIR,IJDIM,SNOVAL)
      DIMENSION SNOANL(IJDIM),SLMASK(IJDIM),AISANL(IJDIM),GLACIR(IJDIM)
      WRITE(6,*) ' '
      WRITE(6,*) 'QC of SNOW'
      KOUNT=0
      DO IJ=1,IJDIM
        IF(GLACIR(IJ).NE.0..AND.SNOANL(IJ).EQ.0.) THEN
          SNOANL(IJ)=SNOVAL
          KOUNT=KOUNT+1
        ENDIF
      ENDDO
      PER=FLOAT(KOUNT)/FLOAT(IJDIM)*100.
      IF(KOUNT.GT.0) THEN
        PRINT *,'SNOW filled over glacier points at ',KOUNT,
     1          ' POINTS (',PER,'%)'
      ENDIF
      KOUNT=0
      DO IJ=1,IJDIM
        IF(SLMASK(IJ).EQ.0.AND.AISANL(IJ).EQ.0) THEN
          SNOANL(IJ)=0.
          KOUNT=KOUNT+1
        ENDIF
      ENDDO
      PER=FLOAT(KOUNT)/FLOAT(IJDIM)*100.
      IF(KOUNT.GT.0) THEN
        PRINT *,'SNOW set to zero over open sea at ',KOUNT,
     1          ' POINTS (',PER,'%)'
      ENDIF
      RETURN
      END
      SUBROUTINE QCSICE(AIS,GLACIR,AMXICE,AICICE,AICSEA,SLLND,SLMASK,
     1                  RLA,RLO,IDIM,JDIM)
C
      DIMENSION AIS(IDIM,JDIM),GLACIR(IDIM,JDIM),
     1          AMXICE(IDIM,JDIM),SLMASK(IDIM,JDIM)
      DIMENSION RLA(IDIM,JDIM),RLO(IDIM,JDIM)
C
C  CHECK SEA-ICE COVER MASK AGAINST LAND-SEA MASK
C
      WRITE(6,*) 'QC of sea ice'
      KOUNT=0
      KOUNT1=0
      DO J=1,JDIM
      DO I=1,IDIM
        IF(AIS(I,J).NE.AICICE.AND.AIS(I,J).NE.AICSEA) THEN
          PRINT *,'SEA ICE MASK NOT ',AICICE,' OR ',AICSEA
          PRINT *,'AIS(I,J),AICICE,AICSEA,RLA(I,J),RLO(I,J)=',
     1             AIS(I,J),AICICE,AICSEA,RLA(I,J),RLO(I,J)
          CALL ABORT
        ENDIF
        IF(SLMASK(I,J).EQ.0..AND.GLACIR(I,J).EQ.1..AND.
     1     AIS(I,J).NE.1.) THEN
          KOUNT1=KOUNT1+1
          AIS(I,J)=1.
        ENDIF
        IF(SLMASK(I,J).EQ.SLLND.AND.AIS(I,J).EQ.AICICE) THEN
          KOUNT=KOUNT+1
          AIS(I,J)=AICSEA
        ENDIF
      ENDDO
      ENDDO
      PER=FLOAT(KOUNT)/FLOAT(IDIM*JDIM)*100.
      IF(KOUNT.GT.0) THEN
        PRINT *,' Sea ice over land mask at ',KOUNT,' points (',PER,'%)'
      ENDIF
      PER=FLOAT(KOUNT1)/FLOAT(IDIM*JDIM)*100.
      IF(KOUNT1.GT.0) THEN
        PRINT *,' Sea ice set over glacier points over ocean at ',
     1          KOUNT1,' points (',PER,'%)'
      ENDIF
C     KOUNT=0
C     DO J=1,JDIM
C     DO I=1,IDIM
C       IF(AMXICE(I,J).NE.0..AND.AIS(I,J).EQ.0.) THEN
C         AIS(I,J)=0.
C         KOUNT=KOUNT+1
C       ENDIF
C     ENDDO
C     ENDDO
C     PER=FLOAT(KOUNT)/FLOAT(IDIM*JDIM)*100.
C     IF(KOUNT.GT.0) THEN
C       PRINT *,' Sea ice exceeds maxice at ',KOUNT,' points (',PER,'%)'
C     ENDIF
C
C  Remove isolated open ocean surrounded by sea ice and/or land
C
      IJ=0
      DO J=1,JDIM
        DO I=1,IDIM
          IJ=IJ+1
          IP=I+1
          IM=I-1
          JP=J+1
          JM=J-1
          IF(JP.GT.JDIM) JP=JDIM-1
          IF(JM.LT.1) JM=2
          IF(IP.GT.IDIM) IP=1
          IF(IM.LT.1) IM=IDIM
          IF(SLMASK(I,J).EQ.0..AND.AIS(I,J).EQ.0.) THEN
            IF((SLMASK(IP,JP).EQ.1..OR.AIS(IP,JP).EQ.1.).AND.
     1         (SLMASK(I ,JP).EQ.1..OR.AIS(I ,JP).EQ.1.).AND.
     2         (SLMASK(IM,JP).EQ.1..OR.AIS(IM,JP).EQ.1.).AND.
     3         (SLMASK(IP,J ).EQ.1..OR.AIS(IP,J ).EQ.1.).AND.
     4         (SLMASK(IM,J ).EQ.1..OR.AIS(IM,J ).EQ.1.).AND.
     5         (SLMASK(IP,JM).EQ.1..OR.AIS(IP,JM).EQ.1.).AND.
     6         (SLMASK(I ,JM).EQ.1..OR.AIS(I ,JM).EQ.1.).AND.
     7         (SLMASK(IM,JM).EQ.1..OR.AIS(IM,JM).EQ.1.)) THEN
                AIS(I,J)=1.
              WRITE(6,*) ' Isolated open sea point surrounded by',
     1                   ' sea ice or land modified to sea ice',
     2                   ' at LAT=',RLA(I,J),' LON=',RLO(I,J)
            ENDIF
          ENDIF
        ENDDO
      ENDDO
      RETURN
      END
      SUBROUTINE SETLSI(SLMASK,AISFLD,IJDIM,AICICE,SLIFLD)
C
      DIMENSION SLMASK(IJDIM),SLIFLD(IJDIM),AISFLD(IJDIM)
C
C  Set surface condition indicator slimsk
C
      DO IJ=1,IJDIM
        SLIFLD(IJ)=SLMASK(IJ)
        IF(AISFLD(IJ).EQ.AICICE) SLIFLD(IJ)=2.0
      ENDDO
      RETURN
      END
      SUBROUTINE SCALE(FLD,IJDIM,SCL)
C
      DIMENSION FLD(IJDIM)
      DO IJ=1,IJDIM
        FLD(IJ)=FLD(IJ)*SCL
      ENDDO
      RETURN
      END
      SUBROUTINE QCMXMN(TTL,FLD,SLIMSK,SNO,
     1                  FLDLMX,FLDLMN,FLDOMX,FLDOMN,FLDIMX,FLDIMN,
     2                  FLDJMX,FLDJMN,FLDSMX,FLDSMN,EPSFLD,
     3                  RLA,RLO,IJDIM,MODE,PERCRIT,LGCHEK)
C
      PARAMETER(MMPRT=2)
C
      CHARACTER*8 TTL
      DIMENSION FLD(IJDIM),SLIMSK(IJDIM),SNO(IJDIM)
      DIMENSION RLA(IJDIM),RLO(IJDIM)
      LOGICAL LGCHEK
C
C  CHECK AGAINST LAND-SEA MASK AND ICE COVER MASK
C
      PRINT *,' '
      PRINT *,'QC of ',TTL,' MODE=',MODE,'(0=count only, 1=replace)'
C
      KMAXL=0
      KMINL=0
      KMAXO=0
      KMINO=0
      KMAXI=0
      KMINI=0
      KMAXJ=0
      KMINJ=0
      KMAXS=0
      KMINS=0
C
      DO IJ=1,IJDIM
C
C  Lower bound check over bare land
C
        IF(FLDLMN.NE.999..AND.SLIMSK(IJ).EQ.1..AND.SNO(IJ).LE.0..AND.
     1        FLD(IJ).LT.FLDLMN-EPSFLD) THEN
          KMINL=KMINL+1
          IF(KMINL.LT.MMPRT) THEN
            PRINT 8001,RLA(IJ),RLO(IJ),FLD(IJ),FLDLMN
 8001        FORMAT(' Bare land min. check. LAT=',F5.1,
     1         ' LON=',F6.1,' FLD=',E11.6, ' to ',E11.6)
          ENDIF
          IF(MODE.EQ.1) FLD(IJ)=FLDLMN
        ENDIF
C
C  Upper bound check over bare land
C
        IF(FLDLMX.NE.999..AND.SLIMSK(IJ).EQ.1..AND.SNO(IJ).LE.0..AND.
     1        FLD(IJ).GT.FLDLMX+EPSFLD) THEN
          KMAXL=KMAXL+1
          IF(KMAXL.LT.MMPRT) THEN
            PRINT 8002,RLA(IJ),RLO(IJ),FLD(IJ),FLDLMX
 8002        FORMAT(' Bare land max. check. LAT=',F5.1,
     1         ' LON=',F6.1,' FLD=',E11.6, ' to ',E11.6)
          ENDIF
          IF(MODE.EQ.1) FLD(IJ)=FLDLMX
        ENDIF
C
C  Lower bound check over snow covered land
C
        IF(FLDSMN.NE.999..AND.SLIMSK(IJ).EQ.1..AND.SNO(IJ).GT.0..AND.
     1        FLD(IJ).LT.FLDSMN-EPSFLD) THEN
          KMINS=KMINS+1
          IF(KMINS.LT.MMPRT) THEN
            PRINT 8003,RLA(IJ),RLO(IJ),FLD(IJ),FLDSMN
 8003        FORMAT(' Sno covrd land min. check. LAT=',F5.1,
     1         ' LON=',F6.1,' FLD=',E9.4, ' to ',E9.4)
          ENDIF
          IF(MODE.EQ.1) FLD(IJ)=FLDSMN
        ENDIF
C
C  Upper bound check over snow covered land
C
        IF(FLDSMX.NE.999..AND.SLIMSK(IJ).EQ.1..AND.SNO(IJ).GT.0..AND.
     1        FLD(IJ).GT.FLDSMX+EPSFLD) THEN
          KMAXS=KMAXS+1
          IF(KMAXS.LT.MMPRT) THEN
            PRINT 8004,RLA(IJ),RLO(IJ),FLD(IJ),FLDSMX
 8004        FORMAT(' Snow land max. check. LAT=',F5.1,
     1         ' LON=',F6.1,' FLD=',E9.4, ' to ',E9.4)
          ENDIF
          IF(MODE.EQ.1) FLD(IJ)=FLDSMX
        ENDIF
C
C  Lower bound check over open ocean
C
        IF(FLDOMN.NE.999..AND.SLIMSK(IJ).EQ.0..AND.
     1        FLD(IJ).LT.FLDOMN-EPSFLD) THEN
          KMINO=KMINO+1
          IF(KMINO.LT.MMPRT) THEN
            PRINT 8005,RLA(IJ),RLO(IJ),FLD(IJ),FLDOMN
 8005        FORMAT(' Open ocean min. check. LAT=',F5.1,
     1         ' LON=',F6.1,' FLD=',E9.4,' to ',E9.4)
          ENDIF
          IF(MODE.EQ.1) FLD(IJ)=FLDOMN
        ENDIF
C
C  Upper bound check over open ocean
C
        IF(FLDOMX.NE.999..AND.SLIMSK(IJ).EQ.0..AND.
     1        FLD(IJ).GT.FLDOMX+EPSFLD) THEN
          KMAXO=KMAXO+1
          IF(KMAXO.LT.MMPRT) THEN
            PRINT 8006,RLA(IJ),RLO(IJ),FLD(IJ),FLDOMX
 8006        FORMAT(' Open ocean max. check. LAT=',F5.1,
     1         ' LON=',F6.1,' FLD=',E9.4, ' to ',E9.4)
          ENDIF
          IF(MODE.EQ.1) FLD(IJ)=FLDOMX
        ENDIF
C
C  Lower bound check over sea ice without snow
C
        IF(FLDIMN.NE.999..AND.SLIMSK(IJ).EQ.2..AND.SNO(IJ).LE.0..AND.
     1        FLD(IJ).LT.FLDIMN-EPSFLD) THEN
          KMINI=KMINI+1
          IF(KMINI.LT.MMPRT) THEN
            PRINT 8007,RLA(IJ),RLO(IJ),FLD(IJ),FLDIMN
 8007        FORMAT(' Seaice no snow min. check LAT=',F5.1,
     1         ' LON=',F6.1,' FLD=',E9.4, ' to ',E9.4)
          ENDIF
          IF(MODE.EQ.1) FLD(IJ)=FLDIMN
        ENDIF
C
C  Upper bound check over sea ice without snow
C
        IF(FLDIMX.NE.999..AND.SLIMSK(IJ).EQ.2..AND.SNO(IJ).LE.0..AND.
     1        FLD(IJ).GT.FLDIMX+EPSFLD) THEN
          KMAXI=KMAXI+1
          IF(KMAXI.LT.MMPRT) THEN
            PRINT 8008,RLA(IJ),RLO(IJ),FLD(IJ),FLDIMX
 8008        FORMAT(' Seaice no snow max. check LAT=',F5.1,
     1         ' LON=',F6.1,' FLD=',E9.4, ' to ',E9.4)
          ENDIF
          IF(MODE.EQ.1) FLD(IJ)=FLDIMX
        ENDIF
C
C  Lower bound check over sea ice with snow
C
        IF(FLDJMN.NE.999..AND.SLIMSK(IJ).EQ.2..AND.SNO(IJ).GT.0..AND.
     1        FLD(IJ).LT.FLDJMN-EPSFLD) THEN
          KMINJ=KMINJ+1
          IF(KMINJ.LT.MMPRT) THEN
            PRINT 8009,RLA(IJ),RLO(IJ),FLD(IJ),FLDJMN
 8009        FORMAT(' Sea ice snow min. check LAT=',F5.1,
     1         ' LON=',F6.1,' FLD=',E9.4, ' to ',E9.4)
          ENDIF
          IF(MODE.EQ.1) FLD(IJ)=FLDJMN
        ENDIF
C
C  Upper bound check over sea ice with snow
C
        IF(FLDJMX.NE.999..AND.SLIMSK(IJ).EQ.2..AND.SNO(IJ).GT.0..AND.
     1        FLD(IJ).GT.FLDJMX+EPSFLD) THEN
          KMAXJ=KMAXJ+1
          IF(KMAXJ.LT.MMPRT) THEN
            PRINT 8010,RLA(IJ),RLO(IJ),FLD(IJ),FLDJMX
 8010        FORMAT(' Seaice snow max check LAT=',F5.1,
     1         ' LON=',F6.1,' FLD=',E9.4, ' to ',E9.4)
          ENDIF
          IF(MODE.EQ.1) FLD(IJ)=FLDJMX
        ENDIF
      ENDDO
C
C  Print results
C
      WRITE(6,*) 'SUMMARY OF QC'
      PERMAX=0.
      IF(KMINL.GT.0) THEN
        PER=FLOAT(KMINL)/FLOAT(IJDIM)*100.
        PRINT 9001,FLDLMN,KMINL,PER
 9001   FORMAT(' Bare land min check.  Modified to ',F8.1,
     1         ' at ',I5,' points ',F4.1,'%')
        IF(PER.GT.PERMAX) PERMAX=PER
      ENDIF
      IF(KMAXL.GT.0) THEN
        PER=FLOAT(KMAXL)/FLOAT(IJDIM)*100.
        PRINT 9002,FLDLMX,KMAXL,PER
 9002   FORMAT(' Bare land max check. Modified to ',F8.1,
     1         ' at ',I5,' points ',F4.1,'%')
        IF(PER.GT.PERMAX) PERMAX=PER
      ENDIF
      IF(KMINO.GT.0) THEN
        PER=FLOAT(KMINO)/FLOAT(IJDIM)*100.
        PRINT 9003,FLDOMN,KMINO,PER
 9003   FORMAT(' Open ocean min check.  Modified to ',F8.1,
     1         ' at ',I5,' points ',F4.1,'%')
        IF(PER.GT.PERMAX) PERMAX=PER
      ENDIF
      IF(KMAXO.GT.0) THEN
        PER=FLOAT(KMAXO)/FLOAT(IJDIM)*100.
        PRINT 9004,FLDOMX,KMAXO,PER
 9004   FORMAT(' Open sea max check. Modified to ',F8.1,
     1         ' at ',I5,' points ',F4.1,'%')
        IF(PER.GT.PERMAX) PERMAX=PER
      ENDIF
      IF(KMINS.GT.0) THEN
        PER=FLOAT(KMINS)/FLOAT(IJDIM)*100.
        PRINT 9009,FLDSMN,KMINS,PER
 9009   FORMAT(' Snow covered land min check. Modified to ',F8.1,
     1         ' at ',I5,' points ',F4.1,'%')
        IF(PER.GT.PERMAX) PERMAX=PER
      ENDIF
      IF(KMAXS.GT.0) THEN
        PER=FLOAT(KMAXS)/FLOAT(IJDIM)*100.
        PRINT 9010,FLDSMX,KMAXS,PER
 9010   FORMAT(' Snow covered land max check. Modified to ',F8.1,
     1         ' at ',I5,' points ',F4.1,'%')
        IF(PER.GT.PERMAX) PERMAX=PER
      ENDIF
      IF(KMINI.GT.0) THEN
        PER=FLOAT(KMINI)/FLOAT(IJDIM)*100.
        PRINT 9005,FLDIMN,KMINI,PER
 9005   FORMAT(' Bare ice min check.  Modified to ',F8.1,
     1         ' at ',I5,' points ',F4.1,'%')
        IF(PER.GT.PERMAX) PERMAX=PER
      ENDIF
      IF(KMAXI.GT.0) THEN
        PER=FLOAT(KMAXI)/FLOAT(IJDIM)*100.
        PRINT 9006,FLDIMX,KMAXI,PER
 9006   FORMAT(' Bare ice max check. Modified to ',F8.1,
     1         ' at ',I5,' points ',F4.1,'%')
        IF(PER.GT.PERMAX) PERMAX=PER
      ENDIF
      IF(KMINJ.GT.0) THEN
        PER=FLOAT(KMINJ)/FLOAT(IJDIM)*100.
        PRINT 9007,FLDJMN,KMINJ,PER
 9007   FORMAT(' Snow covered ice min check.  Modified to ',F8.1,
     1         ' at ',I5,' points ',F4.1,'%')
        IF(PER.GT.PERMAX) PERMAX=PER
      ENDIF
      IF(KMAXJ.GT.0) THEN
        PER=FLOAT(KMAXJ)/FLOAT(IJDIM)*100.
        PRINT 9008,FLDJMX,KMAXJ,PER
 9008   FORMAT(' Snow covered ice max check. Modified to ',F8.1,
     1         ' at ',I5,' points ',F4.1,'%')
        IF(PER.GT.PERMAX) PERMAX=PER
      ENDIF
C
      IF(LGCHEK) THEN
        IF(PERMAX.GT.PERCRIT) THEN
          WRITE(6,*) ' Too many bad points.  Aborting ....'
          CALL ABORT
        ENDIF
      ENDIF
C
      RETURN
      END
      SUBROUTINE SETZRO(FLD,EPS,IJDIM)
C
      DIMENSION FLD(IJDIM)
      DO IJ=1,IJDIM
      IF(ABS(FLD(IJ)).LT.EPS) FLD(IJ)=0.
      ENDDO
      RETURN
      END
      SUBROUTINE GETSCV(SNOFLD,SCVFLD,IJDIM)
C
      DIMENSION SNOFLD(IJDIM),SCVFLD(IJDIM)
C
      DO IJ=1,IJDIM
        SCVFLD(IJ)=0.
        IF(SNOFLD(IJ).GT.0.) SCVFLD(IJ)=1.
      ENDDO
      RETURN
      END
      SUBROUTINE GETSTC(TSFFLD,TG3FLD,SLIFLD,IJDIM,LSOIL,STCFLD,TSFIMX)
C
      DIMENSION TSFFLD(IJDIM),TG3FLD(IJDIM),SLIFLD(IJDIM)
      DIMENSION STCFLD(IJDIM,LSOIL)
C
C  Layer Soil temperature
C
      DO K = 1, LSOIL
        DO I = 1, IJDIM
          IF(SLIFLD(I).EQ.1.0) THEN
            FACTOR = ((K-1) * 2 + 1) / (2. * LSOIL)
            STCFLD(I,K) = FACTOR*TG3FLD(I)+(1.-FACTOR)*TSFFLD(I)
          ELSEIF(SLIFLD(I).EQ.2.0) THEN
            FACTOR = ((K-1) * 2 + 1) / (2. * LSOIL)
            STCFLD(I,K) = FACTOR*TSFIMX+(1.-FACTOR)*TSFFLD(I)
          ELSE
            STCFLD(I,K) = TG3FLD(I)
          ENDIF
        ENDDO
      ENDDO
      IF(LSOIL.GT.2) THEN
        DO K = 3, LSOIL
          DO IJ = 1, IJDIM
            STCFLD(IJ,K) = STCFLD(IJ,2)
          ENDDO
        ENDDO
      ENDIF
      RETURN
      END
      SUBROUTINE GETSMC(WETFLD,IJDIM,LSOIL,SMCFLD)
C
      DIMENSION WETFLD(IJDIM)
      DIMENSION SMCFLD(IJDIM,LSOIL)
C
      WRITE(6,*) 'GETSMC'
C
C  Layer Soil wetness
C
      DO K = 1, LSOIL
        DO IJ = 1, IJDIM
          SMCFLD(IJ,K) = (WETFLD(IJ)*1000./150.)*.37 + .1
        ENDDO
      ENDDO
      RETURN
      END
      SUBROUTINE USESGT(SIG1T,SLIANL,TG3ANL,IJDIM,LSOIL,TSFANL,STCANL,
     1                  TSFIMX)
C
      DIMENSION SIG1T(IJDIM),SLIANL(IJDIM),TG3ANL(IJDIM)
      DIMENSION TSFANL(IJDIM),STCANL(IJDIM,LSOIL)
C
C  Soil temperature
C
      IF(SIG1T(1).GT.0.) THEN
        DO IJ=1,IJDIM
          IF(SLIANL(IJ).NE.0.) THEN
            TSFANL(IJ)=SIG1T(IJ)
          ENDIF
        ENDDO
      ENDIF
      CALL GETSTC(TSFANL,TG3ANL,SLIANL,IJDIM,LSOIL,STCANL,TSFIMX)
C
      RETURN
      END
      SUBROUTINE SNOSFC(SNOANL,TSFANL,TSFSMX,IJDIM)
      DIMENSION SNOANL(IJDIM)
      DIMENSION TSFANL(IJDIM)
C
      WRITE(6,*) 'Set snow temp to TSFSMX if greater'
      KOUNT=0
      DO IJ=1,IJDIM
        IF(SNOANL(IJ).GT.0.) THEN
          IF(TSFANL(IJ).GT.TSFSMX) TSFANL(IJ)=TSFSMX
          KOUNT=KOUNT+1
        ENDIF
      ENDDO
      IF(KOUNT.GT.0) THEN
        PER=FLOAT(KOUNT)/FLOAT(IJDIM)*100.
        WRITE(6,*) 'Snow sfc.  TSF set to ',TSFSMX,' at ',
     1              KOUNT, ' POINTS ',PER,'%'
      ENDIF
      RETURN
      END
      SUBROUTINE ALBOCN(ALBCLM,SLMASK,ALBOMX,IJDIM)
      DIMENSION ALBCLM(IJDIM),SLMASK(IJDIM)
      DO IJ=1,IJDIM
        IF(SLMASK(IJ).EQ.0) ALBCLM(IJ)=ALBOMX
      ENDDO
      RETURN
      END
      SUBROUTINE QCMXICE(GLACIR,AMXICE,IJDIM)
      DIMENSION GLACIR(IJDIM),AMXICE(IJDIM)
      WRITE(6,*) 'QC of maximum ice extent'
      KOUNT=0
      DO IJ=1,IJDIM
        IF(GLACIR(IJ).EQ.1..AND.AMXICE(IJ).EQ.0.) THEN
          AMXICE(IJ)=0.
          KOUNT=KOUNT+1
        ENDIF
      ENDDO
      IF(KOUNT.GT.0) THEN
        PER=FLOAT(KOUNT)/FLOAT(IJDIM)*100.
        WRITE(6,*) ' Max ice limit less than glacier coverage at ',
     1              KOUNT, ' POINTS ',PER,'%'
      ENDIF
      RETURN
      END
      SUBROUTINE QCSLI(SLIANL,SLIFCS,IJDIM)
      DIMENSION SLIANL(IJDIM),SLIFCS(IJDIM)
      WRITE(6,*) ' '
      WRITE(6,*) 'QCSLI'
      KOUNT=0
      DO IJ=1,IJDIM
        IF(SLIANL(IJ).EQ.1..AND.SLIFCS(IJ).EQ.0.) THEN
          KOUNT=KOUNT+1
          SLIFCS(IJ)=1.
        ENDIF
        IF(SLIANL(IJ).EQ.0..AND.SLIFCS(IJ).EQ.1.) THEN
          KOUNT=KOUNT+1
          SLIFCS(IJ)=0.
        ENDIF
        IF(SLIANL(IJ).EQ.2..AND.SLIFCS(IJ).EQ.1.) THEN
          KOUNT=KOUNT+1
          SLIFCS(IJ)=0.
        ENDIF
        IF(SLIANL(IJ).EQ.1..AND.SLIFCS(IJ).EQ.2.) THEN
          KOUNT=KOUNT+1
          SLIFCS(IJ)=1.
        ENDIF
      ENDDO
      IF(KOUNT.GT.0) THEN
        PER=FLOAT(KOUNT)/FLOAT(IJDIM)*100.
        WRITE(6,*) ' Inconsistency of SLMASK between forecast and',
     1             ' analysis corrected at ',KOUNT, ' POINTS ',PER,'%'
      ENDIF
      RETURN
      END
      SUBROUTINE NNTPRT(DATA,IMAX,JMAX)
      DIMENSION DATA(IMAX*JMAX)
      ILAST=0
      I1=1
      I2=80
 1112 CONTINUE
      WRITE(6,*) ' '
      DO J=1,JMAX
        WRITE(6,1111) (NINT(DATA(IMAX*(J-1)+I)),I=I1,I2)
      ENDDO
      IF(ILAST.EQ.1) RETURN
      I1=I1+80
      I2=I1+79
      IF(I2.GE.IMAX) THEN
        ILAST=1
        I2=IMAX
      ENDIF
      GO TO 1112
 1111 FORMAT(80I1)
      RETURN
      END
      SUBROUTINE QCBYFC(TSFFCS,SNOFCS,QCTSFS,QCSNOS,QCTSFI,
     1                  IJDIM,LSOIL,SNOANL,AISANL,SLIANL,TSFANL,ALBANL,
     2                  ZORANL,PLRANL,SMCANL,
     3                  PLRCLM,SMCCLM,TSFSMX,ALBOMX,ZOROMX)
C
      DIMENSION TSFFCS(IJDIM),SNOFCS(IJDIM)
      DIMENSION SNOANL(IJDIM),AISANL(IJDIM),SLIANL(IJDIM),ZORANL(IJDIM),
     1          TSFANL(IJDIM),ALBANL(IJDIM),PLRANL(IJDIM),
     2          SMCANL(IJDIM,LSOIL)
      DIMENSION PLRCLM(IJDIM),SMCCLM(IJDIM,LSOIL)
C
      WRITE(6,*) 'QC of snow and sea-ice ANALYSIS'
C
C QC of snow analysis 
C 
C  Questionable snow cover
C
      KOUNT=0
      DO IJ=1,IJDIM
        IF(SLIANL(IJ).GT.0..AND.
     1     TSFFCS(IJ).GT.QCTSFS.AND.SNOANL(IJ).GT.0.) THEN
          KOUNT=KOUNT+1
          SNOANL(IJ)=0.
          TSFANL(IJ)=TSFFCS(IJ)
        ENDIF
      ENDDO
      IF(KOUNT.GT.0) THEN
        PER=FLOAT(KOUNT)/FLOAT(IJDIM)*100.
        WRITE(6,*) ' Guess surface temp .GT. ',QCTSFS,
     1             ' but snow analysis indicates snow cover'
        WRITE(6,*) ' Snow analysis set to zero',
     1             ' at ',KOUNT, ' POINTS ',PER,'%'
      ENDIF
C
C  Questionable no snow cover
C
      KOUNT=0
      DO IJ=1,IJDIM
        IF(SLIANL(IJ).GT.0..AND.
     1     SNOFCS(IJ).GT.QCSNOS.AND.SNOANL(IJ).LT.0.) THEN
          KOUNT=KOUNT+1
          SNOANL(IJ)=SNOFCS(IJ)
          TSFANL(IJ)=TSFFCS(IJ)
        ENDIF
      ENDDO
      IF(KOUNT.GT.0) THEN
        PER=FLOAT(KOUNT)/FLOAT(IJDIM)*100.
        WRITE(6,*) ' Guess snow depth .GT. ',QCSNOS,
     1             ' but snow analysis indicates no snow cover'
        WRITE(6,*) ' Snow analysis set to guess value',
     1             ' at ',KOUNT, ' POINTS ',PER,'%'
      ENDIF
C
C  Questionable sea ice cover
C removed 2/2005
c     KOUNT=0
c     DO IJ=1,IJDIM
c       IF(SLIANL(IJ).EQ.2..AND.
c    1     TSFFCS(IJ).GT.QCTSFI.AND.AISANL(IJ).EQ.1.) THEN
c         KOUNT=KOUNT+1
c         AISANL(IJ)=0.
c         SLIANL(IJ)=0.
c         TSFANL(IJ)=TSFFCS(IJ)
c         SNOANL(IJ)=0.
c         ZORANL(IJ)=ZOROMX
c         PLRANL(IJ)=PLRCLM(IJ)
c         ALBANL(IJ)=ALBOMX
c         DO K=1,LSOIL
c           SMCANL(IJ,K)=SMCCLM(IJ,K)
c         ENDDO
c       ENDIF
c     ENDDO
c     IF(KOUNT.GT.0) THEN
c       PER=FLOAT(KOUNT)/FLOAT(IJDIM)*100.
c       WRITE(6,*) ' Guess surface temp .GT. ',QCTSFI,
c    1             ' but sea-ice analysis indicates sea-ice'
c       WRITE(6,*) ' Sea-ice analysis set to zero',
c    1             ' at ',KOUNT, ' POINTS ',PER,'%'
c     ENDIF
C
      RETURN
      END
      SUBROUTINE SETRMSK(KPDS5,SLMASK,IGAUL,JGAUL,WLON,RNLAT,
     1                   DATA,IMAX,JMAX,LMASK,RSLMSK)
      DIMENSION SLMASK(IGAUL,JGAUL)
      DIMENSION DATA(IMAX,JMAX),RSLMSK(IMAX,JMAX)
      LOGICAL LMASK
C
      PARAMETER(KPDTSF=11,KPDWET=86,KPDSNO=65,KPDZOR=83,
     1          KPDALB=84,KPDAIS=91,KPDTG3=11,KPDPLR=224,
     2          KPDGLA=238,KPDMXI=91,KPDSCV=238,
     3          KPDORO=8,KPDMSK=81,KPDSTC=11,KPDACN=91)
C
      IJMAX=IMAX*JMAX
      RSLMSK=0.
C
C  Surface temperature
C
      IF(KPDS5.EQ.KPDTSF) THEN
        LMASK=.false.
C
C  Bucket soil wetness
C
      ELSEIF(KPDS5.EQ.KPDWET) THEN
        CALL GA2LA(SLMASK,IGAUL,JGAUL,RSLMSK,IMAX,JMAX,WLON,RNLAT)
        CALL ROF01(RSLMSK,IJMAX,'GE',0.5)
        LMASK=.true.
C       WRITE(6,*) 'WET RSLMSK'
C       CALL NNTPRT(RSLMSK,IMAX,JMAX)
C
C  Snow depth
C
      ELSEIF(KPDS5.EQ.KPDSNO) THEN
        CALL GA2LA(SLMASK,IGAUL,JGAUL,RSLMSK,IMAX,JMAX,WLON,RNLAT)
        CALL ROF01(RSLMSK,IJMAX,'GE',0.5)
        LMASK=.true.
C       WRITE(6,*) 'SNO RSLMSK'
C       CALL NNTPRT(RSLMSK,IMAX,JMAX)
C
C  Surface roughness
C
      ELSEIF(KPDS5.EQ.KPDZOR) THEN
        RSLMSK=DATA
        CALL ROF01(RSLMSK,IJMAX,'LT',9.9)
        LMASK=.true.
C       WRITE(6,*) 'ZOR RSLMSK'
C       CALL NNTPRT(RSLMSK,IMAX,JMAX)
C
C  Albedo
C
      ELSEIF(KPDS5.EQ.KPDALB) THEN
        RSLMSK=DATA
        CALL ROF01(RSLMSK,IJMAX,'LT',99.)
        LMASK=.true.
C       WRITE(6,*) 'ALB RSLMSK'
C       CALL NNTPRT(RSLMSK,IMAX,JMAX)
C
C  Sea ice, mask, concentration, max extent
C
      ELSEIF(KPDS5.EQ.KPDAIS) THEN
        LMASK=.false.
        DATAMAX = maxval(DATA)
        CRIT = 1.1
        if (DATAMAX .gt. CRIT) then
c         new ice analyses .. ice(land) > crit
c         use data to define land/sea mask
c         use this mask for interpolation
          LMASK=.true.
          where (DATA .gt. CRIT)
            RSLMSK = 1
          elsewhere
            RSLMSK = 0
          endwhere
c         set land points to 0 (no sea ice)
          where (DATA .gt. CRIT) DATA = 0.0
        endif
C
C  Deep soil temperature
C
      ELSEIF(KPDS5.EQ.KPDTG3) THEN
        LMASK=.false.
C
C  Plant resistance
C
      ELSEIF(KPDS5.EQ.KPDPLR) THEN
        CALL GA2LA(SLMASK,IGAUL,JGAUL,RSLMSK,IMAX,JMAX,WLON,RNLAT)
        CALL ROF01(RSLMSK,IJMAX,'GE',0.5)
        LMASK=.true.
C
C       WRITE(6,*) 'PLR RSLMSK'
C       CALL NNTPRT(RSLMSK,IMAX,JMAX)
C
C  Glacier points
C
      ELSEIF(KPDS5.EQ.KPDGLA) THEN
        LMASK=.false.
C
C  Snow cover
C
      ELSEIF(KPDS5.EQ.KPDSCV) THEN
        CALL GA2LA(SLMASK,IGAUL,JGAUL,RSLMSK,IMAX,JMAX,WLON,RNLAT)
        CALL ROF01(RSLMSK,IJMAX,'GE',0.5)
        LMASK=.true.
C       WRITE(6,*) 'SCV RSLMSK'
C       CALL NNTPRT(RSLMSK,IMAX,JMAX)
C
      ENDIF
C
      RETURN
      END
      SUBROUTINE GA2LA(GAUIN,IMXIN,JMXIN,REGOUT,IMXOUT,JMXOUT,
     1                 WLON,RNLAT)
C                                                                       
C  INTERPOLATION FROM LAT/LON GRID TO OTHER LAT/LON GRID                
C                                                                       
      DIMENSION GAUIN (IMXIN,JMXIN)                                   
C                                                                       
      DIMENSION REGOUT(IMXOUT,JMXOUT)                                     
      DIMENSION GAUL(500),REGL(500)                                     
      DIMENSION IINDX1(1000)                                            
      DIMENSION IINDX2(1000)                                            
      DIMENSION JINDX1(500)
      DIMENSION JINDX2(500) 
      DIMENSION DDX(1000)
      DIMENSION DDY(500)
C
      CALL GAULAT(GAUL,JMXIN)
      DO J=1,JMXIN
        GAUL(J)=90.-GAUL(J)
      ENDDO
C 
      DPHI=180./FLOAT(JMXOUT-1)
      DO J=1,JMXOUT
        IF(RNLAT.GT.0.) THEN
          REGL(J)=RNLAT-FLOAT(J-1)*DPHI                                       
        ELSE
          REGL(J)=RNLAT+FLOAT(J-1)*DPHI                                       
        ENDIF
      ENDDO                                                          
C                                                                       
      DXIN =360./FLOAT(IMXIN )                                          
      DXOUT=360./FLOAT(IMXOUT)                                          
C                                                                       
      DO I=1,IMXOUT                                                  
        ALAMD=WLON+FLOAT(I-1)*DXOUT                                            
        IF(ALAMD.LT.0.) ALAMD=ALAMD+360.
        IF(ALAMD.GT.360.) ALAMD=ALAMD-360.
        I1=ALAMD/DXIN+1.001                                               
        IINDX1(I)=I1                                                      
        I2=I1+1                                                           
        IF(I2.GT.IMXIN) I2=1                                              
        IINDX2(I)=I2                                                      
        DDX(I)=(ALAMD-FLOAT(I1-1)*DXIN)/DXIN                              
      ENDDO                                                          
C                                                                       
      J2=1                                                              
      DO 40 J=1,JMXOUT                                                  
      APHI=REGL(J)                                                      
      DO 50 JJ=1,JMXIN                                                  
      IF(APHI.LT.GAUL(JJ)) GO TO 50                                     
      J2=JJ                                                             
      GO TO 42                                                          
   50 CONTINUE                                                          
   42 CONTINUE                                                          
      IF(J2.GT.2) GO TO 43                                              
      J1=1                                                              
      J2=2                                                              
      GO TO 44                                                          
   43 CONTINUE                                                          
      IF(J2.LE.JMXIN) GO TO 45                                          
      J1=JMXIN-1                                                        
      J2=JMXIN                                                          
      GO TO 44                                                          
   45 CONTINUE                                                          
      J1=J2-1                                                           
   44 CONTINUE                                                          
      JINDX1(J)=J1                                                      
      JINDX2(J)=J2                                                      
      DDY(J)=(APHI-GAUL(J1))/(GAUL(J2)-GAUL(J1))                        
   40 CONTINUE                                                          
C                                                                       
C     WRITE(6,*) 'GA2LA'
C     WRITE(6,*) 'IINDX1'                                                  
C     WRITE(6,*) (IINDX1(N),N=1,IMXOUT)                                    
C     WRITE(6,*) 'IINDX2'                                                  
C     WRITE(6,*) (IINDX2(N),N=1,IMXOUT)                                    
C     WRITE(6,*) 'JINDX1'                                                  
C     WRITE(6,*) (JINDX1(N),N=1,JMXOUT)                                    
C     WRITE(6,*) 'JINDX2'                                                  
C     WRITE(6,*) (JINDX2(N),N=1,JMXOUT)                                    
C     WRITE(6,*) 'DDY'                                                     
C     WRITE(6,*) (DDY(N),N=1,JMXOUT)                                       
C     WRITE(6,*) 'DDX'                                                     
C     WRITE(6,*) (DDX(N),N=1,JMXOUT)                                       
C                                                                       
      DO 60 J=1,JMXOUT                                                  
      Y=DDY(J)                                                          
      J1=JINDX1(J)                                                      
      J2=JINDX2(J)                                                      
      DO 60 I=1,IMXOUT                                                  
      X=DDX(I)                                                          
      I1=IINDX1(I)                                                      
      I2=IINDX2(I)                                                      
      REGOUT(I,J)=(1.-X)*(1.-Y)*GAUIN(I1,J1)+(1.-Y)*X*GAUIN(I2,J1)+     
     1           (1.-X)*Y*GAUIN(I1,J2)+X*Y*GAUIN(I2,J2)                 
   60 CONTINUE                                                          
C                                                                       
      SUM1=0.                                                           
      SUM2=0.                                                           
      DO 70 I=1,IMXIN                                                   
      SUM1=SUM1+GAUIN(I,1)                                              
      SUM2=SUM2+GAUIN(I,JMXIN)                                          
   70 CONTINUE                                                          
      SUM1=SUM1/FLOAT(IMXIN)                                            
      SUM2=SUM2/FLOAT(IMXIN)                                            
C                                                                       
      DO 80 I=1,IMXOUT                                                  
      REGOUT(I,     1)=SUM1                                             
      REGOUT(I,JMXOUT)=SUM2                                             
   80 CONTINUE                                                          
C                                                                       
      RETURN                                                            
      END                                                               
      SUBROUTINE RDGB(LUGB,LGRIB,LSKIP,KPDS,KGDS,NDATA,LBMS,DATA)
C
C  READ GRIB FILE
C  INPUT
C    LUGB - LOGICAL UNIT TO READ
C    LGRIB - LENGTH OF GRIB RECORD
C    LSKIP - BYTES TO SKIP FOR GRIB RECORD
C  OUTPUT
C    KPDS(22) - UNPACKED PRODUCT DEFINITION SECTION
C    KGDS(20) - UNPACKED GRID DEFINITION SECTION
C    NDATA    - NUMBER OF DATA POINTS
C    LBMS(NDATA) - LOGICAL BIT MAP
C    DATA(NDATA) - DATA UNPACKED
C
      CHARACTER GRIB(LGRIB)*1
      INTEGER KPDS(1000),KGDS(20),KPTR(16)
      LOGICAL LBMS(*)
      REAL DATA(*)
      NDATA=0
      CALL BAREAD(LUGB,LSKIP,LGRIB,LREAD,GRIB)
      IF(LREAD.LT.LGRIB) RETURN
      CALL W3FI63(GRIB,KPDS,KGDS,LBMS,DATA,KPTR,IRET)
      IF(IRET.NE.0) RETURN
      NDATA=KPTR(10)
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE PDS567(LUGI,KPDS5,KPDS6,KPDS7,INUM,LSKIP,LGRIB)
C
C  SEARCH GRIB1 INDEX FILE FOR GIVEN PARAMETER AND LEVEL.
C
C  LUGI     - INPUT INTEGER UNIT OF GRIB1 INDEX FILE (GB1IX1) TO READ
C  KPDS5    - INPUT INTEGER PARAMETER NUMBER TO FIND (-1 FOR WILDCARD)
C  KPDS6    - INPUT INTEGER LEVEL TYPE TO FIND (-1 FOR WILDCARD)
C  KPDS7    - INPUT INTEGER LEVEL VALUE TO FIND (-1 FOR WILDCARD)
C  INUM     - INPUT AND OUTPUT INTEGER RECORD NUMBER
C  LSKIP    - OUTPUT INTEGER BYTES TO SKIP IN GRIB1 DATA FILE
C  LGRIB    - OUTPUT INTEGER BYTES TO READ IN GRIB1 DATA FILE
C             (0 IF RECORD NOT FOUND)
C
C     PARAMETER(MBUF=8192*128)
      PARAMETER(MBUF=1024*128)
      CHARACTER CBUF(MBUF)*1
      INTEGER MOVA2I
      SAVE LUX,NSKP,NLEN,NNUM,CBUF
      DATA LUX/0/
      CHARACTER CHEAD(2)*81
      IF(LUGI.NE.LUX) THEN
c       REWIND LUGI
c       READ(LUGI,IOSTAT=IOS) CHEAD
c       IF(IOS.EQ.0.AND.CHEAD(1)(42:47).EQ.'GB1IX1') THEN
        call baread(lugi,0,162,lhead,chead)
        IF(CHEAD(1)(42:47).EQ.'GB1IX1') THEN

          LUX=0
          READ(CHEAD(2),'(8X,3I10,2X,A40)',IOSTAT=IOS) NSKP,NLEN,NNUM
          IF(IOS.EQ.0) THEN
            NBUF=NNUM*NLEN
            IF(NBUF.GT.MBUF) THEN
              PRINT *,'PDS567: INCREASE BUFFER FROM ',MBUF,' TO ',NBUF
              NNUM=MBUF/NLEN
              NBUF=NNUM*NLEN
              CALL ABORT
            ENDIF
            CALL BAREAD(LUGI,NSKP,NBUF,LBUF,CBUF)
            IF(LBUF.EQ.NBUF) THEN
              LUX=LUGI
              INUM=0
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      LGRIB=0
      IF(LUGI.EQ.LUX) THEN
        N=INUM+1
        DOWHILE(LGRIB.EQ.0.AND.N.LE.NNUM)
          IF((KPDS5.EQ.-1.OR.KPDS5.EQ.MOVA2I(CBUF((N-1)*NLEN+34))).AND.
     &       (KPDS6.EQ.-1.OR.KPDS6.EQ.MOVA2I(CBUF((N-1)*NLEN+35))).AND.
     &       (KPDS7.EQ.-1.OR.KPDS7.EQ.MOVA2I(CBUF((N-1)*NLEN+36))*256+
     &                                MOVA2I(CBUF((N-1)*NLEN+37)))) THEN
            INUM=N
            CALL gbytec(CBUF,LSKIP,8*(N-1)*NLEN,8*4)
            CALL gbytec(CBUF,LGRIB,8*(N-1)*NLEN+8*20,8*4)
          ENDIF
          N=N+1
        ENDDO
      ENDIF
      RETURN
      END
C-----------------------------------------------------------------------
      SUBROUTINE PDSCAN(LUGI,KPDS,INUM,LSKIP,LGRIB)
C
C  SEARCH GRIB1 INDEX FILE FOR GIVEN PDS PARAMETERS.
C
C  LUGI     - INPUT INTEGER UNIT OF GRIB1 INDEX FILE (GB1IX1) TO READ
C  KPDS     - INPUT AND OUTPUT INTEGER (22) PDS LIST TO MATCH
C             (SAME DEFINITIONS AS IN W3FI63) (-1 FOR WILDCARD)
C  INUM     - INPUT AND OUTPUT INTEGER RECORD NUMBER (-1 TO REOPEN FILE)
C  LSKIP    - OUTPUT INTEGER BYTES TO SKIP IN GRIB1 DATA FILE
C  LGRIB    - OUTPUT INTEGER BYTES TO READ IN GRIB1 DATA FILE
C             (0 IF RECORD NOT FOUND)
C
      INTEGER KPDS(1000)
C     PARAMETER(MBUF=8192*128)
      PARAMETER(MBUF=1024*128)
      CHARACTER CBUF(MBUF)*1
      SAVE LUX,NSKP,NLEN,NNUM,CBUF
      DATA LUX/0/
      CHARACTER CHEAD(2)*81
      CHARACTER CPDS(28)*1
      INTEGER KPTR(16),KPDSI(1000)
      LOGICAL LMATCH
      IF(INUM.LT.0.OR.LUGI.NE.LUX) THEN
c       REWIND LUGI
c       READ(LUGI,IOSTAT=IOS) CHEAD
c       IF(IOS.EQ.0.AND.CHEAD(1)(42:47).EQ.'GB1IX1') THEN
        call baread(lugi,0,162,lhead,chead)
        IF(CHEAD(1)(42:47).EQ.'GB1IX1') THEN

          LUX=0
          READ(CHEAD(2),'(8X,3I10,2X,A40)',IOSTAT=IOS) NSKP,NLEN,NNUM
          IF(IOS.EQ.0) THEN
            NBUF=NNUM*NLEN
            IF(NBUF.GT.MBUF) THEN
              PRINT *,'PDS567: INCREASE BUFFER FROM ',MBUF,' TO ',NBUF
              NNUM=MBUF/NLEN
              NBUF=NNUM*NLEN
              CALL ABORT
            ENDIF
            CALL BAREAD(LUGI,NSKP,NBUF,LBUF,CBUF)
            IF(LBUF.EQ.NBUF) THEN
              LUX=LUGI
              INUM=0
            ENDIF
          ENDIF
        ENDIF
      ENDIF
      LGRIB=0
      IF(INUM.GE.0.AND.LUGI.EQ.LUX) THEN
        N=INUM+1
        DOWHILE(LGRIB.EQ.0.AND.N.LE.NNUM)
          CPDS=CBUF((N-1)*NLEN+26:(N-1)*NLEN+53)
          KPTR=0
          CALL gbytec(CBUF,KPTR(3),(N-1)*NLEN*8+25*8,3*8)
          KPDSI(18)=1
          CALL gbytec(CPDS,KPDSI(4),7*8,8)
          CALL FI632(CPDS,KPTR,KPDSI,IRET)
          LMATCH=.TRUE.
          IP=1
          DOWHILE(LMATCH.AND.IP.LE.22)
            LMATCH=KPDS(IP).EQ.-1.OR.KPDS(IP).EQ.KPDSI(IP)
            IP=IP+1
          ENDDO
          IF(LMATCH) THEN
            KPDS=KPDSI
            INUM=N
            CALL gbytec(CBUF,LSKIP,(N-1)*NLEN*8,4*8)
            CALL gbytec(CBUF,LGRIB,(N-1)*NLEN*8+20*8,4*8)
          ENDIF
          N=N+1
        ENDDO
      ENDIF
      RETURN
      END
