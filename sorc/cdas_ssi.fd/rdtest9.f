      SUBROUTINE RDTEST(INBUFR,ON85DT,NTDATA,NSDATA,NWDATA,
     .                  NPDATA,NQDATA,NPWDAT,NQTDATA,NSPROF)

      PARAMETER (NOUT=512)

      DIMENSION PSDATA(NOUT,8) , PSTYPE(NOUT)
      DIMENSION QDATA (NOUT,7) , QTYPE (NOUT) , QMAXERR(NOUT)
      DIMENSION TDATA (NOUT,7) , TTYPE (NOUT) , IQTFLG (NOUT)
      DIMENSION WDATA (NOUT,8) , WTYPE (NOUT)
      DIMENSION SDATA (NOUT,5) , STYPE (NOUT)
      DIMENSION PWDATA(NOUT,6) , PWTYPE(NOUT) , PWMERR (NOUT)

      CHARACTER*40 HDSTR,OBSTR,QMSTR,OESTR
      CHARACTER*8  SUBSET,ON85DT(4),DATE
      DIMENSION    HDR(5),OBS(8,255),QMS(8,255),OES(8,255)
      DIMENSION    PSF(0:3),QMF(0:3),QQF(0:3),TMF(0:3),WDF(0:3)
      LOGICAL      SAT,QMP,QPS,QQQ,QTM,QTS,QWD,QPW

      DATA HDSTR /'SID XOB YOB DHR TYP             '/
      DATA OBSTR /'POB QOB TOB ZOB UOB VOB PWO CAT '/
      DATA QMSTR /'PQM QQM TQM ZQM WQM NUL PWQ     '/
      DATA OESTR /'POE QOE TOE NUL WOE NUL PWE     '/
      DATA PSF   /  .9 , 1 , 1 , 1.2 /
      DATA QMF   / 1.5 , 1 , 1 ,  .7 /
      DATA QQF   /  .9 , 1 , 1 , 1.7 /
      DATA TMF   /  .9 , 1 , 1 , 1.2 /
      DATA WDF   /  .9 , 1 , 1 , 1.2 /
      DATA EMERR /  .2 /


C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

      DG2RAD = ATAN(1.)/45.

C  ZERO DATA COUNTERS
C  ------------------

      NTDATA  = 0
      NSDATA  = 0
      NWDATA  = 0
      NPDATA  = 0
      NQDATA  = 0
      NPWDAT  = 0

      MTDATA  = 0
      MSDATA  = 0
      MWDATA  = 0
      MPDATA  = 0
      MQDATA  = 0
      MPWDAT  = 0

      NMRECS  = 0
      NSPROF  = 0
      NQTDATA = 0
      IQTFLG  = 0

C  OPEN THEN READ THE BUFR DATA
C  ----------------------------

c note datebf only returns 2 digit year (iy)
 
      CALL DATEBF(INBUFR,IY,IM,ID,IH,IDATE)
      IF(IDATE.LT.0) GOTO 1000

c     CALL W3FS11(ON85DT(2),IY,IM,ID,IH,0)
c note w3fs11 only wants a 2 digit iy

      on85dt(2)(1:1) = char(mod(iy,100))
      on85dt(2)(2:2) = char(im)
      on85dt(2)(3:3) = char(id)
      on85dt(2)(4:4) = char(ih)
 
C  OPEN AGAIN THEN READ THE BUFR DATA
C  ----------------------------------

      CALL OPENBF(INBUFR,'IN',INBUFR)


10    DO WHILE(IREADMG(INBUFR,SUBSET,IDATE).EQ.0)
      DO WHILE(IREADSB(INBUFR).EQ.0)

C  READ THE HEADER
C  ---------------

      CALL UFBINT(INBUFR,HDR,5,1,LEVS,HDSTR)
      STAID = HDR(1)
      RLON  = HDR(2)*DG2RAD
      RLAT  = HDR(3)*DG2RAD
      TIME  = HDR(4)
      KX    = NINT(HDR(5))
      SAT   = KX.GE.160 .AND. KX.LE.179

      IF(SAT) NSPROF = NSPROF+1
      NMRECS = NMRECS+1

C  GO THROUGH THE DATA LEVELS
C  --------------------------

      CALL UFBINT(INBUFR,OBS,8,255,LEVS,OBSTR)
      CALL UFBINT(INBUFR,QMS,8,255,LEVS,QMSTR)
      CALL UFBINT(INBUFR,OES,8,255,LEVS,OESTR)

      DO K=1,LEVS
      PPB = OBS(1,K)
      IF(PPB.GT.0.) POB = LOG(.1*OBS(1,K))
      QOB = OBS(2,K)*1E-6
      TOB = OBS(3,K)+273.15
      ZOB = OBS(4,K)
      UOB = OBS(5,K)
      VOB = OBS(6,K)
      PWO = OBS(7,K)
      CAT = OBS(8,K)

      PQM = QMS(1,K)
      QQM = QMS(2,K)
      TQM = QMS(3,K)
      ZQM = QMS(4,K)
      WQM = QMS(5,K)
      PWQ = QMS(7,K)

      POE  = OES(1,K)*1E-3
      QOE  = OES(2,K)*.1
      TOE  = OES(3,K)
      WOE  = OES(5,K)
      PWE = OES(7,K)

C  SEE WHICH VARIABLES WE HAVE HERE
C  --------------------------------

      QMP = PQM.LT.4 .AND. PPB.GT.0
      QPS = ZQM.LT.4 .AND. QMP .AND. CAT.EQ.0
      QQQ = QQM.LT.4 .AND. QMP
      QTM = TQM.LT.4 .AND. QMP .AND. .NOT.SAT
      QTS = TQM.LT.4 .AND. QMP .AND.  SAT
      QWD = WQM.LT.4 .AND. QMP
      QPW = PWQ.LT.4 .AND. QMP

C  STORE SURFACE PRESSURE DATA
C  ---------------------------

      IF(QPS) THEN
         NPDATA = NPDATA+1
         MPDATA = MAX(MOD(MPDATA+1,NOUT+1),1)
         PSTYPE(MPDATA  ) = KX
         PSDATA(MPDATA,1) = POE*PSF(NINT(PQM))
         PSDATA(MPDATA,2) = RLON
         PSDATA(MPDATA,3) = RLAT
         PSDATA(MPDATA,4) = POB
         PSDATA(MPDATA,5) = ZOB
         PSDATA(MPDATA,6) = TOB
         PSDATA(MPDATA,7) = STAID
         PSDATA(MPDATA,8) = TIME
         IF(MPDATA.EQ.NOUT) WRITE(84) PSDATA,PSTYPE
      ENDIF

C  STORE SPECIFIC HUMIDITY DATA
C  ----------------------------

      IF(QQQ) THEN
         NQDATA = NQDATA+1
         MQDATA = MAX(MOD(MQDATA+1,NOUT+1),1)
         QMAXERR(MQDATA) = EMERR*QMF(NINT(QQM))
         QTYPE(MQDATA  ) = KX
         QDATA(MQDATA,1) = QOE*QQF(NINT(QQM))
         QDATA(MQDATA,2) = RLON
         QDATA(MQDATA,3) = RLAT
         QDATA(MQDATA,4) = POB
         QDATA(MQDATA,5) = QOB
         QDATA(MQDATA,6) = STAID
         QDATA(MQDATA,7) = TIME
         IF(MQDATA.EQ.NOUT) WRITE(85) QDATA,QTYPE,QMAXERR
      ENDIF

C  STORE NON-SATEM TEMPERATURE DATA
C  --------------------------------

      IF(QTM) THEN
         NTDATA = NTDATA+1
         MTDATA = MAX(MOD(MTDATA+1,NOUT+1),1)
         IF(QQM.GE.4 .AND. PPB .GT. 300.)THEN
            NQTDATA = NQTDATA+1
            IQTFLG(MTDATA)=1
         ENDIF
         TTYPE(MTDATA  ) = KX
         TDATA(MTDATA,1) = TOE*TMF(NINT(TQM))
         TDATA(MTDATA,2) = RLON
         TDATA(MTDATA,3) = RLAT
         TDATA(MTDATA,4) = POB
         TDATA(MTDATA,5) = TOB
         TDATA(MTDATA,6) = STAID
         TDATA(MTDATA,7) = TIME
         IF(MTDATA.EQ.NOUT) WRITE(81) TDATA,TTYPE,IQTFLG
         IF(MTDATA.EQ.NOUT) IQTFLG = 0                        
      ENDIF

C  STORE SATEM TEMPERATURE DATA
C  ----------------------------

      IF(QTS) THEN
         NSDATA = NSDATA+1
         MSDATA = MAX(MOD(MSDATA+1,NOUT+1),1)
         STYPE(MSDATA  ) = KX
         SDATA(MSDATA,1) = RLON
         SDATA(MSDATA,2) = RLAT
         SDATA(MSDATA,3) = POB
         SDATA(MSDATA,4) = TOB
         SDATA(MSDATA,5) = TIME
         IF(MSDATA.EQ.NOUT) WRITE(82) SDATA,STYPE
      ENDIF

C  STORE WIND DATA
C  ---------------

      IF(QWD) THEN
         NWDATA = NWDATA+1
         MWDATA = MAX(MOD(MWDATA+1,NOUT+1),1)
         WTYPE(MWDATA  ) = KX
         WDATA(MWDATA,1) = WOE*WDF(NINT(WQM))
         WDATA(MWDATA,2) = RLON
         WDATA(MWDATA,3) = RLAT
         WDATA(MWDATA,4) = POB
         WDATA(MWDATA,5) = UOB
         WDATA(MWDATA,6) = VOB
         WDATA(MWDATA,7) = STAID
         WDATA(MWDATA,8) = TIME
         IF(MWDATA.EQ.NOUT) WRITE(83) WDATA,WTYPE
      ENDIF

C  STORE PRECIPITABLE WATER DATA
C  -----------------------------

      IF(QPW) THEN
         NPWDAT = NPWDAT+1
         MPWDAT = MAX(MOD(MPWDAT+1,NOUT+1),1)
         PWMERR(MPWDAT  ) = 12.
         PWTYPE(MPWDAT  ) = KX
         PWDATA(MPWDAT,1) = 4
         PWDATA(MPWDAT,2) = RLON
         PWDATA(MPWDAT,3) = RLAT
         PWDATA(MPWDAT,4) = PWO
         PWDATA(MPWDAT,5) = STAID
         PWDATA(MPWDAT,6) = TIME
         IF(MPWDAT.EQ.NOUT) WRITE(86) PWDATA,PWTYPE,PWMERR
      ENDIF
      ENDDO

      ENDDO
      GOTO 10
      ENDDO

      CALL CLOSBF(INBUFR)

C  MAKE SURE TO FLUSH THE PIPES AND CLOSE THE FAUCETS
C  --------------------------------------------------

      MTDATA = MOD(MTDATA,NOUT)
      MSDATA = MOD(MSDATA,NOUT)
      MWDATA = MOD(MWDATA,NOUT)
      MPDATA = MOD(MPDATA,NOUT)
      MQDATA = MOD(MQDATA,NOUT)
      MPWDAT = MOD(MPWDAT,NOUT)

      IF(MTDATA.GT.0) WRITE(81) ((TDATA(I,J),I=1,MTDATA),J=1,7),
     .                (TTYPE(I),I=1,MTDATA),(IQTFLG(I),I=1,MTDATA)
      IF(MSDATA.GT.0) WRITE(82) ((SDATA(I,J),I=1,MSDATA),J=1,5),
     .                (STYPE(I),I=1,MSDATA)
      IF(MWDATA.GT.0) WRITE(83) ((WDATA(I,J),I=1,MWDATA),J=1,8),
     .                (WTYPE(I),I=1,MWDATA)
      IF(MPDATA.GT.0) WRITE(84) ((PSDATA(I,J),I=1,MPDATA),J=1,8),
     .                (PSTYPE(I),I=1,MPDATA)
      IF(MQDATA.GT.0) WRITE(85) ((QDATA(I,J),I=1,MQDATA),J=1,7),
     .                (QTYPE(I),I=1,MQDATA),(QMAXERR(I),I=1,MQDATA)
      IF(MPWDAT.GT.0) WRITE(86) ((PWDATA(I,J),I=1,MPWDAT),J=1,6),
     .                (PWTYPE(I),I=1,MPWDAT),(PWMERR(I),I=1,MPWDAT)

      CLOSE(81)
      CLOSE(82)
      CLOSE(83)
      CLOSE(84)
      CLOSE(85)
      CLOSE(86)

C  NORMAL EXIT
C  -----------

1000  PRINT *,' RDTEST - NUMBER OF PREPDA DTA RECORDS READ=',NMRECS
      PRINT *,' NTDATA= ',NTDATA
      PRINT *,' NSDATA= ',NSDATA
      PRINT *,' NWDATA= ',NWDATA
      PRINT *,' NPDATA= ',NPDATA
      PRINT *,' NQDATA= ',NQDATA
      PRINT *,' NPWDAT= ',NPWDAT
      PRINT *,' NQTDATA=',NQTDATA
      PRINT *,' NSPROF= ',NSPROF
      PRINT *
      RETURN
      END
