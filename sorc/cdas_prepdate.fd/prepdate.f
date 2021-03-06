C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM:  CDAS_PREPDATE
C   PRGMMR: KEYSER           ORG: NP22        DATE: 2003-09-02
C
C ABSTRACT: READS THE SECTION 1 DATE OUT OF THE FIRST DATA MESSAGE IN
C   A PREPBUFR FILE AND WRITES IT TO UNIT 51 IN THE FORM YYYYMMDDHH.
C
C PROGRAM HISTORY LOG:
C ????-??-??  ?????????   ORIGINAL AUTHOR
C 2003-09-02  D. KEYSER   MODIFIED TO LINK TO PRODUCTION BUFRLIB
C                         RATHER THAN USE IN-LINE VERSIONS OF SOME
C                         ROUTINES; MODIFIED TO WRITE DATE TO UNIT 51
C                         RATHER THAN STDOUT (UNIT 06) SINCE PARENT
C                         SCRIPT COULD HAVE FAILED IF SOME OTHER
C                         PRINT INFO WERE IN STDOUT (SCRIPT HAD
C                         TESTED STDOUT AGAINST EXPECTED DATE, NOW
C                         IT TESTS THE CAT OF UNIT 51 AGAINST THE
C                         EXPECTED DATE; ADDED DOCBLOCK)
C
C USAGE:
C   INPUT FILES:
C     UNIT 11  - PREPBUFR FILE
C
C   OUTPUT FILES:
C     UNIT 06  - STANDARD OUTPUT PRINT
C     UNIT 51  - FILE WHICH CONTAINS PRPEBUFR DATE IN FORM YYYYMMDDHH
C
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
C     LIBRARY:
C       W3LIB    -  W3TAGB  W3TAGE
C       BUFRLIB  -  DATELEN DATEBF
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C            > 0 - ABNORMAL RUN
C
C REMARKS: NONE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM SP
C
C$$$

      PROGRAM CDAS_PREPDATE
 
      CHARACTER*8  SUBSET
      INTEGER*4 IDIR
 
      DATA LUNIN /11    /
 
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL W3TAGB('CDAS_PREPDATE',2003,0245,0061,'NP22')

      CALL DATELEN(10)

      CALL DATEBF(LUNIN,IY,IM,ID,IH,IDATE)
      WRITE(51,'(i4,3i2.2)') IY,IM,ID,IH

      CALL W3TAGE('CDAS_PREPDATE')

      STOP
      END
