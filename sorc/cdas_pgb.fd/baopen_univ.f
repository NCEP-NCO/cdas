C-----------------------------------------------------------------------
      SUBROUTINE BAOPEN(LU,CFN,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: BAOPEN         BYTE-ADDRESSABLE OPEN
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 94-04-01
C
C ABSTRACT: OPEN A FILE TO BE ACCESSED BY BAREAD OR BAWRITE
C
C PROGRAM HISTORY LOG:
C   96-10-01  IREDELL     STANDARD F77 VERSION
C
C USAGE:    CALL BAOPEN(LU,CFN,IRET)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO OPEN
C     CFN          CHARACTER (*) FILE NAME TO OPEN
C   OUTPUT ARGUMENTS:
C     IRET         INTEGER RETURN CODE (0 IF SUCCESSFUL)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
      CHARACTER CFN*(*)
c     CHARACTER CFORM*11
c     PARAMETER(CFORM='UNFORMATTED')
c     OPEN(LU,FILE=CFN,IOSTAT=IRET,ACCESS='DIRECT',RECL=1,FORM=CFORM,
c    1		status='unknown')
	open(lu,file=cfn,form='unformatted')
      END
