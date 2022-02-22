C-----------------------------------------------------------------------
      SUBROUTINE WRYTE(LU,LC,C)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: WRYTE          WRITE DATA OUT BY BYTES
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: EFFICIENTLY WRITE UNFORMATTED A CHARACTER ARRAY.
C
C PROGRAM HISTORY LOG:
C   92-10-31  IREDELL
C   95-10-31  IREDELL     WORKSTATION VERSION
C   98-05-24  EBISUZAKI   ANSI F77 VERSION
C
C USAGE:    CALL WRYTE(LU,LC,C)
C   INPUT ARGUMENTS:
C     LU           INTEGER UNIT TO WHICH TO WRITE
C     LC           INTEGER NUMBER OF BYTES TO WRITE
C     C            CHARACTER*1 (LC) BUFFER TO WRITE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C   MACHINE: WORKSTATIONS
C
C$$$
      CHARACTER*1 C(LC)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	write(lu) c
c     CALL BAWRITE(LU,-1,LC,IWRT,C)
c     IF (LC.NE.IWRT) WRITE(*,*) 'WRYTE ERROR'
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
