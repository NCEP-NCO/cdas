      SUBROUTINE W3FI58(IFIELD,NPTS,NWORK,NPFLD,NBITS,LEN,KMIN)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK  ***
C                .      .    .                                       .
C SUBPROGRAM:  W3FI58   - PACK POSITIVE DIFFERENCES IN LEAST BITS
C   PRGMMR:  ALLARD, R.       ORG:  NMC411        DATE:  JULY 1987
C
C ABSTRACT:  CONVERTS AN ARRAY OF INTEGER NUMBERS INTO AN ARRAY OF
C   POSITIVE DIFFERENCES (NUMBER(S) - MINIMUM VALUE) AND PACKS THE
C   MAGNITUDE OF EACH DIFFERENCE RIGHT-ADJUSTED INTO THE LEAST
C   NUMBER OF BITS THAT HOLDS THE LARGEST DIFFERENCE.
C
C PROGRAM HISTORY LOG:
C   87-09-02  ALLARD
C   88-10-02  R.E.JONES   CONVERTED TO CDC CYBER 205 FTN200 FORTRAN
C   90-05-17  R.E.JONES   CONVERTED TO CRAY CFT77 FORTRAN
C   90-05-18  R.E.JONES   CHANGE NAME VBIMPK TO W3LIB NAME W3FI58
C   96-05-14  IREDELL     GENERALIZED COMPUTATION OF NBITS
C   98-06-30  EBISUZAKI   LINUX PORT
C
C USAGE:  CALL W3FI58(IFIELD,NPTS,NWORK,NPFLD,NBITS,LEN,KMIN)
C
C   INPUT:
C
C     IFIELD - ARRAY OF INTEGER DATA FOR PROCESSING
C     NPTS   - NUMBER OF DATA VALUES TO PROCESS IN IFIELD (AND NWORK)
C              WHERE, NPTS > 0
C
C   OUTPUT:
C
C     NWORK  - WORK ARRAY WITH INTEGER DIFFERENCE
C     NPFLD  - ARRAY FOR PACKED DATA (character*1)
C              (USER IS RESPONSIBLE FOR AN ADEQUATE DIMENSION.)
C     NBITS  - NUMBER OF BITS USED TO PACK DATA WHERE, 0 < NBITS < 32
C              (THE MAXIMUM DIFFERENCE WITHOUT OVERFLOW IS 2**31 -1)
C     LEN    - NUMBER OF PACKED BYTES IN NPFLD (SET TO 0 IF NO PACKING)
C              WHERE, LEN = (NBITS * NPTS + 7) / 8 WITHOUT REMAINDER
C     KMIN   - MINIMUM VALUE (SUBTRACTED FROM EACH DATUM). IF THIS
C              PACKED DATA IS BEING USED FOR GRIB DATA, THE
C              PROGRAMER WILL HAVE TO CONVERT THE KMIN VALUE TO AN
C              IBM370 32 BIT FLOATING POINT NUMBER.
C
C   SUBPROGRAMS CALLED:
C
C     W3LIB:  SBYTES, SBYTE
C
C   EXIT STATES:  NONE
C
C     NOTE:  LEN = 0, NBITS = 0, AND NO PACKING PERFORMED IF
C
C     (1) KMAX = KMIN  (A CONSTANT FIELD)
C     (2) NPTS < 1  (SEE INPUT ARGUMENT)
C
C ATTRIBUTES:
C   LANGUAGE: CRAY CFT77 FORTRAN
C   MACHINE:  CRAY Y-MP8/832
C
C$$$
C
      PARAMETER(ALOG2=0.69314718056)
      INTEGER  IFIELD(*)
      CHARACTER*1  NPFLD(*)
      INTEGER  NWORK(*)
C
      EQUIVALENCE (BIGDIF,IX)
C
      DATA  KZERO / 0 /
C
C / / / / / /
C
      LEN   = 0
      NBITS = 0
      IF (NPTS.LE.0) GO TO 3000
C
C FIND THE MAX-MIN VALUES IN INTEGER FIELD (IFIELD).
C
      KMAX = IFIELD(1)
      KMIN = KMAX
      DO 1000 I = 2,NPTS
        KMAX = MAX(KMAX,IFIELD(I))
        KMIN = MIN(KMIN,IFIELD(I))
 1000 CONTINUE
C
C IF A CONSTANT FIELD, RETURN WITH NO PACKING AND 'LEN' AND 'NBITS' SET
C TO ZERO.
C
      IF (KMAX.EQ.KMIN) GO TO 3000
C
C DETERMINE LARGEST DIFFERENCE IN IFIELD AND FLOAT (BIGDIF).
C
      BIGDIF = KMAX - KMIN
C
C NBITS IS COMPUTED AS THE LEAST INTEGER SUCH THAT
C   BIGDIF < 2**NBITS
C
      NBITS=LOG(BIGDIF+0.5)/ALOG2+1
C
C FORM DIFFERENCES IN NWORK ARRAY.
C
      DO 2000 K = 1,NPTS
        NWORK(K) = IFIELD(K) - KMIN
 2000 CONTINUE
C
C PACK EACH MAGNITUDE IN NBITS (NBITS = THE LEAST POWER OF 2 OR 'N')
C
      LEN=(NBITS*NPTS-1)/8+1
      CALL SBYTESC(NPFLD,NWORK,0,NBITS,0,NPTS)
C
C ADD ZERO-BITS AT END OF PACKED DATA TO INSURE A BYTE BOUNDARY.
C
      NOFF = NBITS * NPTS
      NZERO=LEN*8-NOFF
      IF(NZERO.GT.0) CALL SBYTEC(NPFLD,KZERO,NOFF,NZERO)
C
 3000 CONTINUE
      RETURN
C
      END
