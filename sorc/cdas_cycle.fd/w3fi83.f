      SUBROUTINE W3FI83 (DATA,NPTS,FVAL1,FDIFF1,ISCAL2,
     *                                ISC10,KPDS,KGDS)
C$$$  SUBPROGRAM DOCUMENTATION  BLOCK
C                .      .    .                                       .
C SUBPROGRAM:  W3FI83        RESTORE DELTA PACKED DATA TO ORIGINAL
C   PRGMMR: CAVANAUGH        ORG: NMC421      DATE:93-08-18
C
C ABSTRACT: RESTORE DELTA PACKED DATA TO ORIGINAL VALUES
C           RESTORE FROM BOUSTREPHEDONIC ALIGNMENT
C
C PROGRAM HISTORY LOG:
C   93-07-14  CAVANAUGH
C   93-07-22  STACKPOLE      ADDITIONS TO FIX SCALING
C   94-01-27  CAVANAUGH   ADDED REVERSAL OF EVEN NUMBERED ROWS
C                         (BOUSTROPHEDONIC PROCESSING) TO RESTORE
C                         DATA TO ORIGINAL SEQUENCE.
C   94-03-02  CAVANAUGH   CORRECTED REVERSAL OF EVEN NUMBERED ROWS
C   98-06-30  EBISUZAKI   LINUX PORT
C
C USAGE:    CALL W3FI83(DATA,NPTS,FVAL1,FDIFF1,ISCAL2,
C    *                                ISC10,KPDS,KGDS)
C   INPUT ARGUMENT LIST:
C     DATA     - SECOND ORDER DIFFERENCES
C     NPTS     - NUMBER OF POINTS IN ARRAY
C     FVAL1    - ORIGINAL FIRST ENTRY IN ARRAY
C     FDIFF1   - ORIGINAL FIRST FIRST-DIFFERENCE
C     ISCAL2   - POWER-OF-TWO EXPONENT FOR UNSCALING
C     ISC10    - POWER-OF-TEN EXPONENT FOR UNSCALING
C     KPDS     - ARRAY OF INFORMATION FOR PDS
C     KGDS     - ARRAY OF INFORMATION FOR GDS
C
C   OUTPUT ARGUMENT LIST:
C     DATA     - EXPANDED ORIGINAL DATA VALUES
C
C REMARKS:
C
C ATTRIBUTES:
C   LANGUAGE: HP FORTRAN 77
C   MACHINE:  Hewlett-Packard 9000/705, 715, 735, 750, 755, 712
C
C$$$
C
      REAL          FVAL1,FDIFF1
      REAL          DATA(*),BOUST(200)
      INTEGER       NPTS,NROW,NCOL,KPDS(*),KGDS(*),ISC10
C  ---------------------------------------
      SAVE
C
C     REMOVE DECIMAL UN-SCALING INTRODUCED DURING UNPACKING
C
      DSCAL = 10.0 ** ISC10
      IF (DSCAL.EQ.0.0) THEN
          DO 50 I=1,NPTS
              DATA(I) = 1.0
   50     CONTINUE
      ELSE IF (DSCAL.EQ.1.0) THEN
      ELSE
          DO 51 I=1,NPTS
              DATA(I) = DATA(I) * DSCAL
   51     CONTINUE
      END IF
C
      DATA(1)  = FVAL1
      DATA(2)  = FDIFF1
      DO 200 J = 3,2,-1
          DO 100 K = J, NPTS
              DATA(K)  = DATA(K) + DATA(K-1)
  100     CONTINUE
  200 CONTINUE
C
C     NOW REMOVE THE BINARY SCALING FROM THE RECONSTRUCTED FIELD
C     AND THE DECIMAL SCALING TOO
C
      IF (DSCAL.EQ.0) THEN
          SCALE  = 0.0
      ELSE
          SCALE =(2.0**ISCAL2)/DSCAL
      END IF
      DO 300 I=1,NPTS
        DATA(I) = DATA(I) * SCALE
  300 CONTINUE
C  ==========================================================
      IF (AND(KPDS(4),128).NE.0) THEN
          NROW  = KGDS(3)
          NCOL  = KGDS(2)
C
C      DATA LAID OUT BOUSTROPHEDONIC STYLE
C
C
C         PRINT*, '  REVERSE BOUSTROPHEDON'
          DO 210 I = 2, NROW, 2
C
C          REVERSE THE EVEN NUMBERED ROWS
C
              DO 201 J = 1, NCOL
                  NPOS  = I * NCOL - J + 1
                  BOUST(J) = DATA(NPOS)
  201         CONTINUE
              DO 202 J = 1, NCOL
                  NPOS  = NCOL * (I-1) + J
                  DATA(NPOS)  = BOUST(J)
  202         CONTINUE
  210     CONTINUE
C
C
      END IF
C  =================================================================
      RETURN
      END
