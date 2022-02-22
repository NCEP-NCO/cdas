      PROGRAM SIGTIME
C
C  GETPRP READS LABEL,prints VALID TIME YYYY MMDDHH
C
c	should be compile with real*8 or real*4 float and integer*4 
c
	real    rinc(5)
	integer jdat(8),idat(8)

	real fhour

c	skip 1st record
	READ(11,iostat=ierr)
	if (ierr.ne.0) call abortc('i/o reading label')
	idat=0
	READ(11,iostat=ierr) FHOUR,idat(5),idat(2),idat(3),idat(1)

	if (ierr.ne.0) call abortc('i/o reading date record')
	rinc=0
	rinc(2)=FHOUR
        call w3movdat(rinc,idat,jdat)
C
	print '(i4,3i2.2)', jdat(1),jdat(2),jdat(3),jdat(5)
	stop
	END
	subroutine abortc(string)
	character string*(*)
	write(*,*) string
	stop 77
	end
