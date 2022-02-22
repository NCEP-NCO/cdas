      subroutine iminv (a,n,det,l,m)
c
c     ..................................................................
c
c        ................
c
c        purpose
c           invert a matrix
c
c        usage
c           call iminv (a,n,d,l,m)
c
c        description of parameters
c           a - input matrix, destroyed in computation and replaced by
c               resultant inverse.
c           n - order of matrix a
c           det - resultant determinant
c           l - work vector of length n
c           m - work vector of length n
c
c        remarks
c           matrix a must be a general matrix
c
c        .............................................
c           none
c
c        method
c           the standard gauss-jordan method is used. the determinant
c           is also calculated. a determinant of zero indicates that
c           the matrix is singular.
c
c     ..................................................................
c
      dimension a(n,n),l(n),m(n)

c	note: running double precision by default

	call dgeicd(a, n, n, 2, rcond, det, junk, 0)

	return
	end
