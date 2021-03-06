      subroutine m1rcons(ap,bp,aqr,bqr,gr,del2,jcap)
C...Translated by FPP 6.0 (3.06G3) 08/09/95  14:56:24    
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    m1rcons    compute legendre generator constants
c   prgmmr: parrish          org: w/nmc22    date: 90-09-21
c
c abstract: get generator constants needed for legendre transforms
c
c program history log:
c   90-09-21  parrish
c
c   input argument list:
c     jcap     - triangular truncation
c
c   output argument list:
c     ap,bp,aqr,bqr,gr - various recursion constants
c     del2     - n*(n+1)/(a**2)
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      dimension ap(0:jcap,0:jcap)
      dimension bp(0:jcap,0:jcap)
      dimension aqr(0:jcap,0:jcap)
      dimension bqr(0:jcap,0:jcap)
      dimension gr(0:jcap,0:jcap)
      dimension del2(0:jcap,0:jcap)
      integer j1x, j2x, j3x
c--------
      write(*,*) 'm1rcons : jcap=', jcap
      rerth=conmc('rerth$')
      DO J2X = 1, JCAP*(JCAP + 2) + 1
         AP(J2X-1,0) = 0.
         BP(J2X-1,0) = 0.
         AQR(J2X-1,0) = 0.
         BQR(J2X-1,0) = 0.
         GR(J2X-1,0) = 0.
         DEL2(J2X-1,0) = 0.
      END DO
      do 20 m=0,jcap
        do 10 l=0,jcap-m
          n=m+l
          ap(m,l)=sqrt((2.*n+1.)*(2.*n+3.)/
     *         ((n-l+1.)*(n+l+1.)))
          bp(m,l)=-sqrt((n-l)*(n+l)*(2.*n+3.)/
     *         ((n-l+1.)*(n+l+1.)*max(1.,2.*n-1.)))
          aqr(m,l)=ap(m,l)*n/(n+2.)
          bqr(m,l)=bp(m,l)*n*(n-1.)/((n+1.)*(n+2.))
          gr(m,l)=rerth*ap(m,l)/((n+1.)*(n+2.))
          del2(m,l)=n*(n+1.)/(rerth*rerth)
10      continue
20    continue
      do 40 m=0,jcap-1
        mii=jcap-m
      IF (M.NE.MII .OR. JCAP-M.LT.1+M .OR. JCAP.LT.1) THEN
         DO 30 L = 1, JCAP - M
            DEL2(MII,JCAP+1-L) = DEL2(M,L)
   30    CONTINUE
      ELSE
         DO L = 1, JCAP - M
            LII = JCAP + 1 - L
            DEL2(MII,LII) = DEL2(M,L)
         END DO
      ENDIF
40    continue
      return
      end
