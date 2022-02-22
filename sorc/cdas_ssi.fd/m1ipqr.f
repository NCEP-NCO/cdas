      subroutine m1ipqr(pe,qe,ro,slat,clat,nlath,jcap)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    m1ipqr     initialize legendre recursions
c   prgmmr: parrish          org: w/nmc22    date: 90-09-21
c
c abstract: initialize legendre recursions.
c
c program history log:
c   90-09-21  parrish
c
c   input argument list:
c     slat,clat - sin and cos of latitudes where functions are given
c     nlath    - number of latitudes where functions are evaluated
c     jcap     - triangular truncation
c
c   output argument list:
c     pe,qe,ro - starting functions for spherical harmonic recursions
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
        dimension pe(nlath,0:jcap),qe(nlath,0:jcap),ro(nlath,0:jcap)
        dimension slat(nlath),clat(nlath)
c-------
        rerth=conmc('rerth$')
        do 60 j=1,nlath
          pe(j,0)=1./sqrt(2.)
          qe(j,0)=0.
          qe(j,1)=rerth*sqrt(3./16.)
          do 30 l=1,jcap
            pe(j,l)=sqrt((2.*l+1.)/(2.*l))*clat(j)*pe(j,l-1)
30        continue
          do 40 l=2,jcap
            qe(j,l)=sqrt((2.*l+1.)/(2.*l))*(l/(l+1.))*clat(j)
     *                       *qe(j,l-1)
 40       continue
          do 50 l=0,jcap
            ro(j,l)=-qe(j,l)*slat(j)
 50       continue
60      continue
      return
      end
