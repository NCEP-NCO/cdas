      subroutine satcov(jcap,nlath,nlon,cshat,
     *  rlats,pln,wgts,trigs,ifax,rlkm,lmad)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    satcov     set up error for satems
c   prgmmr: parrish          org: w/nmc22    date: 90-10-06
c
c abstract: set up grid error and correlation spectrum for satems
c     (start with transform of soar function)
c
c program history log:
c   90-10-06  parrish
c
c   input argument list:
c     jcap     - triangular truncation
c     nlath    - number of gaussian lats in one hemisphere
c     nlon     - number of longitudes
c     ap,bp    - recursion constants for spherical harmonics
c     slat     - sin(gaussian latitudes)
c     pe0      - starting functions for spherical harmonics
c     wgts     - gaussian integration weights
c     trigs,ifax - used by fft
c     rlats    - grid latitudes (radians)
c     rlkm     - sat error correlation length scale
c     lmix,lastmix,lpairs - used for multitasking
c
c   output argument list:
c     cshat    - sat. correlation function array
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      real cshat((jcap+1)*(jcap+2))
      dimension lmad(0:jcap,0:jcap)
      real csn(0:jcap),cst(0:jcap)
      real pln((jcap+1)*(jcap+2),nlath)
      real wgts(nlath*2),trigs(2*nlon),rlats(2*nlath)
      real cgrid(2*nlath+1,nlon+2)
      integer ifax(10)
c--------
c        rlkm=400.
c--------        rlkm is length parameter for soar (in km)
      nc=(jcap+1)*(jcap+2)
      rl=rlkm/6370.
      if(rlkm .eq. 0.)then
       csn=1.
      else
c--------         rl is length in radians
c--------
c-------- compute correlation function on grid.
c--------
       pih=2.*atan(1.)
       do j=1,nlath*2
         arg=(pih-rlats(j))/rl
         corsat=(1.+arg)*exp(-arg)
c       print *,j,corsat
        do i=1,nlon
         cgrid(j,i)=corsat
        end do
       end do
       call g2s0(cshat,cgrid,jcap,nlon,nlath,wgts,pln,trigs,ifax)
c--------
c-------- beyond wave 30, error is infinite
c--------
       do n=31,jcap
        cshat(lmad(n,0))=cshat(lmad(30,0))
       end do
       do n=0,jcap
        csn(n)=sqrt(2./(2.*n+1.))*cshat(lmad(n,0))
        csn(n)=max(csn(n),5.e-4*csn(0))
       end do
      end if
      write(6,110)rlkm,jcap
110   format(' satem error covar spectrum follows for l=',f5.0,
     *    ' km, jcap=',i3)
      write(6,120)(csn(n),n=0,jcap)
120   format(1h ,5e12.4)
      do m=0,jcap
       do l=0,jcap-m
        cshat(lmad(m,l))=csn(m+l)
        cshat(lmad(m,l)+1)=csn(m+l)
       end do
      end do
c--------
c-------- multiply l=0 part of spectrum by 2
c--------
      do i=1,nc
       if(cshat(i) .ne. 0.)cshat(i)=1./cshat(i)
      end do
      do m=0,jcap
       cshat(lmad(m,0))=2.*cshat(lmad(m,0))
      end do
c--------
c-------- compute diagonal for normalization
c--------
         call s2mg2x(cshat,cgrid,jcap,nlath,nlon,pln)
         rnmax=-1.e12
         rnmin=1.e12
         do 1150 i=1,nlon
          do 1150 j=1,2*nlath
           rnmax=max(cgrid(j,i),rnmax)
           rnmin=min(cgrid(j,i),rnmin)
1150     continue
         write(6,1160)rlkm,jcap,rnmax,rnmin
1160     format(' satem error l= ',f5.0,' km, jcap= ',i3,
     *     ' max, min of h*chat*htrans=',3e11.3)
         factor=1./rnmax
         cshat=factor*cshat
c        call s2mg2x(cshat,cgrid,jcap,nlath,nlon,ap,bp,slat,pe0)
c        rnmax=-1.e12
c        rnmin=1.e12
c        do 2150 i=1,nlon
c         do 2150 j=1,2*nlath
c          rnmax=max(cgrid(j,i),rnmax)
c          rnmin=min(cgrid(j,i),rnmin)
c150     continue
c        write(6,2160)rnmax,rnmin
c160     format(' after normalization, max, min of h*chat*htrans=',
c    *          2e11.3)
      return
      end
