      subroutine getlalo(rlats,rlons,wgts,jcap,nlon,nlath,
     *    del2out,trigs,ifax,pln,qln,rln)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    getlalo    return gaussian lats and lons and trancons
c   prgmmr: parrish          org: w/nmc22    date: 90-09-21
c
c abstract: return gaussian lats, lons, and transform stuff.
c
c program history log:
c   90-09-21  parrish
c
c   input argument list:
c     jcap     - triangular truncation
c     nlon     - number of longitudes
c     nlath    - number of gaussian lats in one hemisphere
c
c   output argument list:
c     rlats,rlons    - grid latitudes, longitudes (radians)
c     wgts     - gaussian integration weights
c     pln,qln,rln - spherical harmonics.
c     del2     - n*(n+1)/(a**2)
c     trigs,ifax - used by fft
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      dimension rlats(2*nlath),wgts(2*nlath),rlons(nlon)
      dimension ap(0:jcap,0:jcap),bp(0:jcap,0:jcap)
      dimension aqr(0:jcap,0:jcap),bqr(0:jcap,0:jcap)
      dimension gr(0:jcap,0:jcap),del2(0:jcap,0:jcap)
      dimension slat(nlath),clat(nlath),trigs(nlon*2),ifax(10)
      dimension pe0(nlath,0:jcap),qe0(nlath,0:jcap),ro0(nlath,0:jcap)
      dimension pln((jcap+1)*(jcap+2),nlath)
      dimension qln((jcap+1)*(jcap+2),nlath)
      dimension rln((jcap+1)*(jcap+2),nlath)
      dimension del2out((Jcap+1)*(jcap+2))
c--------
      dimension w1(nlath-1),w2(nlath-1),w3(nlath-1),w4(nlath-1)
c--------
c-------- get stuff for fft*s
c-------
         call fftfax(nlon,ifax,trigs)
c--------
c-------- compute recursion constants and initialize legendre functions
c--------
      call m1rcons(ap,bp,aqr,bqr,gr,del2,jcap)
c--------
c-------- get latitudes, longitudes, and integration weights.
c-------
      pih=2.*atan(1.)
      call m1glat(nlath-1,w1,w2,w3,w4)
      rlats(1)=-pih
      wgts(1)=0.
ccdir$ ivdep
      do 100 i=1,nlath-1
        rlats(i+1)=w1(i)-pih
        wgts(i+1)=w2(i)
100   continue
ccdir$ ivdep
      do 200 i=1,nlath
        rlats(nlath*2+1-i)=-rlats(i)
        wgts(nlath*2+1-i)=wgts(i)
        slat(i)=sin(rlats(i))
        clat(i)=cos(rlats(i))
200   continue
      dlon=8.*atan(1.)/nlon
      do 300 i=1,nlon
        rlons(i)=(i-1.)*dlon
300   continue
c--------
c-------- next get initial p,q,r.
c--------
      call m1ipqr(pe0,qe0,ro0,slat,clat,nlath,jcap)
c-------- finally get pln,qln,rln
      call getpln(pln,qln,rln,jcap,nlath,ap,bp,slat,pe0,
     *          qe0,ro0,aqr,bqr,gr,clat,del2,del2out)
      return
      end
