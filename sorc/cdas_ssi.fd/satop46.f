      subroutine satop4(msat,nlath,nlon,nsig,rt,cshat,
     *    pln,trigs,ifax,jcap,isatv,sfile)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    satop4     multiply by inverse of sat cov. matrix
c   prgmmr: parrish          org: w/nmc22    date: 90-10-11
c
c abstract: multiply t by inverse of sat. error covariance matrix.
c     i.e., inclusion of vertically correlated error
c
c program history log:
c   90-10-11  parrish
c   92-07-21
c
c   input argument list:
c     msat     - number of satellite profiles               
c     nlath    - number of gaussian lats in one hemisphere
c     nlon     - number of longitudes
c     nsig     - number of sigma levels
c     rt       - input temperature correction field        
c     cshat    - diagonal spectral error covariance matrix (inverse)
c     pln      - spherical harmonics
c     trigs,ifax - used by fft
c     jcap     - spectral trunctation
c     isatv    - array flags for use of sat. covariance
c     iscra    - unit number for conventional scratch unit
c
c   output argument list:
c     rt       - output vector after inclusion of sat. info.
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
c--------
c-------- multiply input vector t by inverse of correlated obs error
c--------
      real rt(2*nlath+1,nlon+2,nsig)
      dimension cshat((jcap+1)*(jcap+2)),isatv(nsig)
      real pln((jcap+1)*(jcap+2),nlath)
      dimension trigs(nlon*2),ifax(10)
      dimension sfile(*)
c--------
c-------- local space
c
c     dimension indl(nsig)
      real rr(2*nlath+1,nlon+2,nsig)
      real st(2*nlath+1,nlon+2,nsig)
      ngrd=(2*nlath+1)*(nlon+2)
      st=rt
      rt=0.
      if(msat .eq. 0)go to 440
      rr=0.
      iacnt=0
      do 610 ll=1,msat
      iacnt=iacnt+1
      numt=sfile(iacnt)
      do 602 lll=1,numt
c     nlevs=sfile(1+iacnt)
      jlat=sfile(1+iacnt)
      jlon=sfile(2+iacnt)
      ibeg=sfile(3+iacnt)
      iacnt=iacnt+3+nsig
      nxig=nsig-ibeg+1
      call dgemv('T',nxig,nxig,1.,sfile(iacnt+1),nxig,
     *     st(jlat,jlon,ibeg),ngrd,1.,rr(jlat,jlon,ibeg),ngrd)
c     do 644 n=1,nlevs
c44   indl(n)=sfile(iacnt+n)
c     iacnt=iacnt+2*nlevs
c     do 133 llm=1,nsig
c     do 133 llx=1,nsig
c     rr(jlat,jlon,llm)=rr(jlat,jlon,llm)+
c    *     st(jlat,jlon,llx)
c    *     *sfile((llm-1)*nsig+llx+iacnt)
c133  continue
      iacnt=iacnt+nsig*nsig
 602  continue
 610  continue
 611  continue
      call satc(rr,nsig,jcap,nlon,nlath,
     *            pln,trigs,ifax,cshat,isatv)
      iacnt=0
      do 510 ll=1,msat
      iacnt=iacnt+1
      numt=sfile(iacnt)
      do 102 lll=1,numt
c     nlevs=sfile(1+iacnt)
      jlat=sfile(1+iacnt)
      jlon=sfile(2+iacnt)
      ibeg=sfile(3+iacnt)
      iacnt=iacnt+3+nsig
      nxig=nsig-ibeg+1
      call dgemv('N',nxig,nxig,1.,sfile(iacnt+1),nxig,
     *     rr(jlat,jlon,ibeg),ngrd,1.,rt(jlat,jlon,ibeg),ngrd)
c     do 44 n=1,nsigs
c4    indl(n)=sfile(iacnt+n)
c     iacnt=iacnt+2*nlevs
c     do 1133 llm=1,nsig
c     do 1133 llx=1,nsig
c     rt(jlat,jlon,llm)=rt(jlat,jlon,llm)+
c    *     rr(jlat,jlon,llx)*sfile((llx-1)*nsig+llm+iacnt)
c133  continue
      iacnt=iacnt+nsig*nsig
 102  continue
 510  continue
 511  continue
440   continue
      return
      end
