      subroutine inguess(gu,gv,gt,gp,gq,gmtns,sigi,sigl,
     *  inges,jcap,nsig,nlath,nlon,hourg,idateg,
     *   pln,qln,rln,trigs,ifax,
     *     ml2lm,factslm,factvlm)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    inguessv   same as inguess, but add vort,div, del(ps)
c   prgmmr: parrish          org: w/nmc22    date: 94-02-11
c
c abstract: augment inguess with vort, div, grad (log(psfc))
c
c program history log:
c   94-02-11  parrish
c
c   input argument list:
c     inges    - unit number of guess coefs
c     jcap     - triangular truncation
c     nsig     - number of sigma levels
c     nlath    - number of gaussian lats in one hemisphere
c     nlon     - number of longitudes
c     ap,bp,aqr,bqr,gr - recursion constants for spherical harmonics
c     slat,clat - sin and cos of gaussian latitudes
c     pe0,qe0,ro0 - starting functionf for spherical harmonic recursions
c     trigs,ifax - used by
c     del2     - n*(n+1)/a**2
c
c   output argument list:
c     gu       - guess u on grid
c     gv       - guess v on grid
c     gt       - guess t on grid
c     gp       - guess log(sfcp) on grid
c     gq       - guess specific humidity on grid
c     gmtns    - guess mountains
c     vortb,divb, plonb,platb - guess vort,div, grad(log(psfc))
c     sigi     - sigma values at interfaces of  sigma layers
c     sigl     - sigma values at mid-point of each sigma layer
c     hourg    - hour of guess field
c     idateg   - date of guess field
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      dimension gu(2*nlath+1,nlon+2,nsig)
      dimension gv(2*nlath+1,nlon+2,nsig)
      dimension gt(2*nlath+1,nlon+2,nsig)
      dimension gp(2*nlath+1,nlon+2)
      dimension gq(2*nlath+1,nlon+2,nsig)
      dimension gmtns(2*nlath+1,nlon+2)
      dimension sigl(nsig),sigi(nsig+1)
      integer idateg(4)
      dimension del2((jcap+1)*(jcap+2))
      dimension trigs(nlon*2),ifax(10)
      dimension ml2lm((jcap+1)*(jcap+2))
      dimension factslm((jcap+1)*(jcap+2))
      dimension factvlm((jcap+1)*(jcap+2))
c--------
c-------- local space
c--------
      real zc((jcap+1)*(jcap+2),nsig)
      real dc((jcap+1)*(jcap+2),nsig)
      real tc((jcap+1)*(jcap+2),nsig)
      real qc((jcap+1)*(jcap+2),nsig)
      real pc((jcap+1)*(jcap+2))
      real rc((jcap+1)*(jcap+2))
      character*4 on85(8)
      real pln((jcap+1)*(jcap+2),nlath)
      real qln((jcap+1)*(jcap+2),nlath)
      real rln((jcap+1)*(jcap+2),nlath)
c--------
c-------- read in guess, putting into internal format.
c--------
      call rdgesc(zc,dc,tc,qc,pc,rc,hourg,idateg,sigi,sigl,
     *  inges,jcap,nsig,on85,ml2lm,factslm,factvlm)
c--------
c-------- reconstruct variables on grid
c--------
ccmic$ do all shared (nsig,pc,gp,jcap,nlon,nlath,pln,trigs,ifax)
ccmic$*       shared (rc,gmtns,tc,gt,qc,gq,zc,dc,gu,gv,qln,rln)
ccmic$*       private(kk,k)
         do kk=1,nsig*3+2
          if(kk.eq.3*nsig+1) 
     *      call s2g0(pc,gp,jcap,nlon,nlath,pln,trigs,ifax)
          if(kk.eq.3*nsig+2) 
     *      call s2g0(rc,gmtns,jcap,nlon,nlath,pln,trigs,ifax)
          k=mod(kk-1,nsig)+1
          if(kk.ge.1.and.kk.le.nsig) 
     *      call s2g0(tc(1,k),gt(1,1,k),jcap,nlon,nlath,pln,
     *                 trigs,ifax)
          if(kk.ge.nsig+1.and.kk.le.2*nsig)
     *      call s2g0(qc(1,k),gq(1,1,k),jcap,nlon,nlath,pln,
     *                 trigs,ifax)
          if(kk.ge.2*nsig+1.and.kk.le.3*nsig)
     *      call s2gvec(zc(1,k),dc(1,k),gu(1,1,k),gv(1,1,k),
     *       jcap,nlon,nlath,qln,rln,trigs,ifax)
         end do
      return
      end
