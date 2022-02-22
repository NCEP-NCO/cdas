      subroutine qoper(u,v,vort,t,plon,plat,nsig,jcap,nlon,nlath,
     *  pln,qln,rln,trigs,ifax,del2,wgts,a3,sigl,sigi,ds,iback,rlats,
     *   rus,rvs,rts,rvorts,rplons,rplats)
c-------------------
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    qoper   tangent linear model for divergency tendency
c   prgmmr: parrish          org: w/nmc22    date: 94-02-12
c
c abstract: tangent linear model (tlm) for divergence tendency.
c         special note: vertical advection terms not included yet
c-----                       (they are included in fulldivt computation)
c
c program history log:
c   94-02-12  parrish
c
c   input argument list:
c     u,v,vort,t,plon,plat - perturbation u,v, etc. on gaussian grid
c     nsig     - number of sigma layers
c     jcap     - triangular truncation
c     nlon     - number of longitudes
c     nlath    - number of gaussian lats in one hemisphere
c     pln,qln,rln - spherical harmonics
c     trigs,ifax - used by fft
c     del2     - n*(n+1)/a**2
c     wgts     - gaussian integration weights
c     a3       - hydrostatic matrix
c     sigl,sigi - vertical coordinate stuff
c     iback    - unit number where reference fields are stored
c
c   output argument list:
c     ds       - perturbation divergence tendency coefficients
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      dimension sigl(nsig),sigi(nsig+1)
      real t(2*nlath+1,nlon+2,nsig)
      dimension a3(nsig,nsig)
      dimension del2((jcap+1)*(jcap+2))
      dimension wgts(2*nlath)
      dimension u(2*nlath+1,nlon+2,nsig)
      dimension v(2*nlath+1,nlon+2,nsig)
      dimension vort(2*nlath+1,nlon+2,nsig)
      dimension plon(2*nlath+1,nlon+2)
      dimension plat(2*nlath+1,nlon+2)
      dimension trigs(nlon*2),ifax(10)
      dimension pln((jcap+1)*(jcap+2),nlath)
      dimension qln((jcap+1)*(jcap+2),nlath)
      dimension rln((jcap+1)*(jcap+2),nlath)
      dimension ds((jcap+1)*(jcap+2),nsig)
      dimension rlats(2*nlath)
      dimension rus(2*nlath+1,nlon+2,nsig)
      dimension rvs(2*nlath+1,nlon+2,nsig)
      dimension rts(2*nlath+1,nlon+2,nsig)
      dimension rvorts(2*nlath+1,nlon+2,nsig)
      dimension rplons(2*nlath+1,nlon+2)
      dimension rplats(2*nlath+1,nlon+2)
c--------
c-------- internal scratch dynamic space follows:
c--------
      dimension ts((jcap+1)*(jcap+2),nsig)
      dimension ps((jcap+1)*(jcap+2))
      dimension uw(2*nlath+1,nlon+2,nsig),vw(2*nlath+1,nlon+2,nsig)
      dimension pw(2*nlath+1,nlon+2)
      dimension coriolis(2*nlath+1,nlon+2)
c--------
         ng=(2*nlath+1)*nlon
         nc=(jcap+1)*(jcap+2)
         omega=conmc('omega$')
         gascon=conmc('rd$')
         eaccel=9.8
ccmic$  do all shared (nlath,coriolis,nlon,omega,rlats,uw,gascon,t)
ccmic$*        shared (rplons,rts,plon,rvs,vort,v,rvorts,vw,rus,rplats)
ccmic$*        shared (plat,u,eaccel,a3,t,nsig)
ccmic$*        private (j,k,i,l)
         do j=1,2*nlath
          coriolis(j,1:nlon)=2.*omega*sin(rlats(j))
c--------
c-------- compute ud,vd, bige        
c------------------------compute full non-lin bal eq  
          do k=1,nsig
           do i=1,nlon
            uw(j,i,k)=-gascon*t(j,i,k)*rplons(j,i)
     *           -gascon*rts(j,i,k)*plon(j,i)
     *           +rvs(j,i,k)*vort(j,i,k)
     *           +v(j,i,k)*(rvorts(j,i,k)+coriolis(j,i))
            vw(j,i,k)=-rus(j,i,k)*vort(j,i,k)
     *           -gascon*t(j,i,k)*rplats(j,i)
     *           -gascon*rts(j,i,k)*plat(j,i)
     *            -u(j,i,k)*(rvorts(j,i,k)+coriolis(j,i))
            vort(j,i,k)=u(j,i,k)*rus(j,i,k)
     *           +v(j,i,k)*rvs(j,i,k)
           end do
          end do
          do k=1,nsig
           do l=1,nsig
            do i=1,nlon
             vort(j,i,k)=vort(j,i,k)+eaccel*a3(k,l)*t(j,i,l)
            end do
           end do
          end do
         end do
c---------
c--------- now get div (ud,vd) - del2 (bige)  (bige stored in vort)
c---------
ccmic$ do all shared (nsig,ts,vort,jcap,nlon,nlath,wgts,pln,trigs,ifax)
ccmic$*       shared (ds,uw,vw,qln,rln,del2,nc)
ccmic$*       private (k)
         do k=1,nsig
          call g2s0(ts(1,k),vort(1,1,k),jcap,nlon,nlath,
     *         wgts,pln,trigs,ifax)
          call grad2s(ds(1,k),uw(1,1,k),vw(1,1,k),jcap,nlon,nlath,
     *      qln,rln,trigs,ifax,wgts,del2)
          ts(1:nc,k)=del2(1:nc)*ts(1:nc,k)
          ds(1:nc,k)=ds(1:nc,k)+ts(1:nc,k)
         end do
       return
       end
