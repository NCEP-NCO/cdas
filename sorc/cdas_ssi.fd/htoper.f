      subroutine htoper(zs,ds,hs,qs,ps,u,v,vort,t,p,plon,plat,q,
     *     bhalf,bhalfp,nsig,jcap,nlon,nlath,del2,
     *     pln,qln,rln,trigs,ifax,
     *     agvz,wgvz,bvz,nmdszh,vz,vd,vh,vq,in,baln)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    htoper      transpose of hoper
c   prgmmr: parrish          org: w/nmc22    date: 90-10-06
c
c abstract: apply transpose of hoper, going from grid to spectral.
c
c program history log:
c   90-10-06  parrish
c
c   input argument list:
c     u,v,vort,t,p,plon,plat,q - u,v,etc on gaussian grid
c     bhalf    - background error stats
c     bhalfp   - background error stats surface pressure
c     nsig     - number of sigma levels
c     jcap     - triangular truncation
c     nlon     - number of longitudes
c     nlath    - number of gaussian lats in one hemisphere
c     del2     - n*(n+1)/a**2
c     trigs,ifax - used by fft
c     agvz     - mass-variable modes to temperature conversion
c     wgvz     - mass-variable modes to log(psfc) conversion
c     bvz      - mass-variable modes to divergence conversion
c     nmdszh   - number of modes used in balance eqn.
c     vz       - vertical mode matrix - z    
c     vd       - vertical mode matrix - d    
c     vh       - vertical mode matrix - temps
c     vq       - vertical mode matrix - q   
c     in       - total wavenumber index array
c     baln     - spectral balance operator constants
c
c
c   output argument list:
c     zs,ds,hs,qs,ps - coefs of vort, div, unbal t, unbal log(ps), q
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      real t(2*nlath+1,nlon+2,nsig),p(2*nlath+1,nlon+2)
      real plon(2*nlath+1,nlon+2),plat(2*nlath+1,nlon+2)
      dimension agvz(0:jcap,nsig,nmdszh)
      dimension wgvz(0:jcap,nmdszh)
      dimension bvz(0:jcap,nsig,nmdszh)
      dimension bhalf((jcap+1)*(jcap+2),nsig,4)
      dimension bhalfp((jcap+1)*(jcap+2))
      dimension zs((jcap+1)*(jcap+2),nsig)
      dimension ds((jcap+1)*(jcap+2),nsig)
      dimension hs((jcap+1)*(jcap+2),nsig)
      dimension qs((jcap+1)*(jcap+2),nsig)
      dimension ps((jcap+1)*(jcap+2))
      dimension u(2*nlath+1,nlon+2,nsig)
      dimension v(2*nlath+1,nlon+2,nsig)
      dimension vort(2*nlath+1,nlon+2,nsig)
      dimension q(2*nlath+1,nlon+2,nsig)
      dimension vz(nsig,nsig),vd(nsig,nsig)
      dimension vq(nsig,nsig),vh(nsig,nsig)
      dimension del2((jcap+1)*(jcap+2))
      dimension trigs(nlon*2),ifax(10)
      dimension pln((jcap+1)*(jcap+2),nlath)
      dimension qln((jcap+1)*(jcap+2),nlath)
      dimension rln((jcap+1)*(jcap+2),nlath)
      dimension in((jcap+1)*(jcap+2))
      dimension baln((jcap+1)*(jcap+2))
c--------
c-------- internal scratch dynamic space follows:
c--------
      dimension psd((jcap+1)*(jcap+2))
      dimension zsf((jcap+1)*(jcap+2),nmdszh)
      dimension work((jcap+1)*(jcap+2),nsig)
c--------

         nc=(jcap+1)*(jcap+2)
         ng=(2*nlath+1)*(nlon+2)
ccmic$ do all shared (nsig,psd,plon,plat,jcap,nlon,nlath,qln,rln,work)
ccmic$*     shared (trigs,ifax,ps,p,zs,vort,hs,t,qs,q,zs,ds,u,v,nc,pln)
ccmic$*       private(kk,k,i)
         do kk=1,nsig*3+2
          if(kk.eq.3*nsig+1)
     *      call ts2grad(psd,plon,plat,jcap,nlon,nlath,qln,rln,
     *            trigs,ifax)
          if(kk.eq.3*nsig+2)
     *      call ts2g0(ps,p,jcap,nlon,nlath,pln,trigs,ifax)
          k=mod(kk-1,nsig)+1
          if(kk.ge.1.and.kk.le.nsig) then
           call ts2g0(zs(1,k),vort(1,1,k),jcap,nlon,nlath,pln,
     *             trigs,ifax)
           call ts2gvec(work(1,k),ds(1,k),u(1,1,k),v(1,1,k),
     *          jcap,nlon,nlath,qln,rln,trigs,ifax)

           zs(:,k) = zs(:,k) + work(:,k)
          end if
          if(kk.ge.nsig+1.and.kk.le.2*nsig)
     *      call ts2g0(hs(1,k),t(1,1,k),jcap,nlon,nlath,pln,
     *              trigs,ifax)

          if(kk.ge.2*nsig+1.and.kk.le.3*nsig)
     *      call ts2g0(qs(1,k),q(1,1,k),jcap,nlon,nlath,pln,
     *              trigs,ifax)
         end do
         ps=ps-del2*psd
c--------
c-------- next do vertical transforms
c--------
c-------- tsum in vertical zs                       
      do j=1,nsig
         do i=1,nc
          work(i,j)=vz(1,j)*zs(i,1)
         end do
        do k=2,nsig
         do i=1,nc
          work(i,j)=vz(k,j)*zs(i,k)
     *             +work(i,j)
         end do
        end do
      end do
c--------
c-------- tsum in vertical qs                       
      do j=1,nsig
         do i=1,nc
          zs(i,j)=work(i,j)
         end do
         do i=1,nc
          work(i,j)=vq(1,j)*qs(i,1)
         end do
        do k=2,nsig
         do i=1,nc
          work(i,j)=vq(k,j)*qs(i,k)
     *             +work(i,j)
         end do
        end do
      end do
      qs=work
      work=ds
      ds=0.
      zsf=0.
c--------------- tsum in vertical ds                       
         do j=1,nsig
          do k=1,nsig
           do i=1,nc
            ds(i,j)=ds(i,j)+work(i,k)*vd(k,j)
           end do
          end do
          if(j.le.nmdszh) then
           do k=1,nsig
            do i=1,nc
             zsf(i,j)=zsf(i,j)+bvz(in(i),k,j)*work(i,k)
            end do
           end do
          end if
         end do
c---------------do ttemp and tpsfc
         work=hs
         hs=0.
         do j=1,nsig
          if(j.le.nmdszh) then
           do i=1,nc
            zsf(i,j)=zsf(i,j)+wgvz(in(i),j)*ps(i)
           end do
           do k=1,nsig
            do i=1,nc
             zsf(i,j)=zsf(i,j)+agvz(in(i),k,j)*work(i,k)
            end do
           end do
          end if
          do k=1,nsig
           do i=1,nc
            hs(i,j)=hs(i,j)+work(i,k)*vh(k,j)
           end do
          end do
         end do

c------------------------tapply spectral balance operator
c------------------------to zs
       
        do k=1,nmdszh
         ii0=2*(jcap+1)
         im0=0
         do m=1,jcap
          do ll=1,2*(jcap+1-m)
           zs(im0+ll,k)=zs(im0+ll,k)+baln(ii0+ll)*zsf(ii0+ll,k)
          end do
          ii0=ii0+2*(jcap+1-m)
          im0=im0+2*(jcap+2-m)
         end do
         ii0=0
         ip0=2*(jcap+1)
         do m=0,jcap-1
          do ll=1,2*(jcap-m)
           zs(ip0+ll,k)=zs(ip0+ll,k)+baln(ip0+ll)*zsf(ii0+ll,k)
          end do
          ii0=ii0+2*(jcap+1-m)
          ip0=ip0+2*(jcap-m)
         end do
        end do
      do j=1,nsig
       if(j .eq. 1)then
         do i=1,nc
           ps(i)=ps(i)*bhalfp(i)
         end do
       end if
       do i=1,nc
        zs(i,j)=zs(i,j)*bhalf(i,j,1)
        ds(i,j)=ds(i,j)*bhalf(i,j,2)
        hs(i,j)=hs(i,j)*bhalf(i,j,3)
        qs(i,j)=qs(i,j)*bhalf(i,j,4)
       end do
      end do
      return
      end
