      subroutine gtbhalf(ineofs,bhalf,bhalfp,jcap,nsig,nlath,a,
     *  jcapstat,nsigstat,agvz,wgvz,bvz,nmdszh,vz,vd,vh,vq,sigl)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    getbhalf   obtain bhalf and vert. functs.
c   prgmmr: parrish          org: w/nmc22    date: 90-10-06
c
c abstract: obtain bhalf, (sqrt(bhat)) and vertical functions
c
c program history log:
c   90-10-06  parrish
c
c   input argument list:
c     ineofs   - input file for input statistics
c     jcap     - triangular truncation
c     nsig     - number of sigma levels
c     nlath    - number of gaussian lats in one hemisphere
c     a        - a(4): scaling factors for forecast error spectra
c     jcapstat - triangular truncation for statistics
c     nsigstat - number of sigma levels for statistics
c     nmdszh   - number of vertical modes used in balance stats
c     sigl     - sigma layer values for model
c
c   output argument list:
c     bhalf    - sqrt(bhat) where bhat is background error spectrum
c     bhalfp   - sqrt(bhatp) where bhat is sur. press. back.error spectrum
c     agvz,wgvz,bvz - arrays to convert mass variable to t,ln(ps),div
c     vz,vd,vh,vq - vertical functions for vort,div, temp, and spec hum
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      dimension sigl(nsig)
      dimension aibw(nsig,nsig),w(nsig),vz(nsig,nsig)
      dimension vd(nsig,nsig),vh(nsig,nsig),vq(nsig,nsig)
      dimension chalf(0:jcapstat,0:jcapstat,nsig,4)
      dimension chalfp(0:jcapstat,0:jcapstat)
      dimension bhalf((jcap+1)*(jcap+2),nsig,4),a(4)
      dimension bhalfp((jcap+1)*(jcap+2))
      dimension beta(0:jcap,nsig),gamma(0:jcap,nsig)
      dimension gammac(0:jcapstat,nsig)
      dimension gammap(0:jcap),gammapc(0:jcapstat)
      dimension betac(0:jcapstat,nsig)
      dimension rlsg(nsig),tbar(nsig),a3(nsig,nsig)
      dimension ainv(nsig,nsig)
      dimension alphar(nsig),lwork(nsig),mwork(nsig)
      dimension agvz(0:jcap,nsig,nmdszh),wgvz(0:jcap,nmdszh)
      dimension bvz(0:jcap,nsig,nmdszh)
      integer*4 itmp4(4)
c--------
      print *,' read in vert eofs and horiz error spectra ',
     *     'from unit ',ineofs
      if(nsig.ne.nsigstat) then
      print *,' new vertical resolution, interpolate eofs'
      print *,' stopping code '
      end if
      rewind ineofs
c     read(ineofs)msig,mlath,mmdszh,kcap,rlsg,aibw,w,
      read(ineofs) (itmp4(i),i=1,4),rlsg,aibw,w,
     *   rogc,tbar,a3,vz,vd,vh,vq,chalf,chalfp
      msig = itmp4(1)
      mlath = itmp4(2)
      mmdszh = itmp4(3)
      kcap = itmp4(4)
      read(ineofs)betac,gammac,gammapc
      rewind ineofs

      print *,' for vert eofs, nsig=',msig,
     *     ', nmdszh=',mmdszh,', jcap=',kcap,', nlath=',mlath
      beta=0.
      do 190 k=1,nsig
      do 190 n=0,min(jcap,jcapstat)
        beta(n,k)=betac(n,k)
190   continue
c--------
c-------- now take care of horizontal part of stats
c--------
      gamma=0
      do 200 m=1,nsig
      do 200 n=0,min(jcap,jcapstat)
        gamma(n,m)=gammac(n,m)
200   continue
      do n=0,min(jcap,jcapstat)
       gammap(n)=gammapc(n)
      end do
      bhalf=0.
      do ll=1,4
       do k=1,nsig
        ii=-1
        do m=0,min(jcap,jcapstat)
         do l=0,min(jcap,jcapstat)-m
          ii=ii+2
          bhalf(ii,k,ll)=chalf(m,l,k,ll)
          bhalf(ii+1,k,ll)=bhalf(ii,k,ll)
         end do
        end do
       end do
      end do
      bhalfp=0.
      ii=-1
      do m=0,min(jcap,jcapstat)
       do l=0,min(jcap,jcapstat)-m
        ii=ii+2
        bhalfp(ii)=chalfp(m,l)
        bhalfp(ii+1)=bhalfp(ii)
       end do
      end do
      do 300 l=1,4
      do 300 i=1,(jcap+1)*(jcap+2)*nsig
        if(bhalf(i,1,l) .lt. 0.)then
        print *,' warning '
        print *,i,l,bhalf(i,1,l)
        end if
        bhalf(i,1,l)=a(l)*bhalf(i,1,l)
300   continue
      do i=1,(jcap+1)*(jcap+2)
        if(bhalfp(i) .lt. 0.)then
        print *,' warning surface'
        print *,i,bhalfp(i)
        end if
        bhalfp(i)=a(3)*bhalfp(i)
      end do
      do 400 k=1,nsig
        bhalf(1,k,1)=0.
        bhalf(1,k,2)=0.
400   continue
      do 500 i=1,nsig*4*(jcap+1)*(jcap+2)
        bhalf(i,1,1)=sqrt(bhalf(i,1,1))
500   continue
      bhalfp=sqrt(bhalfp)
      close(ineofs)
c-------------------
      agvz=0.
      wgvz=0.
      bvz=0.
      do j=1,nsig
       do k=1,nsig
        if(j.le.nmdszh)
     *    wgvz(1:jcap,j)=wgvz(1:jcap,j)
     *      +w(k)*gammap(1:jcap)*vz(k,j)
        do i=1,nsig
         if(k.le.nmdszh)
     *     agvz(1:jcap,j,k)=agvz(1:jcap,j,k)
     *        +aibw(j,i)*gamma(1:jcap,j)*vz(i,k)
        end do
        if(k.le.nmdszh)
     *    bvz(1:jcap,j,k)=beta(1:jcap,j)*vz(j,k)
       end do
      end do
      return
      end
