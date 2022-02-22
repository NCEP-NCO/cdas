      subroutine pcgsoi(inprep,ineofs,xhat,xhatp,pwcon,jsat,msat,
     *   niter,miter,jiter,jcap,igen,nsig,nlath,
     *   nlon,del2,wgts,pln,qln,rln,trigs,ifax,in,isatv,
     *  ntdata,nwdata,npdata,nqdata,npwdat,
     *  ntrecs,nwrecs,nprecs,nqrecs,npwrecs,
     *  iscra,nblk,on85dt,ioanl,inext,inges,rlats,as,rt,ru,rv,rpw,rq,rp,
     *  nsigstat,nmdszh,jcapstat,ampdivt,dampdivt,idivt,
     *  nsigdivt,jcapdivt,a3,sigl,sigi,dstlast,dstb,iscra3,rrm0,
     *   mlad,ml2lm,factslm,factvlm,
     *   lmad,lm2ml,factsml,factvml,
     *   rus,rvs,rts,rvorts,rplons,rplats,
     *   qfile,uvfile,tfile,sfile,pwfile,psfile,nsprof)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    pcgsoi      solve spectral oi equation
c   prgmmr: parrish          org: w/nmc22    date: 91-04-02
c
c abstract: solve spectral oi equation. at end of iteration, convert
c   to analysis units
c
c program history log:
c   91-04-02  parrish, d., derber,j.
c
c   input argument list:
c	  inprep   - unit containing prepbufr 
c     ineofs   - unit containing forecast error covariances
c     pwcon    - constants for precip. water calc.
c     jsat     - unit containing sat error covariance matrices
c     msat     - number of satellite profiles
c     niter    - max number of iterations for conjugate gradient.
c     miter    - max number of outer iterations
c     jiter    - current outer iteration
c     jcap     - triangular truncation
c     igen     - generating process identifier
c     nsig     - number of sigma levels
c     nlath    - number of gaussian lats in one hemisphere
c     nlon     - number of longitudes
c     ap,bp,aqr,bqr,gr - recursion constants for spherical harmonics
c     del2     - n*(n+1)/(a**2)
c     wgts     - gaussian integration weights
c     slat,clat - sin and cos of gaussian latitudes
c     trigs,ifax - used by fft
c     pe0,qe0,ro0 - starting functions for spherical harmonic recursions
c     isatv    - array of flags determining the use of sat err. cov.
c     ntdata   - number of conventional temperaturs
c     nwdata   - number of conventional winds
c     npdata   - number of surface pres. data
c     nqdata   - number of spec. hum. data
c     npwdat   - number of prec. water data
c     ntrecs   - number of records of conventional temperaturs
c     nwrecs   - number of records of conventional winds
c     nprecs   - number of records of surface pres. data
c     nqrecs   - number of records of spec. hum. data
c     npwrecs  - number of records of prec. water data
c     iscra    - unit containing conventional data infor.
c     nblk     - blocking factor for unit iscra
c     on85dt   - o.n. 85 date time group
c     ioanl    - output unit number
c     inges    - 6 hr forecast unit number
c     rlats    - grid lat locations
c     as       - background error coefficient
c     rt,ru,rv,rpw,rq,rp - scratch grid arrays
c     nsigstat - number of sigma levels in statistics array
c     nmdszh   - number of vertical modes used in balance equation
c     jcapstat - spectral truncation of statistics
c     a3       - hydrostatic matrix
c     sigl,sigi - sigma structure
c     dstlast,dstb - divtend coefs for latest state and for background
c     iscra3   - unit containing grid stuff needed for tan lin divt
c     rrm0     - starting gradient of penalty for 1st outer iteration.
c
c   output argument list:      
c      none
c
c
c remarks:
c
c attributes:
c   machine:  cray
c
c$$$
      dimension pwcon(nsig)
      real rlats(2*nlath)
      real del2((jcap+1)*(jcap+2)),wgts(nlath*2)
      dimension trigs(nlon*2),ifax(10)
      dimension isatv(nsig),as(4)
      real rp((2*nlath+1)*(nlon+2))
      real rplon((2*nlath+1)*(nlon+2))
      real rplat((2*nlath+1)*(nlon+2))
      real rpw((2*nlath+1)*(nlon+2))
      real rq((2*nlath+1)*(nlon+2)*nsig)
      real rt((2*nlath+1)*(nlon+2)*nsig)
      real st((2*nlath+1)*(nlon+2)*nsig)
      real ru((2*nlath+1)*(nlon+2)*nsig)
      real rv((2*nlath+1)*(nlon+2)*nsig)
      real rvort((2*nlath+1)*(nlon+2)*nsig)
      dimension rus(2*nlath+1,nlon+2,nsig)
      dimension rvs(2*nlath+1,nlon+2,nsig)
      dimension rts(2*nlath+1,nlon+2,nsig)
      dimension rvorts(2*nlath+1,nlon+2,nsig)
      dimension rplons(2*nlath+1,nlon+2)
      dimension rplats(2*nlath+1,nlon+2)
      character*4 on85dt(8)
      real pln((jcap+1)*(jcap+2),nlath)
      real qln((jcap+1)*(jcap+2),nlath)
      real rln((jcap+1)*(jcap+2),nlath)
      dimension mlad(0:jcap,0:jcap)
      dimension ml2lm((jcap+1)*(jcap+2))
      dimension factslm((jcap+1)*(jcap+2))
      dimension factvlm((jcap+1)*(jcap+2))
      dimension lmad(0:jcap,0:jcap)
      dimension lm2ml((jcap+1)*(jcap+2))
      dimension factsml((jcap+1)*(jcap+2))
      dimension factvml((jcap+1)*(jcap+2))
      dimension qfile(17*nqdata),uvfile(18*nwdata),tfile(17*ntdata),
     *     sfile((4+(28+2)*30)*nsprof),pwfile(12*npwdat),
     *     psfile(11*npdata)
c--------
c-------- local space
c--------
      dimension sigl(nsig),sigi(nsig+1)
      real cshat((jcap+1)*(jcap+2))
      dimension vz(nsig,nsig),vd(nsig,nsig)
      dimension vq(nsig,nsig),vh(nsig,nsig)
      dimension agvz(0:jcap,nsig,nmdszh),wgvz(0:jcap,nmdszh)
      dimension bvz(0:jcap,nsig,nmdszh)
      real bhalf((jcap+1)*(jcap+2),nsig,4)
      real bhalfp((jcap+1)*(jcap+2))
      real bdivt(0:jcap,nsig)
      real xhat((jcap+1)*(jcap+2),nsig,4)
      real xhatp((jcap+1)*(jcap+2))
      real phat((jcap+1)*(jcap+2),nsig,4)
      real phatp((jcap+1)*(jcap+2))
      real fhat((jcap+1)*(jcap+2),nsig,4)
      real fhatp((jcap+1)*(jcap+2))
      real ghat((jcap+1)*(jcap+2),nsig,4)
      real ghatp((jcap+1)*(jcap+2))
      real denom(2)
      dimension in((jcap+1)*(jcap+2))
      dimension c1(2),b1(2)
      real ps((jcap+1)*(jcap+2))
      character*4 on85(8)
      integer idateg(4)
      real dstlast((jcap+1)*(jcap+2),nsig)
      real dstb((jcap+1)*(jcap+2),nsig)
      dimension a3(nsig,nsig)
      dimension dsms(nsig),dbms(nsig),difms(nsig)
      real factor((jcap+1)*(jcap+2))
      real factori((jcap+1)*(jcap+2))
      real baln((jcap+1)*(jcap+2))
c-------------
c	write(*,*) '>>pcgsoi5 jcap=',jcap
      ncef=(jcap+1)*(jcap+2)
      ncefs=ncef*nsig
      ncef4s=4*ncefs
      ngrp=(2*nlath+1)*(nlon+2)
      ngrps=(2*nlath+1)*(nlon+2)*nsig
c     imax=222201
c--------
c--------
c-------- obtain initial r,p,x
c--------
c--------
c-------- 7.  obtain bhalf (background error covar)
c--------
         ii=-1
         do m=0,jcap
          ii=ii+2
          factor(ii)=1.
          factori(ii)=2.
          factor(ii+1)=0.
          factori(ii+1)=0.
          if(m.lt.jcap) then
           do l=1,jcap-m
            ii=ii+2
            factor(ii)=2.
            factori(ii)=1.
            factor(ii+1)=2.
            factori(ii+1)=1.
           end do
          end if
         end do
      factor(1)=0.
      factori(1)=0.
      factor=factor/(16.*atan(1.))
      call getbaln(baln,jcap)
      rp=0.
      call initps(rp,nlath,nlon,nprecs,psfile)
      ru=0.
      rv=0.
      call initw(ru,rv,nlath,nlon,nsig,nwrecs,uvfile)
cwrite(*,*) '|ru|1: ', ddot((2*nlath+1)*(nlon+2),
c    1		ru(1),1,ru(1),1)
cwrite(*,*) '|rv|1: ', ddot((2*nlath+1)*(nlon+2),
c    1		rv(1),1,rv(1),1)
      rlkm=400.
      call satcov(jcap,nlath,nlon,cshat,rlats,
     *           pln,wgts,trigs,ifax,rlkm,lmad)
      rt=0.
      st=0.
      call initt(st,ntrecs,nlath,nlon,nsig,tfile)
      rq=0.
      call initqpw(rq,nlath,nlon,nsig,nqrecs,npwrecs,
     *   pwcon,qfile,pwfile)
c	write(*,*) '|cshat|: ', ddot((jcap+1)*(jcap+2),
c     1		cshat(1),1,cshat(1),1)
c	write(*,*) 'msat, nlath,nlon,nsig',msat,nlath,nlon,nsig
c	write(*,*) '|rt|5: ', ddot((2*nlath+1)*(nlon+2)*nsig,
c     1		rt(1),1,rt(1),1)
c	write(*,*) '|pln|: ', ddot((jcap+1)*(jcap+2)*nlath,
c     1		pln(1,1),1,pln(1,1),1)
c	write(*,*) 'isatv',isatv

      call initsat(msat,nlath,nlon,nsig,rt,cshat,pln,
     *  trigs,ifax,jcap,isatv,sfile)
c	write(*,*) 'sfile:1: ',(sfile(i9),i9=1,50)
c
c	write(*,*) '|rt|0: ', ddot((2*nlath+1)*(nlon+2)*nsig,
c     1		rt(1),1,rt(1),1)
c	write(*,*) '|st|0: ', ddot((2*nlath+1)*(nlon+2)*nsig,
c     1		st(1),1,st(1),1)
      rt=rt+st
c--------
c--------  first obtain vertical resolution of input stats
c--------
      call gtbhalf(ineofs,bhalf,bhalfp,jcap,nsig,nlath,as,jcapstat,
     *     nsigstat,agvz,wgvz,bvz,nmdszh,vz,vd,vh,vq,sigl)
      call gtbdivt(idivt,bdivt,nsig,jcap,nsigdivt,jcapdivt,sigl)
      print *,' in pcgsoi, ampdivt=',ampdivt
      bdivt=ampdivt*bdivt
      if(jiter.gt.1) then
       dbms=0.
       dsms=0.
       difms=0.
       dbtot=0.
       dstot=0.
       diftot=0.
       do k=1,nsig
        do i=1,ncef
         dsms(k)=dsms(k)+factor(i)*dstlast(i,k)**2
         dbms(k)=dbms(k)+factor(i)*dstb(i,k)**2
         difms(k)=difms(k)+factor(i)*(dstlast(i,k)-dstb(i,k))**2
         dstlast(i,k)=dampdivt*dstb(i,k)-dstlast(i,k)
c-------------------derber always changes the stupid sign
         dstlast(i,k)=-dstlast(i,k)
        end do
        dbtot=dbtot+dbms(k)/nsig
        dstot=dstot+dsms(k)/nsig
        diftot=diftot+difms(k)/nsig
       end do
       do k=1,nsig
        do i=1,ncef
         dstlast(i,k)=bdivt(in(i),k)*dstlast(i,k)
        end do
c-------------------multiply zonal terms by extra factor of 2
c------------- for homogeneous, isotropic covariance in grid space.
        dstlast(1:ncef,k)=factori(1:ncef)*dstlast(1:ncef,k)
       end do
       write(6,*)' stats for div tend follow for outer iteration =',
     *              jiter
       write(6,*)' current analysis divtend   background divtend  diff'
       do k=1,nsig
        write(6,94512)k,dsms(k),dbms(k),difms(k)
94512   format(' k=',i3,3e19.5)
       end do
       k=nsig+1
       write(6,94512)k,dstot,dbtot,diftot

c	write(*,*) '|rt|1: ', ddot((2*nlath+1)*(nlon+2)*nsig,
c     1		rt(1),1,rt(1),1)
c	write(*,*) '|r2|1: ', ddot((2*nlath+1)*(nlon+2)*nsig,
c     1		rts(1,1,1),1,rts(1,1,1),1)

       if(ampdivt.gt.0.)
     *  call qtoper(ru,rv,rvort,rt,rplon,rplat,
     *    nsig,jcap,nlon,nlath,pln,qln,rln,trigs,ifax,
     *    del2,wgts,a3,sigl,sigi,dstlast,iscra3,rlats,
     *    rus,rvs,rts,rvorts,rplons,rplats)

c       write(*,*) '|ru|2: ', ddot((2*nlath+1)*(nlon+2),
c    1          ru(1),1,ru(1),1)              
c       write(*,*) '|rv|2: ', ddot((2*nlath+1)*(nlon+2),
c    1          rv(1),1,rv(1),1)

      else
       rvort=0.
       rplat=0.
       rplon=0.
      end if

c        write(*,*) '|rt|2: ', ddot((2*nlath+1)*(nlon+2)*nsig,
c     1          rt(1),1,rt(1),1)
c        write(*,*) '|r2|2: ', ddot((2*nlath+1)*(nlon+2)*nsig,
c     1          rts(1,1,1),1,rts(1,1,1),1)


c     write(*,*) ' before new htoper, ',ru(1),rv(1),rvort(1),
c    *   rt(1),rp(1),rplon(1),rplat(1),rq(1)
c        write(*,*)  't1.1:3 ', ddot(ncefs,ghat(1,1,1),1,ghat(1,1,1),1)
c        write(*,*)  't1.2:3 ', ddot(ncefs,ghat(1,1,2),1,ghat(1,1,2),1)
c        write(*,*)  't1.3:3 ', ddot(ncefs,ghat(1,1,3),1,ghat(1,1,3),1)
c        write(*,*)  't1.4:3 ', ddot(ncefs,ghat(1,1,4),1,ghat(1,1,4),1)
c        write(*,*)  'ghatp1:',ddot(ncefs,ghatp(1),1,ghatp(1),1)

      call htoper(ghat(1,1,1),ghat(1,1,2),ghat(1,1,3),
     *    ghat(1,1,4),ghatp,ru,rv,rvort,rt,rp,rplon,rplat,rq,
     *    bhalf,bhalfp,nsig,jcap,nlon,nlath,del2,
     *    pln,qln,rln,trigs,ifax,
     *    agvz,wgvz,bvz,nmdszh,vz,vd,vh,vq,in,baln)

c        write(*,*)  't1.1:4 ', ddot(ncefs,ghat(1,1,1),1,ghat(1,1,1),1)
c        write(*,*)  't1.2:4 ', ddot(ncefs,ghat(1,1,2),1,ghat(1,1,2),1)
c        write(*,*)  't1.3:4 ', ddot(ncefs,ghat(1,1,3),1,ghat(1,1,3),1)
c        write(*,*)  't1.4:4 ', ddot(ncefs,ghat(1,1,4),1,ghat(1,1,4),1)
c        write(*,*)  'ghatp1:',ddot(ncefs,ghatp(1),1,ghatp(1),1)

c        write(*,*) '|rt|3: ', ddot((2*nlath+1)*(nlon+2)*nsig,
c     1          rt(1),1,rt(1),1)
c        write(*,*) '|r2|3: ', ddot((2*nlath+1)*(nlon+2)*nsig,
c     1          rts(1,1,1),1,rts(1,1,1),1)

c      print *,' after new htoper, ',ghat(lmad(1,1),1,1),
c    *   ghat(lmad(1,1),1,2),ghat(lmad(1,1),1,3),ghat(lmad(1,1),1,4),
c    *        ghatp(lmad(1,1))
c      write(6,*)'at 11',ghat(imax,1,1)
c	write(*,*) 'jiter=',jiter
      if(jiter.gt.1)then
ccdir$ ivdep
       do i=1,ncef4s
        ghat(i,1,1)=ghat(i,1,1)+xhat(i,1,1)
       enddo
       ghatp=ghatp+xhatp
c        write(*,*)  't1.1:0 ', ddot(ncefs,ghat(1,1,1),1,ghat(1,1,1),1)
c        write(*,*)  't1.2:0 ', ddot(ncefs,ghat(1,1,2),1,ghat(1,1,2),1)
c        write(*,*)  't1.3:0 ', ddot(ncefs,ghat(1,1,3),1,ghat(1,1,3),1)
c        write(*,*)  't1.4:0 ', ddot(ncefs,ghat(1,1,4),1,ghat(1,1,4),1)
      endif
      xhat=0.
      xhatp=0.
ccdir$ ivdep
      do 956 i=1,ncef4s
       phat(i,1,1)=-ghat(i,1,1)
 956  continue
      phatp=-ghatp
c        fhat used now as work array
c----------
c--------
c-------- beginning of iteration
c--------
      rrmold=ddot(4*ncefs,ghat(1,1,1),1,ghat(1,1,1),1)
     *         +ddot(ncef,ghatp,1,ghatp,1)
c	write(*,*) 'rrmold term1=', ddot(4*ncefs,ghat(1,1,1),1,
c     1		ghat(1,1,1),1)
c	write(*,*)  't1.1:', ddot(ncefs,ghat(1,1,1),1,ghat(1,1,1),1)
c	write(*,*)  't1.2:', ddot(ncefs,ghat(1,1,2),1,ghat(1,1,2),1)
c	write(*,*)  't1.3:', ddot(ncefs,ghat(1,1,3),1,ghat(1,1,3),1)
c	write(*,*)  't1.4:', ddot(ncefs,ghat(1,1,4),1,ghat(1,1,4),1)
c	write(*,*) 'rrmold term2=', ddot(ncef,ghatp,1,ghatp,1)

      if(jiter.eq.1) then
       rrm0=rrmold
      end if
      write(6,2677)rrmold
2677  format(' dynamics+moisture roriginal=',e12.5)

      a=.5
c---------------test for no data
      if(rrmold.lt.1.e-10) go to 3070
      do 3000 iter=1,niter
c----------                         1/2 t -1   1/2
c---------- apply operator a = i + b   h o  h b
c----------
ccdir$ ivdep
        do i=1,ncef4s
         fhat(i,1,1)=phat(i,1,1)
        end do
        fhatp=phatp
c      print *,' before new hoper, ',fhat(lmad(1,1),1,1),
c    *   fhat(lmad(1,1),1,2),fhat(lmad(1,1),1,3),fhat(lmad(1,1),1,4),
c    *        fhatp(lmad(1,1))
        call hoper(fhat(1,1,1),fhat(1,1,2),fhat(1,1,3),
     *    fhat(1,1,4),fhatp,ru,rv,rvort,rt,rp,rplon,rplat,rq,
     *    bhalf,bhalfp,nsig,jcap,nlon,nlath,del2,
     *    pln,qln,rln,trigs,ifax,
     *    agvz,wgvz,bvz,nmdszh,vz,vd,vh,vq,in,baln)
c     print *,' after new hoper, ',ru(1),rv(1),rvort(1),
c    *   rt(1),rp(1),rplon(1),rplat(1),rq(1)
c-------------------------------------add div-tend penalty here
       if(ampdivt.gt.0.) then
c     print *,' before new qoper, ',ru(1),rv(1),rvort(1),
c    *   rt(1),rplon(1),rplat(1)
        call qoper(ru,rv,rvort,rt,rplon,rplat,
     *    nsig,jcap,nlon,nlath,pln,qln,rln,trigs,ifax,
     *    del2,wgts,a3,sigl,sigi,fhat,iscra3,rlats,
     *    rus,rvs,rts,rvorts,rplons,rplats)
c      print *,' after new qoper, ',fhat(lmad(1,1),1,1)
        do k=1,nsig
         do i=1,ncef
          fhat(i,k,1)=bdivt(in(i),k)*fhat(i,k,1)
         end do
c-------------------multiply zonal terms by extra factor of 2
c------------- for homogeneous, isotropic covariance in grid space.
         fhat(1:ncef,k,1)=factori(1:ncef)*fhat(1:ncef,k,1)
        end do
       end if
c       pend=ddot(3*ncefs,xhat(1,1,1),1,xhat(1,1,1),1)
c       penq=ddot(ncefs,xhat(1,1,4),1,xhat(1,1,4),1)
c       write(72,*)pend,penq
c       print *,' initial error penalties ',pend,penq
        st=rt
        call intps(rp,nlath,nlon,nprecs,psfile)
        call intw(ru,rv,nlath,nlon,nsig,nwrecs,uvfile)
        call intt(st,ntrecs,nlath,nlon,nsig,tfile)
        call intqpw(rq,nlath,nlon,nsig,nqrecs,npwrecs,
     *     pwcon,qfile,pwfile)

c	write(*,*) '|rt|6: ',ddot((2*nlath+1)*(nlon+2)*nsig,
c     1		rt(1),1,rt(1),1)
        call satop4(msat,nlath,nlon,nsig,rt,cshat,
     *    pln,trigs,ifax,jcap,isatv,sfile)
c	write(*,*) 'sfile:2: ',(sfile(i9),i9=1,50)
c	write(*,*) '|rt|7: ', ddot((2*nlath+1)*(nlon+2)*nsig,
c     1		rt(1),1,rt(1),1)
        rt=rt+st
        if(ampdivt.gt.0.) then
c      print *,' before new qtoper, ',fhat(lmad(1,1),1,1)
         call qtoper(ru,rv,rvort,rt,rplon,rplat,
     *     nsig,jcap,nlon,nlath,pln,qln,rln,trigs,ifax,
     *     del2,wgts,a3,sigl,sigi,fhat,iscra3,rlats,
     *     rus,rvs,rts,rvorts,rplons,rplats)
c     print *,' after new qtoper, ',ru(1),rv(1),rvort(1),
c    *   rt(1),rplon(1),rplat(1)
        else
         rvort=0.
         rplon=0.
         rplat=0.
        end if
c     print *,' before new htoper, ',ru(1),rv(1),rvort(1),
c    *   rt(1),rp(1),rplon(1),rplat(1),rq(1)
        call htoper(fhat(1,1,1),fhat(1,1,2),fhat(1,1,3),
     *    fhat(1,1,4),fhatp,ru,rv,rvort,rt,rp,rplon,rplat,rq,
     *    bhalf,bhalfp,nsig,jcap,nlon,nlath,del2,
     *    pln,qln,rln,trigs,ifax,
     *    agvz,wgvz,bvz,nmdszh,vz,vd,vh,vq,in,baln)
c      print *,' after new htoper, ',fhat(lmad(1,1),1,1),
c    *   fhat(lmad(1,1),1,2),fhat(lmad(1,1),1,3),fhat(lmad(1,1),1,4),
c    *        fhatp(lmad(1,1))
c      write(6,*)'at 21',fhat(imax,1,1)
ccdir$ ivdep
        do 958 i=1,ncef4s
 958    fhat(i,1,1)=phat(i,1,1)+fhat(i,1,1)
        fhatp=phatp+fhatp
c      write(6,*)'at 22',fhat(imax,1,1)
        a=0.
        ptgp=ddot(4*ncefs,fhat(1,1,1),1,phat(1,1,1),1)
     *      +ddot(ncef,fhatp,1,phatp,1)
        if(ptgp .gt. 1.e-16)a=rrmold/ptgp
        do 858 i=1,4*ncefs
        xhat(i,1,1)=xhat(i,1,1)+a*phat(i,1,1)
 858    ghat(i,1,1)=ghat(i,1,1)+a*fhat(i,1,1)
        xhatp=xhatp+a*phatp
        ghatp=ghatp+a*fhatp
        rrmnew=ddot(4*ncefs,ghat(1,1,1),1,ghat(1,1,1),1)
     *           +ddot(ncef,ghatp,1,ghatp,1)
        b=0.
        if(abs(rrmold).gt.1.e-16) b=rrmnew/rrmold
        write(6,2678)iter,rrmnew,a,b
2678    format(' dynamics iter,rnew,a,b=',i3,3e12.5)
        if(iter .eq. niter)go to 3100
        if(rrmnew.lt.1.e-7*rrm0)go to 3050
ccdir$ ivdep
        do 608 i=1,ncefs
          phat(i,1,1)=-ghat(i,1,1)+b*phat(i,1,1)
          phat(i,1,2)=-ghat(i,1,2)+b*phat(i,1,2)
          phat(i,1,3)=-ghat(i,1,3)+b*phat(i,1,3)
          phat(i,1,4)=-ghat(i,1,4)+b*phat(i,1,4)
608     continue
        phatp=-ghatp+b*phatp
        rrmold=rrmnew
3000  continue
      go to 3100
3050  continue
      write(6,3060)
3060  format(' iteration stopped because residual reduced by ',
     *    'more than 7 orders of magnitude.')
      go to 3100
3070  continue
       write(6,3075)
3075   format('   apparently no data')
3100  continue
c--------
c-------- convert to model variables
c--------
        ghat=xhat
        ghatp=xhatp
        call hopers(ghat(1,1,1),ghat(1,1,2),ghat(1,1,3),
     *    ghat(1,1,4),ghatp,bhalf,bhalfp,nsig,jcap,
     *     agvz,wgvz,bvz,nmdszh,vz,vd,vh,vq,in,baln)
        fhat=ghat
        ps=ghatp
c--------
c-------- 14. add increment to guess and write out
c--------
c--------
c-------- read in guess, putting into internal format.
c-------- scra and aqr used as scratch
c--------
      call rdgesc(phat(1,1,1),phat(1,1,2),phat(1,1,3),
     *  phat(1,1,4),factor,factori,hourg,
     *  idateg,sigi,sigl,inext,jcap,nsig,on85,
     *  ml2lm,factslm,factvlm)
c--------
c-------- add increments
c--------
      do l=1,nsig*(jcap+1)*(jcap+2)
       fhat(l,1,1)=fhat(l,1,1)+phat(l,1,1)
       fhat(l,1,2)=fhat(l,1,2)+phat(l,1,2)
       fhat(l,1,3)=fhat(l,1,3)+phat(l,1,3)
       fhat(l,1,4)=fhat(l,1,4)+phat(l,1,4)
      end do
      ps=factor+ps
c--------
c-------- get guess q to use as reference when limiting analysis q
c--------   (only after last outer iteration)
c-------
      if (jiter.eq.miter) then
       call rdgesc(phat(1,1,1),phat(1,1,2),phat(1,1,3),
     *   phat(1,1,4),factor,factori,hourg,
     *   idateg,sigi,sigl,inges,jcap,nsig,on85,
     *   ml2lm,factslm,factvlm)
       call s2g0(ps,rv,jcap,nlon,nlath,pln,trigs,ifax)
       do k=1,nsig
        call s2g0(phat(1,k,4),ru((k-1)*ngrp+1),jcap,nlon,
     *     nlath,pln,trigs,ifax)
        call s2g0(fhat(1,k,3),rt((k-1)*ngrp+1),jcap,nlon,
     *     nlath,pln,trigs,ifax)
       end do
       rq=ru
       call genqsat(rt,rq,nlath,nlon,nsig,
     *    rv,sigl)
       rv=0.
       do i=1,ngrps
        if(rq(i) .gt. 0. .and. ru(i) .gt. rq(i))
     *      rq(i)=ru(i)
       end do
       do i=1,ngrps                         
        if(ru(i) .lt. 0.  )rv(i)=ru(i)
       end do
       do k=1,nsig
        call s2g0(fhat(1,k,4),ru((k-1)*ngrp+1),jcap,nlon,
     *     nlath,pln,trigs,ifax)
       end do
       nsuper=0
       nnq=0
       do i=1,ngrps
         if(ru(i) .gt. 0. .and. ru(i) .gt. rq(i)) then
          ru(i)=rq(i)
          nsuper=nsuper+1
         end if
c       end if
       end do
       do i=1,ngrps
c       if(ru(i) .lt. 0. .and. ru(i) .lt. rq(i)) then
c        ru(i)=0.
        if(ru(i) .lt. 0. .and. ru(i) .lt. rv(i)) then
         ru(i)=min(0.,rv(i))
         nnq=nnq+1
        end if
       end do
       print *,' number of supersaturated points = ',nsuper
       print *,' number of negative q points = ',nnq
       do k=1,nsig
        call g2s0(fhat(1,k,4),ru((k-1)*ngrp+1),jcap,nlon,nlath,
     *           wgts,pln,trigs,ifax)
       end do
      end if
      hourg=0.
c--------
c-------- write out analysis
c--------
      call wranlc(inprep,fhat(1,1,1),fhat(1,1,2),fhat(1,1,3),
     *  fhat(1,1,4),
     *  ps,factori,hourg,idateg,sigi,sigl,ioanl,
     *   jcap,igen,nsig,on85,on85dt,lm2ml,factsml,factvml)
      return
      end
