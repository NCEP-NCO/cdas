      subroutine setuprhs(sigl,jiter,rpw,sigi,
     *  inges,iianl,jcap,nsig,nlath,nlon,pwcon,
     *  ntdata,nsdata,nwdata,npdata,nqdata,npwdat,nqtdata,
     *  ntrecs,nwrecs,nprecs,nqrecs,npwrecs,isat,nsigsat,jsat,msat,
     *  rlats,del2,pln,qln,rln,wgts,trigs,ifax,in,
     *  isfc,isatv,iscra,nblk,rt,ru,rv,rq,rp,
     *  a3,ampdivt,dampdivt,dstlast,dstb,iscra3,
     *   ermaxt,ermaxw,ermaxp,ermaxq,ermaxpw,
     *   ermint,erminw,erminp,erminq,erminpw,
     *   grosst,grossst,grossw,grossp,grossq,grosspw,
     *   mlad,ml2lm,factslm,factvlm,
     *   lmad,lm2ml,factsml,factvml,
     *   rus,rvs,rts,rvorts,rplons,rplats,
     *   qfile,uvfile,tfile,sfile,pwfile,psfile,nsprof)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    setuprhs   compute rhs of oi equation
c   prgmmr: parrish          org: w/nmc22    date: 90-10-06
c
c abstract: read in data, first guess, and obtain rhs of oi equation.
c
c program history log:
c   90-10-06  parrish
c
c   input argument list:
c     jiter    - outer iteration counter
c     inges    - unit number for guess spectral coefficients.
c     iianl    - unit number for previous analysis.
c     jcap     - triangular truncation
c     nsig     - number of sigma levels
c     nlath    - number of gaussian lats in one hemisphere
c     nlon     - number of longitudes
c     pwcon    - constants used for integration of precip. water
c     ntdata,nsdata,nwdata,npdata,nqdata,npwdat - num t, satt, etc obs
c     isat     - unit number of input vert. sat. stats
c     nsigsat  - number of sigma levels for input vert sat. stats
c     jsat     - unit number of output sat. infor.
c     isfc     - unit number bges file
c     isatv    - vertical array to indicate horizontal error cov. used.
c     iscra    - unit number of output conventional observation infor.
c     nblk     - blocking factor for output files
c     rt,ru,rv,rq,rp,rpw - scratch grid arrays
c     a3       - hydrostatic matrix
c     ampdivt,dampdivt - parameters for div-tend penalty
c     ermaxt,ermaxw,ermaxp,ermaxq,ermaxpw -  parameters for 
c     ermint,erminw,erminp,erminq,erminpw -    gross error 
c     grosst,grossst,grossw,grossp,grossq,grosspw -  check of data
c
c   output argument list:
c     sigl,sigi - sigma layer midpoint and interface values
c     ntrecs,nwrecs,nprecs,nqrecs,npwrecs - num t, satt, etc records
c     msat     - number of satellite records
c     rlats    - grid latitudes (radians)
c     ap,bp,aqr,bqr,gr - recursion constants for spherical harmonics
c     del2     - n*(n+1)/(a**2)
c     slat,clat - sin and cos of gaussian latitudes
c     pe0,qe0,ro0 - starting functions for spherical harmonics
c     wgts     - gaussian integration weights
c     trigs,ifax - used by fft
c     lmix,lastmix,lpairs - used for multitasking
c     in       - 2-dim wave-number value of each coef
c     dstlast,dstb - coefs of last and guess div-tend
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      real del2((jcap+1)*(jcap+2))
      real pwcon(nsig)
      dimension trigs(nlon*2),ifax(10),rlats(nlath*2)
      real wgts(nlath*2),sigl(nsig)
      dimension isatv(nsig)
      dimension rt(2*nlath+1,nlon+2,nsig)
      dimension ru(2*nlath+1,nlon+2,nsig)
      dimension rv(2*nlath+1,nlon+2,nsig)
      dimension rq(2*nlath+1,nlon+2,nsig)
      dimension rp(2*nlath+1,nlon+2)
      dimension in((jcap+1)*(jcap+2))
      dimension dstlast((jcap+1)*(jcap+2),nsig)
      dimension dstb((jcap+1)*(jcap+2),nsig)
      dimension pln((jcap+1)*(jcap+2),nlath)
      dimension qln((jcap+1)*(jcap+2),nlath)
      dimension rln((jcap+1)*(jcap+2),nlath)
      dimension mlad(0:jcap,0:jcap)
      dimension ml2lm((jcap+1)*(jcap+2))
      dimension factslm((jcap+1)*(jcap+2))
      dimension factvlm((jcap+1)*(jcap+2))
      dimension lmad(0:jcap,0:jcap)
      dimension lm2ml((jcap+1)*(jcap+2))
      dimension factsml((jcap+1)*(jcap+2))
      dimension factvml((jcap+1)*(jcap+2))
      dimension rts(2*nlath+1,nlon+2,nsig)
      dimension rus(2*nlath+1,nlon+2,nsig)
      dimension rvs(2*nlath+1,nlon+2,nsig)
      dimension rvorts(2*nlath+1,nlon+2,nsig)
      dimension rplons(2*nlath+1,nlon+2)
      dimension rplats(2*nlath+1,nlon+2)
c--------
c-------- scratch space
c--------
      dimension rlons(nlon)
      dimension tdata(ntdata,8),wdata(nwdata,8)
      dimension sdata(nsdata,6)
      dimension psdata(npdata,8),qdata(nqdata,7)
      dimension pwdata(npwdat,6)
      dimension rpw(2*nlath+1,nlon+2)
      dimension sigi(nsig+1)
      dimension tempt(ntdata),temps(nsdata),tempq(nqdata)
      dimension tempp(npdata),temppw(npwdat),tempw(nwdata)
      dimension tpres(ntdata),qpres(nqdata),wpres(nwdata)
      dimension spres(nsdata)
      dimension tges(ntdata),qges(nqdata),vges(nwdata),sges(nsdata)
      dimension pges(npdata),uges(nwdata),pwges(npwdat)
      dimension rqtype(nqdata),rwtype(nwdata),qtges(ntdata)
      dimension iqtflg(ntdata)
      dimension ptype(npdata),pwtype(npwdat)
      dimension qmaxerr(nqdata),pwmerr(npwdat)
      dimension fact(nwdata)
      dimension rbqs(nqdata)
      dimension factor(2*nlath+1,nlon+2)
      INTEGER IDATEG8(8),IDATEG28(8)
	  REAL RINC(5)
      integer idateg(4)
      integer idateg2(4),idate5(5)
      integer*4 idate4(4)

      dimension qfile(17*nqdata),uvfile(18*nwdata),tfile(17*ntdata),
     *     sfile((4+(28+2)*30)*nsprof),pwfile(12*npwdat),
     *     psfile(11*npdata)
c---------------
      isatv=1
c--------
c-------- 2.  check for consistency of times for previous analysis
c--------     and 6 hr forecast, bring in 6 hr forecast on grid.
c--------
      deltat=-9999
      rewind inges
      read(inges)
cwne  read(inges)hourg,idateg
      read(inges)hourg,idate4
      idateg = idate4

	  IDATEG8=0
	  IDATEG8(1)=IDATEG(4)
	  IDATEG8(2)=IDATEG(2)
	  IDATEG8(3)=IDATEG(3)
	  IDATEG8(5)=IDATEG(1)
      print *, ' guess  file ',hourg,idateg
      close(inges)
      rewind iianl
      read(iianl,end=1112)
cwne  read(iianl,end=1112)hourg2,idateg2
      read(iianl,end=1112)hourg2,idate4
      idateg2(1) = idate4(1)
      idateg2(2) = idate4(2)
      idateg2(3) = idate4(3)
      idateg2(4) = idate4(4)
	  IDATEG28=0
	  IDATEG28(1)=IDATEG2(4)
	  IDATEG28(2)=IDATEG2(2)
	  IDATEG28(3)=IDATEG2(3)
	  IDATEG28(5)=IDATEG2(1)
      print *, ' guess2 file ',hourg2,idateg2
      close(iianl)

	  print*,'IDATEG8',IDATEG8
	  PRINT*,'IDATEG28',IDATEG28
      CALL W3DIFDAT(IDATEG8,IDATEG28,3,RINC)
	  print*,'rinc',rinc
      deltatt=(NINT(RINC(3))+60*(HOURG-HOURG2)) /60.

      deltat=deltatt
      print *,' deltat = ',deltat
c--------
1112  continue
c----------------preset number of data records to zero
        ntrecs=0
        nwrecs=0
        nprecs=0
        nqrecs=0
        npwrecs=0
c--------
c-------- initialize various transform constants
c--------
      call getlalo(rlats,rlons,wgts,jcap,nlon,nlath,
     *  del2,trigs,ifax,pln,qln,rln)
c--------
c-------- 1. read data                                           
c--------
      call rdprep(
     *   tdata,sdata,wdata,psdata,qdata,pwdata,
     *   tdata(1,8),sdata(1,6),rwtype,ptype,rqtype,pwtype,
     *   qmaxerr,pwmerr,iqtflg,
     *   ntdata,nsdata,nwdata,npdata,nqdata,npwdat)

      call inguessv(ru,rv,rt,rp,rq,rpw,sigi,sigl,
     *   inges,jcap,nsig,nlath,nlon,hourg,idateg,
     *   rvorts,rus,rplons,rplats,del2,pln,qln,rln,trigs,ifax,
     *     ml2lm,factslm,factvlm)
c
c--------
c--------  preprocess data, i.e, convert to grid locations, modify error
c--------  etc.
c--------
      ansig=float(nsig)
c
c   read surface file to get 10m wind factors

       rewind(isfc)
       call rdfact(factor,idateg,hourg,nlath,nlon,isfc)
c
c     set beginning and ending lats for increasing weight in S.H.
c
      blat=10.*0.017453292
      elat=20.*0.017453292
      call gdcrdp(blat,1,rlats,2*nlath)
      call gdcrdp(elat,1,rlats,2*nlath)
c-----------------rus  has latest div 
c     print *,' before new fulldivt, ',ru(1,1,1),rv(1,1,1),rt(1,1,1),
c    *    rp(1,1),rq(1,1,1),rpw(1,1),rvorts(1,1,1),rus(1,1,1),
c    *   rplons(1,1),rplats(1,1)
      call fulldivt(ru,rv,rt,rvorts,rus,rplons,rplats,rpw,
     *       nsig,jcap,nlon,nlath,pln,qln,rln,trigs,ifax,del2,wgts,a3,
     *       sigl,sigi,jiter,dstlast,rlats)
c     print *,' after new fulldivt, ',dstlast(lmad(1,1),1)
c-----------------now rus has latest vert velocity
      rus=ru
      rvs=rv
      rts=rt
c     rewind iscra3
c     write(iscra3)ru,rplon
c     write(iscra3)rv,rplat
c     write(iscra3)rt
c     write(iscra3)rvort
c     write(iscra3)rdiv
c     close(iscra3)
      if(npdata.gt.0) then
        call prepp(psdata(1,1),psdata(1,2),psdata(1,3),psdata(1,4),
     *    psdata(1,5),psdata(1,6),ptype,
     *    npdata,rt,rp,rpw,
     *    nlath*2,nlon,nsig,rlats,rlons,sigl)
       if(deltat .le. 0.) then
        do l=1,npdata
         psdata(l,8)=0.
        end do
       else
        do l=1,npdata
         psdata(l,8)=min(max(-1.,psdata(l,8)/deltat),0.)
        end do
       end if
       call intrp2(rp,tempp,psdata(1,3),psdata(1,2),
     *     nlath*2,nlon,npdata)
       do l=1,npdata
        pges(l)=(1.+psdata(l,8))*tempp(l)
       end do
      end if
      if(ntdata.gt.0) then
       call prept(tdata(1,1),tdata(1,2),tdata(1,3),tdata(1,4),
     *   tdata(1,8),tpres,ntdata,
     *   rp,nlath*2,nlon,nsig,rlats,rlons,sigl)
       do i=1,ntdata
        wgt1=.5
        wgt2=.5
        if(i .ne. 1) then
         if(tdata(i,6) .eq. tdata(i-1,6)) then
          if(tdata(i,2) .eq. tdata(i-1,2) .and. tdata(i,3) .eq.
     *       tdata(i-1,3) .and. tdata(i,8).eq. tdata(i-1,8)) then
           aldiff=tdata(i,4)-tdata(i-1,4)
           if(aldiff .lt. 1. .and. aldiff .ge. 0.) wgt2=.5*aldiff
           if(tdata(i-1,4) .ge. ansig) then
            wgt2=0.
            wgt1=0.
           end if
          end if
         end if
        end if
        if(i .ne. ntdata) then
         if(tdata(i,6) .eq. tdata(i+1,6) .and. tdata(i,4) .lt.
     *        ansig) then
          if(tdata(i,2) .eq. tdata(i+1,2) .and. tdata(i,3) .eq.
     *       tdata(i+1,3) .and. tdata(i,8).eq. tdata(i+1,8)) then
           aldiff=tdata(i+1,4)-tdata(i,4)
           if(aldiff .lt. 1. .and. aldiff .ge. 0.) wgt1=.5*aldiff
          end if
         end if
        end if
        tdata(i,1)=tdata(i,1)*(wgt1+wgt2)
       end do
       if(deltat .le. 0.) then
        do l=1,ntdata
         tdata(l,7)=0.
        end do
       else
        do l=1,ntdata
         tdata(l,7)=min(max(-1.,tdata(l,7)/deltat),0.)
        end do
       end if
       call intrp3(rt,tempt,tdata(1,3),tdata(1,2),tdata(1,4),
     *     nlath*2,nlon,nsig,ntdata)
       do l=1,ntdata
        tges(l)=(1.+tdata(l,7))*tempt(l)
       end do
       qtges=0.
       if(nqtdata .gt. 0) then
        call intrp3(rq,tempt,tdata(1,3),tdata(1,2),tdata(1,4),
     *       nlath*2,nlon,nsig,ntdata)
        do l=1,ntdata
         if(iqtflg(l) .eq.1) qtges(l)=(1.+tdata(l,7))*tempt(l)
        end do    
       end if
      end if
      if(nsdata.gt.0) then
       call preps(sdata(1,1),sdata(1,2),sdata(1,3),
     *    spres,nsdata,
     *    rp,nlath*2,nlon,nsig,rlats,rlons,sigl)
       if(deltat .le. 0.) then
        do l=1,nsdata
         sdata(l,5)=0.
        end do
       else
        do l=1,nsdata
         sdata(l,5)=min(max(-1.,sdata(l,5)/deltat),0.)
        end do
       end if
       call intrp3(rt,temps,sdata(1,2),sdata(1,1),sdata(1,3),
     *     nlath*2,nlon,nsig,nsdata)
       do l=1,nsdata
        sges(l)=(1.+sdata(l,5))*temps(l)
       end do
      end if
      if(npwdat.gt.0) then
c
c         create constants for p.w. calculations     
c
      acon=100./9.8
      do k=1,nsig
        pwcon(k)=acon*(sigi(k)-sigi(k+1))
      end do
       call preppw(pwdata(1,1),pwdata(1,2),pwdata(1,3),pwdata(1,4),
     *    pwmerr,pwtype,
     *    npwdat,nsig,nlath*2,nlon,rlats,rlons)
       if(deltat .le. 0.) then
        do l=1,npwdat
         pwdata(l,6)=0.
        end do
       else
        do l=1,npwdat
         pwdata(l,6)=min(max(-1.,pwdata(l,6)/deltat),0.)
        end do
       end if
       call intrp2(rp,pwdata(1,5),pwdata(1,3),pwdata(1,2),
     *     2*nlath,nlon,npwdat)
       do l=1,npwdat
        pwdata(l,5)=10.*exp(pwdata(l,5))
        pwges(l)=0.
       end do
       do k=1,nsig
        call intrp2(rq(1,1,k),temppw,pwdata(1,3),pwdata(1,2),
     *     2*nlath,nlon,npwdat)
        do l=1,npwdat
         pwges(l)=pwges(l)+(1.+pwdata(l,6))*temppw(l)*pwdata(l,5)*
     *                                       pwcon(k)
        end do
       end do
      end if
      if(nwdata.gt.0) then
c
       call prepw(wdata(1,1),wdata(1,2),wdata(1,3),wdata(1,4),
     *    rwtype,wpres,nwdata,rp,fact,factor,
     *    nlath*2,nlon,nsig,rlats,rlons,sigl)
        
       do i=1,nwdata
        wgt1=.5
        wgt2=.5
        if(i .ne. 1) then
         if(wdata(i,7) .eq. wdata(i-1,7)) then
          if(wdata(i,2) .eq. wdata(i-1,2) .and. wdata(i,3) .eq.
     *       wdata(i-1,3) .and. rwtype(i) .eq. rwtype(i-1)) then
           aldiff=wdata(i,4)-wdata(i-1,4)
           if(aldiff .lt. 1. .and. aldiff .ge. 0.) wgt2=.5*aldiff
           if(wdata(i-1,4) .ge. ansig) then
            wgt2=0.
            wgt1=0.
           end if
          end if
         end if
        end if
        if(i .ne. nwdata) then
         if(wdata(i,7) .eq. wdata(i+1,7) .and. wdata(i,4) .lt.
     *          ansig) then
          if(wdata(i,2) .eq. wdata(i+1,2) .and. wdata(i,3) .eq.
     *       wdata(i+1,3) .and. rwtype(i) .eq. rwtype(i+1)) then
           aldiff=wdata(i+1,4)-wdata(i,4)
           if(aldiff .lt. 1. .and. aldiff .ge. 0.) wgt1=.5*aldiff
          end if
         end if
        end if
        wdata(i,1)=wdata(i,1)*(wgt1+wgt2)
       end do
       if(deltat .le. 0.) then
        do l=1,nwdata
         wdata(l,8)=0.
        end do
       else
        do l=1,nwdata
         wdata(l,8)=min(max(-1.,wdata(l,8)/deltat),0.)
        end do
       end if
       call intrp3(ru,tempw,wdata(1,3),wdata(1,2),wdata(1,4),
     *     2*nlath,nlon,nsig,nwdata)
       do l=1,nwdata
        uges(l)=(1.+wdata(l,8))*tempw(l)*fact(l)
       end do
       call intrp3(rv,tempw,wdata(1,3),wdata(1,2),wdata(1,4),
     *     2*nlath,nlon,nsig,nwdata)
       do l=1,nwdata
        vges(l)=(1.+wdata(l,8))*tempw(l)*fact(l)
       end do
      end if
      if(nqdata.gt.0) then
       call prepq(qdata(1,1),qdata(1,2),qdata(1,3),qdata(1,4),
     *   rqtype,
     *   qpres,nqdata,qmaxerr,rbqs,rt,
     *   rp,nlath*2,nlon,nsig,rlats,rlons,sigl)
       do i=1,nqdata
        wgt1=.5
        wgt2=.5
        if(i .ne. 1) then
         if(qdata(i,6) .eq. qdata(i-1,6)) then
          if(qdata(i,2) .eq. qdata(i-1,2) .and. qdata(i,3) .eq.
     *       qdata(i-1,3) .and. rqtype(i).eq. rqtype(i-1)) then
           aldiff=qdata(i,4)-qdata(i-1,4)
           if(aldiff .lt. 1. .and. aldiff .ge. 0.) wgt2=.5*aldiff
           if(qdata(i-1,4) .ge. ansig) then
            wgt2=0.
            wgt1=0.
           end if
          end if
         end if
        end if
        if(i .ne. nqdata) then
         if(qdata(i,6) .eq. qdata(i+1,6) .and. qdata(i,4) .lt.
     *        ansig) then
          if(qdata(i,2) .eq. qdata(i+1,2) .and. qdata(i,3) .eq.
     *       qdata(i+1,3) .and. rqtype(i).eq. rqtype(i+1)) then
           aldiff=qdata(i+1,4)-qdata(i,4)
           if(aldiff .lt. 1. .and. aldiff .ge. 0.) wgt1=.5*aldiff
          end if
         end if
        end if
        qdata(i,1)=qdata(i,1)*(wgt1+wgt2)
       end do
       if(deltat .le. 0.) then
        do l=1,nqdata
          qdata(l,7)=0.
        end do
       else
        do l=1,nqdata
          qdata(l,7)=min(max(-1.,qdata(l,7)/deltat),0.)
        end do
       end if
       call intrp3(rq,tempq,qdata(1,3),qdata(1,2),qdata(1,4),
     *      nlath*2,nlon,nsig,nqdata)
       do l=1,nqdata
        qges(l)=(1.+qdata(l,7))*tempq(l)
       end do
      end if
      if(jiter.eq.1) dstb=dstlast
c-------
c------- read in previous analysis if necessary
c-------
      if(deltat .gt. 0.) then 
       call inguess(ru,rv,rt,rp,rq,rpw,sigi,sigl,
     *   iianl,jcap,nsig,nlath,nlon,hourg,idateg,
     *   pln,qln,rln,trigs,ifax,ml2lm,factslm,factvlm)
c-------
c------- add in contibutions from previous analysis time
c-------
c------- first surface pressure
c-------
       if(npdata.gt.0) then
        call intrp2(rp,tempp,psdata(1,3),psdata(1,2),nlath*2,
     *        nlon,npdata)
        do l=1,npdata
         pges(l)=pges(l)-psdata(l,8)*tempp(l)
        end do
       end if
c--------
c-------- next precipitable water
c--------
       if(npwdat.gt.0) then
c--------
c-------- obtain guess precip. water at obs locations
c--------
        do k=1,nsig
         call intrp2(rq(1,1,k),temppw,pwdata(1,3),pwdata(1,2),
     *     2*nlath,nlon,npwdat)
         do l=1,npwdat
          pwges(l)=pwges(l)-pwdata(l,6)*temppw(l)*pwdata(l,5)*
     *                                   pwcon(k)
         end do
        end do
       end if
c-------
c-------- next do wind residuals
c--------
       if(nwdata.gt.0) then
        call intrp3(ru,tempw,wdata(1,3),wdata(1,2),wdata(1,4),
     *     2*nlath,nlon,nsig,nwdata)
        do l=1,nwdata
         uges(l)=uges(l)-wdata(l,8)*tempw(l)*fact(l)
        end do
        call intrp3(rv,tempw,wdata(1,3),wdata(1,2),wdata(1,4),
     *      2*nlath,nlon,nsig,nwdata)
        do l=1,nwdata
         vges(l)=vges(l)-wdata(l,8)*tempw(l)*fact(l)
        end do
       end if
c--------
c-------- satellite temperature
c--------
       if(nsdata.gt.0) then
        call intrp3(rt,temps,sdata(1,2),sdata(1,1),sdata(1,3),
     *     nlath*2,nlon,nsig,nsdata)
        do l=1,nsdata
         sges(l)=sges(l)-sdata(l,5)*temps(l)
        end do
       end if
c--------
c-------- now temperature
c--------
       if(ntdata.gt.0) then
        call intrp3(rt,tempt,tdata(1,3),tdata(1,2),tdata(1,4),
     *     nlath*2,nlon,nsig,ntdata)
        do l=1,ntdata
         tges(l)=tges(l)-tdata(l,7)*tempt(l)
        end do
        if(nqtdata .gt. 0) then
         call intrp3(rq,tempt,tdata(1,3),tdata(1,2),tdata(1,4),
     *      nlath*2,nlon,nsig,ntdata)
         do l=1,ntdata
          if(iqtflg(l).eq.1) qtges(l)=qtges(l)-tdata(l,7)*tempt(l)
         end do    
        end if
       end if
c--------
c-------- next q
c--------
       if(nqdata.gt.0) then
        call intrp3(rq,tempq,qdata(1,3),qdata(1,2),qdata(1,4),
     *     nlath*2,nlon,nsig,nqdata)
        do l=1,nqdata
         qges(l)=qges(l)-qdata(l,7)*tempq(l)
        end do
       end if
      end if
c
c         calculate residuals, put observation infor. in files
c         and print statistics
c
      print *,' blat,elat =',blat,elat
      if(npdata.gt.0) then
       do i=1,npdata
        psdata(i,4)=psdata(i,4)-pges(i)
       end do
       call respsf(psdata(1,4),ptype,npdata)
       call sprp(psdata,pges,ptype,npdata,nprecs,nlath*2,nlon,
     *     psfile,ermaxp,erminp,grossp)
      end if
      if(nwdata.gt.0) then
       do i=1,nwdata
c       if(nint(rwtype(i)) .ne. 283)then
         wdata(i,5)=wdata(i,5)-uges(i)
         wdata(i,6)=wdata(i,6)-vges(i)
c       else
c        spdges=sqrt(uges(i)*uges(i)+vges(i)*vges(i))
c        spdssm=sqrt(wdata(i,5)*wdata(i,5)+wdata(i,6)*wdata(i,6))
c        spdn=(spdssm-spdges)/spdges
c        wdata(i,5)=spdssm
c        wdata(i,6)=spdges
c       end if
       end do
       call residw(wdata(1,4),
     *     wdata(1,5),wdata(1,6),rwtype,
     *     wpres,nwdata)
       call spruv(wdata,uges,vges,fact,rwtype,nwdata,nwrecs,nlath*2,
     *    nlon,nsig,uvfile,ermaxw,erminw,grossw)
      end if
      msat=0
      if(nsdata.gt.0) then
       do i=1,nsdata
        sdata(i,4)=sdata(i,4)-sges(i)
       end do
       call ressat(sdata(1,3),
     *    sdata(1,4),sdata(1,6),spres,nsdata)
       do ll=1,nsdata
        if((sdata(ll,6) .gt. 164.5 .and. sdata(ll,6) .lt. 169.5) .or.
     *    (sdata(ll,6) .gt. 174.5 .and. sdata(ll,6) .lt. 179.5)) then
         do l=1,nsig-4
          isatv(l)=-1
         end do
         go to 744
        end if
       end do
 744   continue
       call sprs(sdata,nsdata,nlath,nlon,nsig,
     *     msat,sfile,nsigsat,isat,blat,elat,grossst,sigl)
      end if
      if(ntdata.gt.0) then
       if(nqtdata .gt. 0) then
        do i=1,ntdata
         if(iqtflg(i).eq.1) then
c         print *,i,tdata(i,5),qtges(i)
          tdata(i,5)=tdata(i,5)*(1.+.608*qtges(i))
         end if
        end do
       end if
       do i=1,ntdata
        tdata(i,5)=tdata(i,5)-tges(i)
       end do
       call restmp(tdata(1,4),
     *     tdata(1,5),tdata(1,8),tpres,ntdata)
       call sprt(tdata,tges,ntdata,ntrecs,nlath*2,nlon,nsig,
     *    tfile,ermaxt,ermint,grosst)
      end if
      if(npwdat.gt.0 .or. nqdata .gt. 0) then
       if(npwdat.gt.0) then
        do i=1,npwdat
         pwdata(i,4)=pwdata(i,4)-pwges(i)
        end do
        call respw(pwdata(1,1),pwdata(1,4),pwmerr,pwtype,npwdat)
       end if
       if(nqdata.gt.0) then
        do i=1,nqdata
         qdata(i,5)=qdata(i,5)-qges(i)
         tempq(i)=rbqs(i)
        end do
        call resq(qdata(1,1),qdata(1,4),
     *     qdata(1,5),rqtype,
     *     qpres,nqdata,qmaxerr,tempq)
       end if
       call sprqpw(qdata,qges,rqtype,nqdata,nqrecs,
     *    pwdata,pwges,pwtype,npwdat,npwrecs,
     *    nlath*2,nlon,nsig,
     *    qfile,pwfile,ermaxpw,erminpw,grosspw,
     *               ermaxq,erminq,grossq,rbqs)
      end if
      return
      end
