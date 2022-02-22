      subroutine spruv(wdata,uges,vges,fact,wtype,nwdta,nwrecs,
     *  nlat,nlon,nsig,uvfile,ermax,ermin,gross)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    spruv       save wind infor.        
c   prgmmr: parrish          org: w/nmc22    date: 90-10-13
c
c abstract: save wind information for later use.
c
c program history log:
c   90-10-13  parrish
c
c   input argument list:
c     wdata    - obs info at obs locations
c     uges,vges - guess u,v values
c     fact     - reduction to 10,20 m factor
c     wtype    - wind type
c     nwdta    - number of obs
c     nlat     - number of latitudes on gaussian grid
c     nlon     - number of longitudes on gaussian grid
c     nsig     - number of layers on gaussian grid
c     nblk     - blocking factor for output file
c     iunit    - output scratch unit
c     ermax,ermin,gross - parameters for gross error test
c
c   output argument list:
c     nwrecs   - number of records of wind data
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      dimension wdata(nwdta,7)
      dimension uges(nwdta),vges(nwdta),wtype(nwdta)
      dimension numw(nsig)
      dimension fact(nwdta)
      dimension uplty(nsig),vplty(nsig)
      dimension icount(nlat,nlon,nsig),ncount(nwdta)
      dimension jl(128,6)
      dimension uvfile(*)
c--------
      obermax=-1.e50
      obermin=1.e50
      resmax=-1.e50
      ratmax=-1.e50
      numgross=0
      numw=0
      ntot=0
      ier=1
      ilon=2
      ilat=3
      isig=4
      iures=5
      ivres=6
      nlth=nlat/2
      factor=2.0
      ssmpen=0.
      numssm=0
      uplty=0.
      umplty=0.
      vplty=0.
      vmplty=0.
      npp=17
      anlon=float(nlon)
      irec=0
      inc=nwdta/128
      inc=max(inc,1)
      nwttot=0
      ncount=0
      is=1
      do 200 kk=1,nwdta
        ngrp=0
        icount=0
        issave=is
        is=is+1
        i128=1
        numdat=0
        do 100 iii=1,5*inc
        ibeg=mod(iii-1,inc)+1
        do 100 i=ibeg,nwdta,inc
        if(ncount(i) .gt. 0)go to 100
        jlat=wdata(i,ilat)
        if(wdata(i,ilon).ge. anlon+1.)wdata(i,ilon)=
     *     wdata(i,ilon)-anlon
        if(wdata(i,ilon).lt. 1.)wdata(i,ilon)=wdata(i,ilon)+anlon
        jlon=wdata(i,ilon)
        jsig=wdata(i,isig)
        dx=wdata(i,ilon)-jlon
        dy=wdata(i,ilat)-jlat
        ds=wdata(i,isig)-jsig
        jlat=max(1,min(jlat,nlat))
        jsig=max(1,min(jsig,nsig))
        if(icount(jlat,jlon,jsig) .eq. 1)go to 100
        jlatp=jlat+1
        jlatp=min(jlatp,nlat)
        if(icount(jlatp,jlon,jsig) .eq. 1)go to 100
        jsigp=jsig+1
        jsigp=min(jsigp,nsig)
        if(icount(jlat,jlon,jsigp) .eq. 1)go to 100
        if(icount(jlatp,jlon,jsig) .eq. 1)go to 100
        jlonp=jlon+1
        if(jlonp .gt. nlon)jlonp=jlonp-nlon
        if(icount(jlat,jlonp,jsigp) .eq. 1)go to 100
        if(icount(jlat,jlonp,jsig) .eq. 1)go to 100
        if(icount(jlatp,jlonp,jsigp) .eq. 1)go to 100
        if(icount(jlatp,jlonp,jsig) .eq. 1)go to 100
        if(jlat .le. nlth)wdata(i,ier)=wdata(i,ier)*factor
c-----------------------------------gross error test added here
        obserror=1./max(sqrt(wdata(i,ier)),1.e-10)
        obserrlm=max(ermin,min(ermax,obserror))
        residual=sqrt(wdata(i,iures)**2+wdata(i,ivres)**2)
        ratio=residual/obserrlm
        if(obserror.lt.1.e5) obermax=max(obermax,obserror)
        obermin=min(obermin,obserror)
        resmax=max(resmax,residual)
        ratmax=max(ratmax,ratio)
        if(ratio.gt.gross) then
         numgross=numgross+1
         wdata(i,ier)=0.
        end if
        if(nint(wtype(i)) .ne. 283)then
        uplty(jsig)=uplty(jsig)+wdata(i,ier)*wdata(i,iures)**2
        vplty(jsig)=vplty(jsig)+wdata(i,ier)*wdata(i,ivres)**2
        numw(jsig)=numw(jsig)+1
        else
        ssmpen=ssmpen+wdata(i,ier)*(wdata(i,iures)-wdata(i,ivres))
     *     **2
        numssm=numssm+1
        end if
        icount(jlat,jlon,jsig)=1
        icount(jlatp,jlon,jsig)=1
        icount(jlat,jlonp,jsig)=1
        icount(jlatp,jlonp,jsig)=1
        icount(jlat,jlon,jsigp)=1
        icount(jlatp,jlon,jsigp)=1
        icount(jlat,jlonp,jsigp)=1
        icount(jlatp,jlonp,jsigp)=1
        jl(i128,1)=jlat
        jl(i128,2)=jlon
        jl(i128,3)=jsig
        jl(i128,4)=jlatp
        jl(i128,5)=jlonp
        jl(i128,6)=jsigp
        wdata(i,ier)=sqrt(wdata(i,ier))
        wgt000=fact(i)*wdata(i,ier)*(1.0-dx)*(1.0-dy)*(1.0-ds)
        wgt010=fact(i)*wdata(i,ier)*dx*(1.0-dy)*(1.0-ds)
        wgt100=fact(i)*wdata(i,ier)*(1.-dx)*dy*(1.0-ds)
        wgt110=fact(i)*wdata(i,ier)*dx*dy*(1.0-ds)
        wgt001=fact(i)*wdata(i,ier)*(1.-dx)*(1.-dy)*ds
        wgt011=fact(i)*wdata(i,ier)*dx*(1.-dy)*ds
        wgt101=fact(i)*wdata(i,ier)*(1.-dx)*dy*ds
        wgt111=fact(i)*wdata(i,ier)*dx*dy*ds
        uvfile(is)=jlat+.001
        uvfile(is+1)=jlon+.001
        uvfile(is+2)=jsig+.001
        uvfile(is+3)=jlatp+.001
        uvfile(is+4)=jlonp+.001
        uvfile(is+5)=jsigp+.001
        uvfile(is+6)=wgt000
        uvfile(is+7)=wgt100
        uvfile(is+8)=wgt010
        uvfile(is+9)=wgt110
        uvfile(is+10)=wgt001
        uvfile(is+11)=wgt101
        uvfile(is+12)=wgt011
        uvfile(is+13)=wgt111
        uvfile(is+14)=wdata(i,iures)
        uvfile(is+15)=wdata(i,ivres)
        uvfile(is+16)=wdata(i,ier)
c       uvfile(is+17)=uges(i)
c       uvfile(is+18)=vges(i)
c       uvfile(is+19)=wtype(i)+.001
        nwttot=nwttot+1
        is=is+npp
        ngrp=ngrp+1
        i128=i128+1
        ncount(i)=1
        if(i128 .eq. 129)i128=1
        if(ngrp .gt. 128)then
        icount(jl(i128,1),jl(i128,2),jl(i128,3))=0
        icount(jl(i128,4),jl(i128,2),jl(i128,3))=0
        icount(jl(i128,1),jl(i128,5),jl(i128,3))=0
        icount(jl(i128,4),jl(i128,5),jl(i128,3))=0
        icount(jl(i128,1),jl(i128,2),jl(i128,6))=0
        icount(jl(i128,4),jl(i128,2),jl(i128,6))=0
        icount(jl(i128,1),jl(i128,5),jl(i128,6))=0
        icount(jl(i128,4),jl(i128,5),jl(i128,6))=0
        end if
100   continue
c121   continue
      uvfile(issave)=ngrp+.001
      irec=irec+1
      if(nwttot .eq. nwdta)go to 201
 200  continue
 201  nwrecs=irec
	write(*,*) 'nwttot, nwdta=', nwttot, nwdta
	write(*,*) 'uvfile(1..20)=', (uvfile(i),i=1,20)
      do 251 k=1,nsig
        umplty=umplty+uplty(k)
        vmplty=vmplty+vplty(k)
        ntot=ntot+numw(k)
        write(6,240)numw(k),k,uplty(k),vplty(k)
240     format(' number of wind obs=',i9,' at level ',i4,' pen = ',
     *     2e12.4)
251   continue
      print *,' total number of ssm/i obs=',numssm
      print *,' total number of wind obs=',ntot
      print *,' ssm/i penalty = ', ssmpen
      tu=umplty/float(ntot)
      write(6,930)umplty,tu
930   format(' total u obs penalty=',e12.4,e12.4)
      tv=vmplty/float(ntot)
      write(6,940)vmplty,tv
940   format(' total v obs penalty=',e12.4,e12.4)
       write(6,*)' gross error check for winds:'
       write(6,*)'   obs error max,min=',obermax,obermin
       write(6,*)'   for check, obs error bounded by ',ermin,ermax
       write(6,*)'   for check, max ratio residual/ob error =',gross
       write(6,*)'   max residual=',resmax
       write(6,*)'   max ratio=',ratmax
       write(6,*)'   number obs that failed gross test = ',numgross
      return
      end
