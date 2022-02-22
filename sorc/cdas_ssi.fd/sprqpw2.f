      subroutine sprqpw(qdata,qges,qtype,nqdta,nqrecs,
     *  pwdata,pwges,pwtype,npwdta,npwrecs,
     *  nlat,nlon,nsig,qfile,pwfile,ermaxpw,erminpw,grosspw,
     *  ermaxq,erminq,grossq,rbqs)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    sprq        store infor. for q and p.w. obs.
c   prgmmr: parrish          org: w/nmc22    date: 90-10-12
c
c abstract: store information for q and p.w. obs.
c
c program history log:
c   90-10-12  parrish
c
c   input argument list:
c     qdata    - obs info at obs locations - q
c     qges     - guess values for observations - q
c     qtype    - observation types - q
c     nqdta    - number of obs - q
c     pwdata   - obs info at obs locations - p.w.
c     pwges    - guess values for observations - p.w.
c     pwtype   - observation types - p.w.
c     npwdta   - number of obs - p.w.
c     nlat     - number of latitudes on gaussian grid
c     nlon     - number of longitudes on gaussian grid
c     nsig     - number of layers on gaussian grid
c     nblk     - blocking factor for output file containing data
c     iunit    - unit number for output file containing data
c     ermaxpw,erminpw,grosspw - parameters for gross check of p.w. obs
c     ermaxq,erminq,grossq - parameters for gross check of q obs
c     rbqs     - guess saturation spec. hum.
c
c   output argument list:
c     nqrecs   - number of records for q data
c     npwrecs  - number of records for p.w. data
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      dimension pwdata(npwdta,5)
      dimension pwges(npwdta),pwtype(npwdta)
      dimension qdata(nqdta,6)
      dimension qges(nqdta),qtype(nqdta)
      dimension numq(nsig)
      dimension qplty(nsig)
      dimension qfile(*),pwfile(*)
      dimension icount(nlat,nlon,nsig),ncount(nqdta)
      dimension jl(128,6)
      dimension rbqs(nqdta)
c--------
c wne mod
      numq = 0
c---
      numtemps=0
      ier=1
      ilon=2
      ilat=3
      ipres=4
      ipsfc=5
      nlth=nlat/2
c   increase obs errors in the n.h. by a factor of 2 to account
      factor=2.
      nsuperp=0
      pwplty=0.
      npp=11
c     print *,npp,ngrp,npwdta
      if(npwdta .eq. 0)go to 1000
      obermax=-1.e50
      obermin=1.e50
      resmax=-1.e50
      ratmax=-1.e50
      numgrspw=0
      ermax=ermaxpw
      ermin=erminpw
      gross=grosspw
      ngrp=0
      irec=0
      is=1
      issave=1
c--------
c-------- initialize grids
c--------
      anlon=float(nlon)
      do 300 i=1,npwdta
        jlat=pwdata(i,ilat)
        if(pwdata(i,ilon).ge. anlon+1.)pwdata(i,ilon)=
     *    pwdata(i,ilon)-anlon
        if(pwdata(i,ilon).lt. 1.)pwdata(i,ilon)=pwdata(i,ilon)+anlon
        jlon=pwdata(i,ilon)
        jlat=max(1,min(jlat,nlat))
        dy=pwdata(i,ilat)-jlat
        dx=pwdata(i,ilon)-jlon
        jlonp=jlon+1
        if(jlonp .gt. nlon)jlonp=jlonp-nlon
        jlatp=jlat+1
        jlatp=min(jlatp,nlat)
        if(jlat .lt. nlth)pwdata(i,ier)=pwdata(i,ier)*factor
        pwdata(i,ier)=sqrt(pwdata(i,ier))
c-----------------------------------gross error test added here
        obserror=1./max(pwdata(i,ier),1.e-10)
        obserrlm=max(ermin,min(ermax,obserror))
        residual=abs(pwdata(i,ipres))
        ratio=residual/obserrlm
        if(obserror.lt.1.e5) obermax=max(obermax,obserror)
        obermin=min(obermin,obserror)
        resmax=max(resmax,residual)
        ratmax=max(ratmax,ratio)
        if(ratio.gt.gross) then
         numgrspw=numgrspw+1
         pwdata(i,ier)=0.
        end if
        valx=pwdata(i,ier)*pwdata(i,ipres)
        wgt00=pwdata(i,ier)*(1.0-dx)*(1.0-dy)
        wgt10=pwdata(i,ier)*(1.0-dx)*dy
        wgt01=pwdata(i,ier)*dx*(1.0-dy)
        wgt11=pwdata(i,ier)*dx*dy
        pwfile(is+1)=jlat
        pwfile(is+2)=jlon
        pwfile(is+3)=jlatp
        pwfile(is+4)=jlonp
        pwfile(is+5)=wgt00
        pwfile(is+6)=wgt10
        pwfile(is+7)=wgt01
        pwfile(is+8)=wgt11
        pwfile(is+9)=pwdata(i,ipsfc)
        pwfile(is+10)=pwdata(i,ipres)
        pwfile(is+11)=pwdata(i,ier)
c       pwfile(is+12)=pwges(i)
c       pwfile(is+13)=pwtype(i)
        is=is+npp
        ngrp=ngrp+1
        pwplty=pwplty+valx*valx
        nsuperp=nsuperp+1
300   continue
      pwfile(issave)=ngrp
      irec=irec+1
      npwrecs=irec
      print *,' number of precip. water sprobs=',nsuperp
      write(6,956)pwplty
956   format(' total p.w. obs penalty=',e12.4)
       write(6,*)' gross error check for total precip water:'
       write(6,*)'   obs error max,min=',obermax,obermin
       write(6,*)'   for check, obs error bounded by ',ermin,ermax
       write(6,*)'   for check, max ratio residual/ob error =',gross
       write(6,*)'   max residual=',resmax
       write(6,*)'   max ratio=',ratmax
       write(6,*)'   number obs that failed gross test = ',numgrspw
c  begin moisture observations
1000  numpq=0
      obermax=-1.e50
      obermin=1.e50
      olatmin=0.
      olonmin=0.
      osigmin=0.
      resmax=-1.e50
      ratmax=-1.e50
      numgrsq=0
      ermax=ermaxq
      ermin=erminq
      gross=grossq
      qplty=0.
      ntot=0
      ier=1
      ilon=2
      ilat=3
      isig=4
      iqres=5
      is=1
      irec=0
      npp=16
      ncount=0
      nqttot=0
      inc=nqdta/128
      inc=max(inc,1)
      anlon=float(nlon)
      is=1
      do 200 kk=1,nqdta
      icount=0
      issave=is
      is=is+1
      i128=1
      ngrp=0
      do 100 iii=1,5*inc
        ibeg=mod(iii-1,inc)+1
        do 100 i=ibeg,nqdta,inc
        if(ncount(i) .gt. 0)go to 100
c--------
        jlat=qdata(i,ilat)
        if(qdata(i,ilon).ge. anlon+1.)qdata(i,ilon)=
     *     qdata(i,ilon)-anlon
        if(qdata(i,ilon).lt. 1.)qdata(i,ilon)=qdata(i,ilon)+anlon
        jlon=qdata(i,ilon)
        jsig=qdata(i,isig)
        dx=qdata(i,ilon)-jlon
        dy=qdata(i,ilat)-jlat
        ds=qdata(i,isig)-jsig
        jlat=max(1,min(jlat,nlat))
        jsig=max(1,min(jsig,nsig))
        if(icount(jlat,jlon,jsig) .eq. 1)go to 100
        jlatp=jlat+1
        jlatp=min(jlatp,nlat)
        if(icount(jlatp,jlon,jsig) .eq. 1)go to 100
        jsigp=jsig+1
        jsigp=min(jsigp,nsig)
        if(icount(jlatp,jlon,jsigp) .eq. 1)go to 100
        if(icount(jlat,jlon,jsigp) .eq. 1)go to 100
        jlonp=jlon+1
        if(jlonp .gt. nlon)jlonp=jlonp-nlon
        if(icount(jlatp,jlonp,jsig) .eq. 1)go to 100
        if(icount(jlat,jlonp,jsig) .eq. 1)go to 100
        if(icount(jlatp,jlonp,jsigp) .eq. 1)go to 100
        if(icount(jlat,jlonp,jsigp) .eq. 1)go to 100
        icount(jlat,jlon,jsig)=1
        icount(jlat,jlonp,jsig)=1
        icount(jlatp,jlon,jsig)=1
        icount(jlatp,jlonp,jsig)=1
        icount(jlat,jlon,jsigp)=1
        icount(jlat,jlonp,jsigp)=1
        icount(jlatp,jlon,jsigp)=1
        icount(jlatp,jlonp,jsigp)=1
        jl(i128,1)=jlat
        jl(i128,2)=jlon
        jl(i128,3)=jsig
        jl(i128,4)=jlatp
        jl(i128,5)=jlonp
        jl(i128,6)=jsigp
        if(jlat .le. nlth)qdata(i,ier)=qdata(i,ier)*factor
        qdata(i,ier)=sqrt(qdata(i,ier))
c-----------------------------------gross error test added here
        obserror=1./max(qdata(i,ier),1.e-10)
        obserror=obserror*100./rbqs(i)
        obserrlm=max(ermin,min(ermax,obserror))
        residual=abs(qdata(i,iqres)*100./rbqs(i))
        ratio=residual/obserrlm
        if(obserror.lt.1.e5) obermax=max(obermax,obserror)
        if(obermin.ge.obserror) then
         obermin=obserror
         olatmin=jlat
         olonmin=jlon
         osigmin=jsig
        end if
c       obermin=min(obermin,obserror)
        resmax=max(resmax,residual)
        ratmax=max(ratmax,ratio)
        if(ratio.gt.gross) then
         numgrsq=numgrsq+1
         qdata(i,ier)=0.
        end if
        val=qdata(i,ier)*qdata(i,iqres)
        wgt000=qdata(i,ier)*(1.0-dx)*(1.0-dy)*(1.0-ds)
        wgt010=qdata(i,ier)*dx*(1.0-dy)*(1.0-ds)
        wgt100=qdata(i,ier)*(1.-dx)*dy*(1.0-ds)
        wgt110=qdata(i,ier)*dx*dy*(1.0-ds)
        wgt001=qdata(i,ier)*(1.-dx)*(1.-dy)*ds
        wgt011=qdata(i,ier)*dx*(1.-dy)*ds
        wgt101=qdata(i,ier)*(1.-dx)*dy*ds
        wgt111=qdata(i,ier)*dx*dy*ds
        qfile(is)=jlat
        qfile(is+1)=jlon
        qfile(is+2)=jsig
        qfile(is+3)=jlatp
        qfile(is+4)=jlonp
        qfile(is+5)=jsigp
        qfile(is+6)=wgt000
        qfile(is+7)=wgt100
        qfile(is+8)=wgt010
        qfile(is+9)=wgt110
        qfile(is+10)=wgt001
        qfile(is+11)=wgt101
        qfile(is+12)=wgt011
        qfile(is+13)=wgt111
        qfile(is+14)=qdata(i,iqres)
        qfile(is+15)=qdata(i,ier)
c       qfile(is+16)=qges(i)
c       qfile(is+17)=qtype(i)
        nqttot=nqttot+1
        is=is+npp
        i128=i128+1
        ncount(i)=1
        ngrp=ngrp+1
        qplty(jsig)=qplty(jsig)+val*val
        numq(jsig)=numq(jsig)+1
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
121   continue
      irec=irec+1
      qfile(issave)=ngrp
c     print *,ngrp
      if(nqttot .eq. nqdta)go to 201
 200  continue
 201  nqrecs=irec
      qmplty=0.
      do 251 k=1,nsig
        qmplty=qmplty+qplty(k)
        ntot=ntot+numq(k)
        write(6,240)numq(k),k,qplty(k)
240     format(' there are ',i9,' q obs at level ',i4,
     *     ' pen =',e12.4)
251   continue
      print *,' total number of q-component obs=',ntot
      print *,' total q obs penalty=',qmplty
       write(6,*)' gross error check for specific humidity:'
       write(6,*)'  (scaled as precent of guess specific humidity)'
       write(6,*)'   obs error max,min=',obermax,obermin
       write(6,*)'  coords of min err, lat,lon,sig=',
     *                   olatmin,olonmin,osigmin
       write(6,*)'   for check, obs error bounded by ',ermin,ermax
       write(6,*)'   for check, max ratio residual/ob error =',gross
       write(6,*)'   max residual=',resmax
       write(6,*)'   max ratio=',ratmax
       write(6,*)'   number obs that failed gross test = ',numgrsq
      return
      end
