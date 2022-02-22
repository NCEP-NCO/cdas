      subroutine sprt(tdata,tges,ntdta,ntrecs,nlat,nlon,nsig,
     *  tfile,ermax,ermin,gross)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    sprt        save obs infor. for conv. temps.
c   prgmmr: parrish          org: w/nmc22    date: 90-10-12
c
c abstract: save obs infor. for conv. temps.   
c
c program history log:
c   90-10-12  parrish
c
c   input argument list:
c     tdata    - obs info at obs locations
c     tges     - guess temperature
c     ntdta    - number of obs
c     nlat     - number of latitudes on gaussian grid
c     nlon     - number of longitudes on gaussian grid
c     nsig     - number of layers on gaussian grid
c     ermax,ermin,gross - parameters for gross error test of data
c
c   output argument list:
c     ntrecs   - number of temperature records on iunit
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      dimension tdata(ntdta,8),tges(ntdta)
c--------
c-------- local space
c--------
      dimension numtemps(nsig)
      dimension tplty(nsig)
      dimension tfile(*)
      dimension icount(nlat,nlon,nsig),ncount(ntdta+2*nlat*nlon)
      dimension jl(128,6)
c--------
      obermax=-1.e50
      obermin=1.e50
      resmax=-1.e50
      ratmax=-1.e50
      numgross=0
      numtemps=0
      tplty=0.
      ntot=0
      nsatot=0
      ier=1
      ilon=2
      ilat=3
      isig=4
      itres=5
      itype=8
      nlth=nlat/2
      npp=16
c   increase obs errors in the n.h. by a factor of 2 to account
c   for n.s. guess error difference
      factor=2.0
      anlon=float(nlon)
c     nttot=ntdta+2*nlat*nlon
      nttot=ntdta
      ndttot=0
      ncount=0
      inc=nttot/128
      inc=max(inc,1)
      nrecs=0
      is=1
      do 200 kk=1,nttot
      icount=0
      i128=1
      issave=is
      is=is+1
      numdat=0
      do 120 iii=1,5*inc
      ibeg=mod(iii-1,inc)+1
      do 120 i=ibeg,nttot,inc
        if(ncount(i) .gt. 0)go to 120
c       if(i .le. ntdta)then
        jlat=tdata(i,ilat)
        if(tdata(i,ilon).ge. anlon+1.)tdata(i,ilon)=
     *    tdata(i,ilon)-anlon
        if(tdata(i,ilon).lt. 1.)tdata(i,ilon)=tdata(i,ilon)+anlon
        jlon=tdata(i,ilon)
        jsig=tdata(i,isig)
        dx=tdata(i,ilon)-jlon
        dy=tdata(i,ilat)-jlat
        ds=tdata(i,isig)-jsig
        jlat=max(1,min(jlat,nlat))
        jsig=max(1,min(jsig,nsig))
        if(icount(jlat,jlon,jsig) .eq. 1)go to 120
        jlatp=jlat+1
        jlatp=min(jlatp,nlat)
        if(icount(jlatp,jlon,jsig) .eq. 1)go to 120
        jsigp=jsig+1
        jsigp=min(jsigp,nsig)
        if(icount(jlat,jlon,jsigp) .eq. 1)go to 120
        if(icount(jlatp,jlon,jsigp) .eq. 1)go to 120
        jlonp=jlon+1
        if(jlonp .gt. nlon)jlonp=jlonp-nlon
        if(icount(jlatp,jlonp,jsigp) .eq. 1)go to 120
        if(icount(jlatp,jlonp,jsig) .eq. 1)go to 120
        if(icount(jlat,jlonp,jsigp) .eq. 1)go to 120
        if(icount(jlat,jlonp,jsig) .eq. 1)go to 120
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
        if(jlat .le. nlth)tdata(i,ier)=tdata(i,ier)*factor
        tdata(i,ier)=sqrt(tdata(i,ier))
c-----------------------------------gross error test added here
        obserror=1./max(tdata(i,ier),1.e-10)
        obserrlm=max(ermin,min(ermax,obserror))
        residual=abs(tdata(i,itres))
        ratio=residual/obserrlm
        if(obserror.lt.1.e5) obermax=max(obermax,obserror)
        obermin=min(obermin,obserror)
        resmax=max(resmax,residual)
        ratmax=max(ratmax,ratio)
        if(ratio.gt.gross) then
         numgross=numgross+1
         tdata(i,ier)=0.
        end if
        val=tdata(i,ier)*tdata(i,itres)
        wgt000=tdata(i,ier)*(1.0-dx)*(1.0-dy)*(1.0-ds)
        wgt010=tdata(i,ier)*dx*(1.0-dy)*(1.0-ds)
        wgt100=tdata(i,ier)*(1.-dx)*dy*(1.0-ds)
        wgt110=tdata(i,ier)*dx*dy*(1.0-ds)
        wgt001=tdata(i,ier)*(1.-dx)*(1.-dy)*ds
        wgt011=tdata(i,ier)*dx*(1.-dy)*ds
        wgt101=tdata(i,ier)*(1.-dx)*dy*ds
        wgt111=tdata(i,ier)*dx*dy*ds
        tfile(is)=jlat+.001
        tfile(is+1)=jlon+.001
        tfile(is+2)=jsig+.001
        tfile(is+3)=jlatp+.001
        tfile(is+4)=jlonp+.001
        tfile(is+5)=jsigp+.001
        tfile(is+6)=wgt000
        tfile(is+7)=wgt100
        tfile(is+8)=wgt010
        tfile(is+9)=wgt110
        tfile(is+10)=wgt001
        tfile(is+11)=wgt101
        tfile(is+12)=wgt011
        tfile(is+13)=wgt111
        tfile(is+14)=tdata(i,itres)
        tfile(is+15)=tdata(i,ier)
c       tfile(is+16)=tges(i)
c       tfile(is+17)=tdata(i,itype)
        tplty(jsig)=tplty(jsig)+val*val
        numtemps(jsig)=numtemps(jsig)+1
c
c   include pseudo-observations near surface to make in balance
c   with surface boundary conditions
c
        i128=i128+1
        ncount(i)=1
        ndttot=ndttot+1
        numdat=numdat+1
        is=is+npp
        if(i128 .eq. 129)i128=1
        if(numdat .gt. 128)then
        icount(jl(i128,1),jl(i128,2),jl(i128,3))=0
        icount(jl(i128,4),jl(i128,2),jl(i128,3))=0
        icount(jl(i128,1),jl(i128,5),jl(i128,3))=0
        icount(jl(i128,4),jl(i128,5),jl(i128,3))=0
        icount(jl(i128,1),jl(i128,2),jl(i128,6))=0
        icount(jl(i128,4),jl(i128,2),jl(i128,6))=0
        icount(jl(i128,1),jl(i128,5),jl(i128,6))=0
        icount(jl(i128,4),jl(i128,5),jl(i128,6))=0
        end if
 120  continue
 121  continue
      tfile(issave)=numdat+.001
      nrecs=nrecs+1
      if(ndttot .eq. nttot)go to 201
200   continue
201   tmplty=0.
      do 251 k=1,nsig
        ntot=ntot+numtemps(k)
        tmplty=tmplty+tplty(k)
        write(6,240)numtemps(k),k,tplty(k)
240     format(' there are ',i9,' temps at level ',i3,' pen = ',e12.4)
251   continue
      print *,' total number of nosat temps=',ntot
      write(6,950)tmplty
950   format(' total t obs penalty=',e12.4,e12.4)
      ntrecs=nrecs
c     print *,npp,ntdta
       write(6,*)' gross error check for temps:'
       write(6,*)'   obs error max,min=',obermax,obermin
       write(6,*)'   for check, obs error bounded by ',ermin,ermax
       write(6,*)'   for check, max ratio residual/ob error =',gross
       write(6,*)'   max residual=',resmax
       write(6,*)'   max ratio=',ratmax
       write(6,*)'   number obs that failed gross test = ',numgross
      return
      end
