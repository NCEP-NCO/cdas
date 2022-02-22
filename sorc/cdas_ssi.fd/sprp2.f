      subroutine sprp(pdata,pges,ptype,npsdta,npsrecs,nlat,nlon,
     *   psfile,ermax,ermin,gross)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    sprp        store psfc info.
c   prgmmr: parrish          org: w/nmc22    date: 90-10-12
c
c abstract: store information for psfc data.
c
c program history log:
c   90-10-12  parrish
c
c   input argument list:
c     pdata    - obs info at obs locations
c     pges     - guess values for psfc.
c     ptype    - observation types
c     npsdta   - number of obs
c     nlat     - number of latitudes on gaussian grid
c     nlon     - number of longitudes on gaussian grid
c     nblk     - blocking data for file iunit
c     iunit    - output file for obs. information
c     ermax,ermin,gross - parameters for gross error check
c
c   output argument list:
c     npsdta   - number of records of psfc. obs
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      dimension pdata(npsdta,7),pges(npsdta),ptype(npsdta)
      dimension icount(nlat,nlon)
      dimension ncount(npsdta),jl(128,4)
      dimension psfile(*)
c--------
      obermax=-1.e50
      obermin=1.e50
      resmax=-1.e50
      ratmax=-1.e50
      numgross=0
      ncount=0
      ier=1
      ilon=2
      ilat=3
      ipres=4
      nsuperp=0
      psplty=0.
      numw=0
      npp=10
      ndcnt=0
c
c   increase obs errors in the n.h. by a factor of 2 to account
c--------
c-------- initialize grids
c--------
      anlon=float(nlon)
      nlth=nlat/2
      factor=2.0
      nrecs=0
      inc=npsdta/128
      inc=max(inc,1)
      is=1
      do 200 k=1,npsdta
      issave=is
      is=is+1
      icount=0
      numdat=0
      i128=1
      do 100 kk=1,inc*5
      ibeg=mod(kk-1,inc)+1
      iend=npsdta
      do 100 i=ibeg,iend,inc
        if(ncount(i) .gt. 0)go to 100
        jlat=pdata(i,ilat)
        if(pdata(i,ilon) .ge. anlon+1.)pdata(i,ilon)=
     *        pdata(i,ilon)-anlon
        if(pdata(i,ilon) .lt. 1.)pdata(i,ilon)=pdata(i,ilon)+anlon
        jlon=pdata(i,ilon)
        jlat=max(1,min(jlat,nlat))
        if(icount(jlat,jlon) .gt. 0)go to 100
        dy=pdata(i,ilat)-jlat
        dx=pdata(i,ilon)-jlon
        jlonp=jlon+1
        if(jlonp .gt. nlon)jlonp=jlonp-nlon
        if(icount(jlat,jlonp) .gt. 0)go to 100
        jlatp=jlat+1
        jlatp=min(jlatp,nlat)
        if(icount(jlatp,jlon) .gt. 0)go to 100
        if(icount(jlatp,jlonp) .gt. 0)go to 100
        icount(jlat,jlon)=1
        icount(jlatp,jlon)=1
        icount(jlat,jlonp)=1
        icount(jlatp,jlonp)=1
        jl(i128,1)=jlat
        jl(i128,2)=jlatp
        jl(i128,3)=jlon
        jl(i128,4)=jlonp
        i128=i128+1
        ncount(i)=1
        ndcnt=ndcnt+1
        if(jlat .le. nlth)pdata(i,ier)=pdata(i,ier)*factor
        pdata(i,ier)=sqrt(pdata(i,ier))
c-----------------------------------gross error test added here
        obserror=1000./max(pdata(i,ier),1.e-10)
        obserrlm=max(ermin,min(ermax,obserror))
        residual=abs(1000.*pdata(i,ipres))
        ratio=residual/obserrlm
        if(obserror.lt.1.e5) obermax=max(obermax,obserror)
        obermin=min(obermin,obserror)
        resmax=max(resmax,residual)
        ratmax=max(ratmax,ratio)
        if(ratio.gt.gross) then
         numgross=numgross+1
         pdata(i,ier)=0.
        end if
        val=pdata(i,ier)*pdata(i,ipres)
        wgt00=pdata(i,ier)*(1.0-dx)*(1.0-dy)
        wgt10=pdata(i,ier)*(1.0-dx)*dy
        wgt01=pdata(i,ier)*dx*(1.0-dy)
        wgt11=pdata(i,ier)*dx*dy
        numdat=numdat+1
        psfile(is)=jlat
        psfile(is+1)=jlon
        psfile(is+2)=jlatp
        psfile(is+3)=jlonp
        psfile(is+4)=wgt00
        psfile(is+5)=wgt10
        psfile(is+6)=wgt01
        psfile(is+7)=wgt11
        psfile(is+8)=pdata(i,ipres)
        psfile(is+9)=pdata(i,ier)
c       psfile(is+10)=pges(i)
c       psfile(is+11)=ptype(i)
        is=is+npp
        nsuperp=nsuperp+1
        psplty=psplty+val*val
        numw=numw+1
        if(i128 .eq. 129)i128=1
        if(numdat .gt. 128)then
          icount(jl(i128,1),jl(i128,3))=0
          icount(jl(i128,1),jl(i128,4))=0
          icount(jl(i128,2),jl(i128,3))=0
          icount(jl(i128,2),jl(i128,4))=0
        end if
100   continue
101   continue
      psfile(issave)=numdat+.001
      nrecs=nrecs+1
      if(ndcnt .eq. npsdta)go to 201
200   continue
      pw=psplty/float(numw)
201   write(6,955)psplty,numw,pw
955   format(' total psfc obs penalty=',e12.4,i8,e12.4)
      npsrecs=nrecs
       write(6,*)' gross error check for psfcs:'
       write(6,*)'   obs error max,min=',obermax,obermin
       write(6,*)'   for check, obs error bounded by ',ermin,ermax
       write(6,*)'   for check, max ratio residual/ob error =',gross
       write(6,*)'   max residual=',resmax
       write(6,*)'   max ratio=',ratmax
       write(6,*)'   number obs that failed gross test = ',numgross
      return
      end
