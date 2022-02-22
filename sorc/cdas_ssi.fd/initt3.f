      subroutine initt(rr,ntrecs,nlath,nlon,nsig,tfile)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    initt       set up initial rhs for temps.
c   prgmmr: derber          org: w/nmc23    date: 91-02-26
c
c abstract: set up initial rhs for temps.
c
c program history log:
c   91-02-26  derber
c
c   input argument list:
c     nlath    - half the number of latitudes on gaussian grid
c     nlon     - number of longitudes on gaussian grid
c     nsig     - number of sigma levels
c     ntrecs   - number of temp records
c     nblk     - blocking factor for iunit
c     iunit    - data scratch file
c
c   output argument list:
c     rr       - results from observation operator (0 for no data)
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      dimension rr(2*nlath+1,nlon+2,nsig)
      dimension tfile(*)
c--------
      if(ntrecs .eq. 0)return
      npp=16
c--------
c
c--------
      rr=0.
      is=1
      do 100 i=1,ntrecs
        ngrp=tfile(is)
        is=is+1
ccdir$ ivdep
      do 101 k=1,ngrp
        jlat=tfile((k-1)*npp+is)
        jlon=tfile((k-1)*npp+is+1)
        jsig=tfile((k-1)*npp+is+2)
        jlatp=tfile((k-1)*npp+is+3)
        jlonp=tfile((k-1)*npp+is+4)
        jsigp=tfile((k-1)*npp+is+5)
        wgt000=tfile((k-1)*npp+is+6)
        wgt100=tfile((k-1)*npp+is+7)
        wgt010=tfile((k-1)*npp+is+8)
        wgt110=tfile((k-1)*npp+is+9)
        wgt001=tfile((k-1)*npp+is+10)
        wgt101=tfile((k-1)*npp+is+11)
        wgt011=tfile((k-1)*npp+is+12)
        wgt111=tfile((k-1)*npp+is+13)
        val=-tfile((k-1)*npp+is+14)*tfile((k-1)*npp+is+15)
c       aerr=aeofs((k-1)*npp+17)
c       tges=aeofs((k-1)*npp+18)
c       ttyp=aeofs((k-1)*npp+19)
        rr(jlat,jlon,jsig)=rr(jlat,jlon,jsig)+wgt000*val
        rr(jlatp,jlon,jsig)=rr(jlatp,jlon,jsig)+wgt100*val
        rr(jlat,jlonp,jsig)=rr(jlat,jlonp,jsig)+wgt010*val
        rr(jlatp,jlonp,jsig)=rr(jlatp,jlonp,jsig)+wgt110*val
        rr(jlat,jlon,jsigp)=rr(jlat,jlon,jsigp)+wgt001*val
        rr(jlatp,jlon,jsigp)=rr(jlatp,jlon,jsigp)+wgt101*val
        rr(jlat,jlonp,jsigp)=rr(jlat,jlonp,jsigp)+wgt011*val
        rr(jlatp,jlonp,jsigp)=rr(jlatp,jlonp,jsigp)+wgt111*val
 101   continue
       is=is+ngrp*npp
100   continue
      return
      end
