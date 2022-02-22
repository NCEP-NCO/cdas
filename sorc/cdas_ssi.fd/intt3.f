      subroutine intt(rt,ntrecs,nlath,nlon,nsig,tfile)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    intt       apply observation operator for temps.
c   prgmmr: derber          org: w/nmc23    date: 91-02-26
c
c abstract: apply observation operator for temperatures.
c
c program history log:
c   91-02-26  derber
c
c   input argument list:
c     rt       - search direction for temps
c     nlath    - half the number of latitudes on gaussian grid
c     nlon     - number of longitudes on gaussian grid
c     nsig     - number of sigma levels
c     ntrecs   - number of temp records
c     nblk     - blocking factor for iunit
c     iunit    - data scratch file
c
c   output argument list:
c     rr       - results from observation operator (no change for no data)
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      dimension rt(2*nlath+1,nlon+2,nsig)
      dimension rr(2*nlath+1,nlon+2,nsig)
      dimension tfile(*)
c--------
      if(ntrecs .eq. 0)return
      rr=rt
      rt=0.
      npp=16
c--------
c
c--------
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
c       tdat=aeofs((k-1)*npp+16)
c       aerr=aeofs((k-1)*npp+17)
c       tges=aeofs((k-1)*npp+18)
c       ttyp=aeofs((k-1)*npp+19)
        val=wgt000*rr(jlat,jlon,jsig)+wgt100*rr(jlatp,jlon,jsig)
     *   +wgt010*rr(jlat,jlonp,jsig)+wgt110*rr(jlatp,jlonp,jsig)
     *   +wgt001*rr(jlat,jlon,jsigp)+wgt101*rr(jlatp,jlon,jsigp)
     *   +wgt011*rr(jlat,jlonp,jsigp)+wgt111*rr(jlatp,jlonp,jsigp)
        rt(jlat,jlon,jsig)=rt(jlat,jlon,jsig)+wgt000*val
        rt(jlatp,jlon,jsig)=rt(jlatp,jlon,jsig)+wgt100*val
        rt(jlat,jlonp,jsig)=rt(jlat,jlonp,jsig)+wgt010*val
        rt(jlatp,jlonp,jsig)=rt(jlatp,jlonp,jsig)+wgt110*val
        rt(jlat,jlon,jsigp)=rt(jlat,jlon,jsigp)+wgt001*val
        rt(jlatp,jlon,jsigp)=rt(jlatp,jlon,jsigp)+wgt101*val
        rt(jlat,jlonp,jsigp)=rt(jlat,jlonp,jsigp)+wgt011*val
        rt(jlatp,jlonp,jsigp)=rt(jlatp,jlonp,jsigp)+wgt111*val
 101   continue
      is=is+npp*ngrp
100   continue
      return
      end
