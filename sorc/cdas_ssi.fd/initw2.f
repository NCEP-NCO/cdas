      subroutine initw(ru,rv,nlath,nlon,nsig,nwrecs,uvfile)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    initw       setup initial rhs for winds         
c   prgmmr: derber          org: w/nmc23    date: 91-02-26
c
c abstract: setup initial rhs for winds
c
c program history log:
c   91-02-26  derber
c
c   input argument list:
c     nlath    - half the number of latitudes on gaussian grid
c     nlon     - number of longitudes on gaussian grid
c     nsig     - number of sigma levels
c     nwrecs   - number of wind records
c     nblk     - blocking factor for iunit
c     iunit    - data scratch file
c
c   output argument list:
c     ru       - results from observation operator (0 for no data)
c     rv       - results from observation operator (0for no data)
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      dimension ru(2*nlath+1,nlon+2,nsig)
      dimension rv(2*nlath+1,nlon+2,nsig)
      dimension uvfile(*)
c--------
      ru=0.
      rv=0.
      if(nwrecs .eq. 0)return
      npp=17
c--------
c
c--------
      is=1
      do 100 i=1,nwrecs
        ngrp=uvfile(is)
        is=is+1
ccdir$ ivdep
      do 101 k=1,ngrp
        jlat=uvfile((k-1)*npp+is)
        jlon=uvfile((k-1)*npp+is+1)
        jsig=uvfile((k-1)*npp+is+2)
        jlatp=uvfile((k-1)*npp+is+3)
        jlonp=uvfile((k-1)*npp+is+4)
        jsigp=uvfile((k-1)*npp+is+5)
        wgt000=uvfile((k-1)*npp+is+6)
        wgt100=uvfile((k-1)*npp+is+7)
        wgt010=uvfile((k-1)*npp+is+8)
        wgt110=uvfile((k-1)*npp+is+9)
        aerr=uvfile((k-1)*npp+is+16)
        valu=-uvfile((k-1)*npp+is+14)*aerr
        valv=-uvfile((k-1)*npp+is+15)*aerr
c       uges=uvfile((k-1)*npp+is+19)
c       vges=uvfile((k-1)*npp+is+20)
c       ityp=uvfile((k-1)*npp+is+21)
c       if(ityp .ne. 283)then
        wgt001=uvfile((k-1)*npp+is+10)
        wgt101=uvfile((k-1)*npp+is+11)
        wgt011=uvfile((k-1)*npp+is+12)
        wgt111=uvfile((k-1)*npp+is+13)

        ru(jlat,jlon,jsig)=ru(jlat,jlon,jsig)+wgt000*valu
        ru(jlatp,jlon,jsig)=ru(jlatp,jlon,jsig)+wgt100*valu
        ru(jlat,jlonp,jsig)=ru(jlat,jlonp,jsig)+wgt010*valu
        ru(jlatp,jlonp,jsig)=ru(jlatp,jlonp,jsig)+wgt110*valu
        ru(jlat,jlon,jsigp)=ru(jlat,jlon,jsigp)+wgt001*valu
        ru(jlatp,jlon,jsigp)=ru(jlatp,jlon,jsigp)+wgt101*valu
        ru(jlat,jlonp,jsigp)=ru(jlat,jlonp,jsigp)+wgt011*valu
        ru(jlatp,jlonp,jsigp)=ru(jlatp,jlonp,jsigp)+wgt111*valu
        rv(jlat,jlon,jsig)=rv(jlat,jlon,jsig)+wgt000*valv
        rv(jlatp,jlon,jsig)=rv(jlatp,jlon,jsig)+wgt100*valv
        rv(jlat,jlonp,jsig)=rv(jlat,jlonp,jsig)+wgt010*valv
        rv(jlatp,jlonp,jsig)=rv(jlatp,jlonp,jsig)+wgt110*valv
        rv(jlat,jlon,jsigp)=rv(jlat,jlon,jsigp)+wgt001*valv
        rv(jlatp,jlon,jsigp)=rv(jlatp,jlon,jsigp)+wgt101*valv
        rv(jlat,jlonp,jsigp)=rv(jlat,jlonp,jsigp)+wgt011*valv
        rv(jlatp,jlonp,jsigp)=rv(jlatp,jlonp,jsigp)+wgt111*valv
c       else
c       valu=wgt000*su(jlat,jlon,jsig)+wgt100*su(jlatp,jlon,jsig)
c    *   +wgt010*su(jlat,jlonp,jsig)+wgt110*su(jlatp,jlonp,jsig)
c       valv=wgt000*sv(jlat,jlon,jsig)+wgt100*sv(jlatp,jlon,jsig)
c    *   +wgt010*sv(jlat,jlonp,jsig)+wgt110*sv(jlatp,jlonp,jsig)
c       uanl=uges*aerr+valu
c       vanl=vges*aerr+valv
c       spdanl=sqrt(uanl*uanl+vanl*vanl)
c       spdn=(spdanl-aerr*udat)/spdanl
c       valu=aerr*uanl*spdn
c       valv=aerr*vanl*spdn
c       ru(jlat,jlon,jsig)=ru(jlat,jlon,jsig)+wgt000*valu
c       ru(jlatp,jlon,jsig)=ru(jlatp,jlon,jsig)+wgt100*valu
c       ru(jlat,jlonp,jsig)=ru(jlat,jlonp,jsig)+wgt010*valu
c       ru(jlatp,jlonp,jsig)=ru(jlatp,jlonp,jsig)+wgt110*valu
c       rv(jlat,jlon,jsig)=rv(jlat,jlon,jsig)+wgt000*valv
c       rv(jlatp,jlon,jsig)=rv(jlatp,jlon,jsig)+wgt100*valv
c       rv(jlat,jlonp,jsig)=rv(jlat,jlonp,jsig)+wgt010*valv
c       rv(jlatp,jlonp,jsig)=rv(jlatp,jlonp,jsig)+wgt110*valv
c       end if 
 101   continue
       is=is+ngrp*npp
100   continue
      return
      end
