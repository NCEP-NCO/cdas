      subroutine initps(ps,nlath,nlon,nprecs,psfile)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    initps       set up initial rhs for surface press.
c   prgmmr: derber          org: w/nmc23    date: 91-02-26
c
c abstract: set up initial rhs for surface pressure observations
c
c program history log:
c   91-02-26  derber
c
c   input argument list:
c     nlath    - half the number of latitudes on gaussian grid
c     nlon     - number of longitudes on gaussian grid
c     nprecs   - number of ps records
c     nblk     - blocking factor for iunit
c     iunit    - data scratch file
c
c   output argument list:
c     ps       - results from observation operator (0 for no data)
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      dimension ps(2*nlath+1,nlon+2)
      dimension psfile(*)
c--------
      ps=0.
      if(nprecs .eq. 0)return
      npp=10
c--------
c-------- initialize grids
c--------
      is=1
      do 100 i=1,nprecs
        ngrp=psfile(is)+.001
        is=is+1
ccdir$ ivdep
        do 101 k=1,ngrp
        jlat=psfile((k-1)*npp+is)
        jlon=psfile((k-1)*npp+is+1)
        jlatp=psfile((k-1)*npp+is+2)
        jlonp=psfile((k-1)*npp+is+3)
        wgt00=psfile((k-1)*npp+is+4)
        wgt10=psfile((k-1)*npp+is+5)
        wgt01=psfile((k-1)*npp+is+6)
        wgt11=psfile((k-1)*npp+is+7)
        val=-psfile((k-1)*npp+is+8)*psfile((k-1)*npp+is+9)
c       aerr=psfile((irpt-1)*npp+11)
c       pges=psfile((irpt-1)*npp+12)
c       ptyp=psfile((irpt-1)*npp+13)
        ps(jlat,jlon)=ps(jlat,jlon)+wgt00*val
        ps(jlatp,jlon)=ps(jlatp,jlon)+wgt10*val
        ps(jlat,jlonp)=ps(jlat,jlonp)+wgt01*val
        ps(jlatp,jlonp)=ps(jlatp,jlonp)+wgt11*val
 101  continue
      is=is+ngrp*npp
100   continue
      return
      end
