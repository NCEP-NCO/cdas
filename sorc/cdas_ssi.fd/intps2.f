      subroutine intps(ps,nlath,nlon,nprecs,psfile)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    intps       apply observation operator for ps 
c   prgmmr: derber          org: w/nmc23    date: 91-02-26
c
c abstract: apply observation operator for ps observations
c
c program history log:
c   91-02-26  derber
c
c   input argument list:
c     ps       - search direction for ps
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
      dimension sps(2*nlath+1,nlon+2)
c--------
c-------- initialize grids
c--------
      sps=ps
      ps=0.
      if(nprecs .eq. 0)return
      npp=10
      is=1
      do 100 i=1,nprecs
        ngrp=psfile(is)+.001
        is=is+1
ccdir$ ivdep
        do 101 irpt=1,ngrp
        jlat=psfile((irpt-1)*npp+is)
        jlon=psfile((irpt-1)*npp+is+1)
        jlatp=psfile((irpt-1)*npp+is+2)
        jlonp=psfile((irpt-1)*npp+is+3)
        wgt00=psfile((irpt-1)*npp+is+4)
        wgt10=psfile((irpt-1)*npp+is+5)
        wgt01=psfile((irpt-1)*npp+is+6)
        wgt11=psfile((irpt-1)*npp+is+7)
c       pdat=psfile((irpt-1)*npp+is+8)
c       aerr=psfile((irpt-1)*npp+is+9)
c       pges=psfile((irpt-1)*npp+is+10)
c       ptyp=psfile((irpt-1)*npp+is+11)
        val=wgt00*sps(jlat,jlon)+wgt10*sps(jlatp,jlon)
     *     +wgt01*sps(jlat,jlonp)+wgt11*sps(jlatp,jlonp)
        ps(jlat,jlon)=ps(jlat,jlon)+wgt00*val
        ps(jlatp,jlon)=ps(jlatp,jlon)+wgt10*val
        ps(jlat,jlonp)=ps(jlat,jlonp)+wgt01*val
        ps(jlatp,jlonp)=ps(jlatp,jlonp)+wgt11*val
 101  continue
      is=is+npp*ngrp
100   continue
      return
      end
