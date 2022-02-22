      subroutine initqpw(rt,nlath,nlon,nsig,nqrecs,npwrecs,
     *     pwcon,qfile,pwfile)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    initqpw       set up initial rhs for q and pw.
c   prgmmr: derber          org: w/nmc23    date: 91-02-26
c
c abstract: set up initial rhs for q and precip. water
c
c program history log:
c   91-02-26  derber
c
c   input argument list:
c     nlath    - half the number of latitudes on gaussian grid
c     nlon     - number of longitudes on gaussian grid
c     nsig     - number of sigma levels
c     nqrecs   - number of q records
c     npwrecs  - number of precip. water records
c     pwcon    - vertical integration precip. water constants
c     nblk     - blocking factor for iunit
c     iunit    - data scratch file
c
c   output argument list:
c     rt       - results from observation operator (0 for no data)
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      dimension pwcon(nsig)
      dimension rt(2*nlath+1,nlon+2,nsig)
      dimension qfile(*),pwfile(*)
c--------
      rt=0.
      if(npwrecs .eq. 0)go to 1000
      npp=11
c--------
c-------- initialize grids
c--------
      is=1
      do 700 i=1,npwrecs
        ngrp=pwfile(is)
        is=is+1
cccdir$ ivdep
        do 701 irpt=1,ngrp
        jlat=pwfile((irpt-1)*npp+is)
        jlon=pwfile((irpt-1)*npp+is+1)
        jlatp=pwfile((irpt-1)*npp+is+2)
        jlonp=pwfile((irpt-1)*npp+is+3)
        wgt00=pwfile((irpt-1)*npp+is+4)
        wgt10=pwfile((irpt-1)*npp+is+5)
        wgt01=pwfile((irpt-1)*npp+is+6)
        wgt11=pwfile((irpt-1)*npp+is+7)
        psfc=pwfile((irpt-1)*npp+is+8)
        val=-pwfile((irpt-1)*npp+is+9)*pwfile((irpt-1)*npp+is+10)
c       aerr=pwfile((irpt-1)*npp+is+11)
c       pwge=pwfile((irpt-1)*npp+is+12)
c       pwty=pwfile((irpt-1)*npp+is+13)
        val=val*psfc
c       val=(val-pdat*aerr)*psfc*psfc
        do 401 k=1,nsig
        rt(jlat,jlon,k)=rt(jlat,jlon,k)+wgt00*val*pwcon(k)
        rt(jlatp,jlon,k)=rt(jlatp,jlon,k)+wgt10*val*pwcon(k)
        rt(jlat,jlonp,k)=rt(jlat,jlonp,k)+wgt01*val*pwcon(k)
        rt(jlatp,jlonp,k)=rt(jlatp,jlonp,k)+wgt11*val*pwcon(k)
 401    continue
 701  continue
      is=is+ngrp*npp
700   continue
1000  if(nqrecs .eq. 0)return
      npp=16
c--------
c
c--------
      is=1
      do 100 i=1,nqrecs
        ngrp=qfile(is)
        is=is+1
ccdir$ ivdep
      do 101 k=1,ngrp
        jlat=qfile((k-1)*npp+is)
        jlon=qfile((k-1)*npp+is+1)
        jsig=qfile((k-1)*npp+is+2)
        jlatp=qfile((k-1)*npp+is+3)
        jlonp=qfile((k-1)*npp+is+4)
        jsigp=qfile((k-1)*npp+is+5)
        wgt000=qfile((k-1)*npp+is+6)
        wgt100=qfile((k-1)*npp+is+7)
        wgt010=qfile((k-1)*npp+is+8)
        wgt110=qfile((k-1)*npp+is+9)
        wgt001=qfile((k-1)*npp+is+10)
        wgt101=qfile((k-1)*npp+is+11)
        wgt011=qfile((k-1)*npp+is+12)
        wgt111=qfile((k-1)*npp+is+13)
        val=-qfile((k-1)*npp+is+14)*qfile((k-1)*npp+is+15)
c       aerr=qfile((k-1)*npp+is+15)
c       qges=qfile((k-1)*npp+is+16)
c       qtyp=qfile((k-1)*npp+is+17)
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
