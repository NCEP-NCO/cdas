      subroutine rdprep(
     .           tdata,sdata,wdata,psdata,qdata,pwdata,
     .           ttype,stype,wtype,pstype,qtype,pwtype,
     .           qmaxerr,pwmerr,iqtflg,
     .           ntdata,nsdata,nwdata,npdata,nqdata,npwdat)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    rdprep     read in and reformat prepda data
c   prgmmr: parrish          org: w/nmc22    date: 90-10-07
c   prgmmr: woollen          org: w/nmc22    date: 93-12-07
c
c abstract: read in and reformat data.
c
c program history log:
c   90-10-07  parrish
c
c usage: call rdprep(inbufr,on85dt,
c    .           tdata,sdata,wdata,psdata,qdata,pwdata,
c    .           ttype,stype,wtype,pstype,qtype,pwtype,
c    .           qmaxerr,pwmerr,nqtdata,iqtflg,
c    .           ntdata,nsdata,nwdata,npdata,nqdata,npwdat)
c   input argument list:
c     inbufr   - unit number for bufr data file.
c     ntdata   - number of temp obs
c     nsdata   - number of satellite obs
c     nwdata   - number of wind obs
c     npdata   - number of surface pressure obs
c     nqdata   - number of moisture obs
c     npwdat   - number of total precipitable water obs
c
c   output argument list:
c     on85dt   - bufr prepda on85 date record
c     tdata    - tdata(ntdata,7)
c     sdata    - sdata(nsdata,5)
c     wdata    - wdata(nwdata,8)
c     psdata   - psdata(npdata,8)
c     qdata    - qdata(nqdata,7)
c     pwdata   - pwdata(npwdat,6)
c     ttype    - temperature types
c     stype    - satellite types
c     wtype    - wind types
c     pstype   - surface pressure types
c     pwtype   - precipitable water types
c     qmaxerr  - maximum error for moisture
c     pwmerr   - maximum error for precipitable water
c     nqtdata  - number of t observations without q
c     iqtflg   - flag for no moisture
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      dimension tdata(ntdata,7),wdata(nwdata,8)
      dimension sdata(nsdata,5)
      dimension psdata(npdata,8),qdata(nqdata,7)
      dimension pwdata(npwdat,6)
      dimension ttype(ntdata),wtype(nwdata)
      dimension stype(nsdata)
      dimension pstype(npdata),qtype(nqdata)
      dimension pwtype(npwdat),iqtflg(ntdata)
      dimension qmaxerr(nqdata),pwmerr(npwdat)
c------
      if(ntdata.gt.0) then
       rewind 81
       ii0=1
       do k=1,1000000
        ii1=min(ntdata,ii0+511)
        read(81)((tdata(i,j),i=ii0,ii1),j=1,7),
     *  (ttype(i),i=ii0,ii1),(iqtflg(i),i=ii0,ii1)
        if(ii1.eq.ntdata) go to 200
        ii0=ii0+512
       end do
200    continue
       close(81)
      end if
      if(nsdata.gt.0) then
       rewind 82
       ii0=1
       do k=1,1000000
        ii1=min(nsdata,ii0+511)
        read(82)((sdata(i,j),i=ii0,ii1),j=1,5),
     *  (stype(i),i=ii0,ii1)
        if(ii1.eq.nsdata) go to 300
        ii0=ii0+512
       end do
300    continue
       close(82)
      end if
      if(nwdata.gt.0) then
       rewind 83
       ii0=1
       do k=1,1000000
        ii1=min(nwdata,ii0+511)
        read(83)((wdata(i,j),i=ii0,ii1),j=1,8),
     *  (wtype(i),i=ii0,ii1)
        if(ii1.eq.nwdata) go to 400
        ii0=ii0+512
       end do
400    continue
       close(83)
      end if
      if(npdata.gt.0) then
       rewind 84
       ii0=1
       do k=1,1000000
        ii1=min(npdata,ii0+511)
        read(84)((psdata(i,j),i=ii0,ii1),j=1,8),
     *  (pstype(i),i=ii0,ii1)
        if(ii1.eq.npdata) go to 500
        ii0=ii0+512
       end do
500    continue
       close(84)
      end if
      if(nqdata.gt.0) then
       rewind 85
       ii0=1
       do k=1,1000000
        ii1=min(nqdata,ii0+511)
        read(85)((qdata(i,j),i=ii0,ii1),j=1,7),
     *  (qtype(i),i=ii0,ii1),(qmaxerr(i),i=ii0,ii1)
        if(ii1.eq.nqdata) go to 600
        ii0=ii0+512
       end do
600    continue
       close(85)
      end if
      if(npwdat.gt.0) then
       rewind 86
       ii0=1
       do k=1,1000000
        ii1=min(npwdat,ii0+511)
        read(86)((pwdata(i,j),i=ii0,ii1),j=1,6),
     *  (pwtype(i),i=ii0,ii1),(pwmerr(i),i=ii0,ii1)
        if(ii1.eq.npwdat) go to 700
        ii0=ii0+512
       end do
700    continue
       close(86)
      end if
      print *,' ntdata=',ntdata
      print *,' nsdata=',nsdata
      print *,' nwdata=',nwdata
      print *,' npdata=',npdata
      print *,' nqdata=',nqdata
      print *,' npwdat=',npwdat
      return
      end
