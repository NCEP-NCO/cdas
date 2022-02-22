      subroutine sprs(tdata,ntdta,nlath,nlon,nsig,
     *  msat,sfile,nsigsat,isat,blat,elat,gross,sigl)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    sprs        form superobs for interactive satems.
c   prgmmr: derber           org: w/nmc23    date: 92-07-21
c
c abstract: form superobs for interactive satems.
c
c program history log:
c   92-07-21  derber 
c
c   input argument list:
c     tdata    - obs info at obs locations
c     ntdta    - number of obs
c     nlath    - number of latitudes pole to eq. on gaussian grid
c     nlon     - number of longitudes on gaussian grid
c     nsig     - number of layers on gaussian grid
c     isat     - unit number input sat. statistics
c     nsigsat  - number of sigma levels for input sat. stats
c     blat     - beginning latitude reduction of weight region
c     elat     - ending latitude reduction of weight region
c     gross    - parameter for gross error testing
c     sigl     - sigma layer midpoint values
c
c   output argument list:
c     msat     - number of satellite profiles
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      dimension tdata(ntdta,6)
c--------
c-------- local space
c-------
      dimension rr(nsig),saterr(nsig,nsig,2)
      dimension saterrin(nsigsat,nsigsat,2)
c     dimension scrat(3*nsig)
      dimension iscrat(3*nsig)
      dimension numtemps(nsig),rr2(200),vloc(200),alsig(nsig)
      dimension indl(nsig)
      dimension el(nsig*nsig)
      dimension aerr(nsig*nsig)
      dimension ist(2*nlath+1,nlon+2)
      real rrs(2*nlath+1,nlon+2,nsig)
      real satf(2*nlath+1,nlon+2,nsig)
      logical odiag
      dimension obserror(nsig,2)
      dimension sigl(nsig),siglin(nsigsat)
      dimension iold(nsig)
      dimension sfile(*)
C--------

      rewind isat
      read(isat,50)msigsat
50    format(1x,i3)

      read(isat,60)siglin
60    format(1x,5e15.7)
      do kk=1,2
       do k=1,nsigsat
        read(isat,60)(saterrin(i,k,kk),i=1,nsigsat)
       end do
      end do
      close (isat)
c-------------find old sigmas closest to new sigmas
         do k=1,nsig
          lmin=0.
          distmin=1.e50
          do l=1,nsigsat
           dist=abs(sigl(k)-siglin(l))
           if(dist.lt.distmin) then
            distmin=dist
            lmin=l
           end if
          end do
          iold(k)=lmin
         end do
         write(6,*)' read satcov and get in current sigma coordinate'
         write(6,*)' iold=',iold
         do loop=1,2
          do k=1,nsig
           do l=1,nsig
            saterr(k,l,loop)=saterrin(iold(k),iold(l),loop)
           end do
          end do
         end do
c-------
      obermax=-1.e50
      obermin=1.e50
      resmax=-1.e50
      ratmax=-1.e50
      numgross=0
c-------
      satf=0.
      ist=0
      nlat=2*nlath
      do 183 l=1,nsig
 183  alsig(l)=float(l)
      rrs=0.
      lllcnt=0
      iacnt=1
      ipcnt=0
      aerr=0.
      indl=0
      ilon=1
      ilat=2
      isig=3
      itres=4
      itime=5
      iltype=6
c
c  96/3/25 - patch to transition vtpr data encodeda as type 170 to 171
c
	  do i=1,ntdta 
      if (tdata(i,iltype) .eq. 170) tdata(i,iltype) = 171
	  enddo
      numtemps=0
      nrec=0
      if(ntdta .eq. 0)go to 440
      anlon=float(nlon)
      nnsat=0
      do l=1,nsig
       obserror(l,1)=sqrt(saterr(l,l,1))
       obserror(l,2)=sqrt(saterr(l,l,2))
       write(6,75321)l,obserror(l,1),obserror(l,2)
75321  format(' sat errors for l=',i3,' are ', 2f10.2)
      end do
      do 160 lllll=161,169
      do 160 llll=1,2
      lll=(llll-1)*10+lllll
      ic=0
      itype=lll-160
      if(itype .gt. 10) itype=itype-10
      if(itype .lt. 5)isflag=1
      if(itype .gt. 5) itype=itype-5
      if(itype .eq. 1 .or. itype .eq. 2) ind=1
      if(itype .eq. 3 )ind=2
      do 100 i=1,ntdta
        if(ic .ge. ntdta)go to 100
        ic=ic+1
        rttype=tdata(ic,iltype)
        istype=nint(rttype)
        if(istype .ne. lll)go to 100
c-----------------------------------gross error test added here
        obserrlm=obserror(min(nsig,max(1,nint(tdata(ic,isig)))),ind)
        residual=abs(tdata(ic,itres))
        ratio=residual/obserrlm
        obermax=max(obermax,obserrlm)
        obermin=min(obermin,obserrlm)
        resmax=max(resmax,residual)
        ratmax=max(ratmax,ratio)
        if(ratio.gt.gross) then
         numgross=numgross+1
         go to 100
        end if
        stlat=nint(tdata(ic,ilat))
        stlon=nint(tdata(ic,ilon))
        nlevsv=1
        rr2(1)=tdata(ic,itres)
        vloc(1)=tdata(ic,isig)
        if(stlon .ge. anlon+1.)stlon=stlon-anlon
        if(stlon .lt. 1. )stlon=stlon+anlon
        jlat=stlat
        jlon=stlon
        jlat=max(1,min(jlat,nlat))
        if(jlon .gt. nlon)jlon=jlon-nlon
        if(ist(jlat,jlon) .ne. 0 .and. ist(jlat,jlon) .lt. ind)
     *     go to 100
        jlatp=jlat+1
        jlonp=jlon+1
        jlatp=min(jlatp,nlat)
        do 422 llm=1,199
          icp=ic+1
          if(icp .gt. ntdta)go to 423
          if(tdata(icp,ilat) .ne. tdata(ic,ilat) .or.
     *      tdata(icp,ilon) .ne. tdata(ic,ilon)) go to 423
          itypep=nint(tdata(icp,iltype))
          if(itypep .ne. istype) go to 423
          if(tdata(icp,isig) .lt. tdata(ic,isig))go to 423
          ic=icp
          nlevsv=nlevsv+1
          rr2(nlevsv)=tdata(ic,itres)
          vloc(nlevsv)=tdata(ic,isig)
 422    continue
 423    continue
        nlevs=0
        if(nlevsv .eq. 1) then
          rr(1)=rr2(1)
          indl(1)=nint(vloc(1))
          numtemps(indl(1))=numtemps(indl(1))+1
          nlevs=1
          go to 130
        end if
        lesig=vloc(nlevsv)
        lesig=min(lesig,nsig)
        lesig=max(1,lesig)
        lbsig=ifix(vloc(1))+1
        lbsig=min(lbsig,nsig)
        lbsig=max(1,lbsig)
        ibval=1
        diffl=vloc(1)-float(lbsig-1)
        if(diffl .lt. .02 .and.
     *     lbsig .ge. 2) then
          nlevs=nlevs+1
          numtemps(lbsig-1)=numtemps(lbsig-1)+1
          indl(nlevs)=lbsig-1
          rr(nlevs)=rr2(1)
        end if
        if(lbsig .gt. lesig) go to 140
        do 129 ll=lbsig,lesig
          do 131 l=ibval+1,nlevsv
            if(vloc(l) .gt. float(ll)) go to 132
131       continue
          print *,' interpolation error ', vloc,lbsig,lesig
132       ibval=l-1
          nlevs=nlevs+1
          numtemps(ll)=numtemps(ll)+1
          indl(nlevs)=ll
          rr(nlevs)=((vloc(ibval+1)-alsig(ll))*rr2(ibval)+
     *       (alsig(ll)-vloc(ibval))*rr2(ibval+1))/
     *       (vloc(ibval+1)-vloc(ibval))
129     continue
140     diffl=lesig+1-vloc(nlevsv)
        if(diffl .gt. 0. .and. diffl .lt. .02 .and.
     *      lesig .le. nsig-1) then
          nlevs=nlevs+1
          numtemps(lesig+1)=numtemps(lesig+1)+1
          indl(nlevs)=lesig+1
          rr(nlevs)=rr2(nlevsv)
        end if
        if(nlevs .gt. 0)then
        ist(jlat,jlon)=ind
        do 192 l=1,nlevs
        satf(jlat,jlon,indl(l))=satf(jlat,jlon,indl(l))+1.
        rrs(jlat,jlon,indl(l))=rrs(jlat,jlon,indl(l))+rr(l)
192     continue
        else
        print *,' number of levels equal to zero ',jlat,jlon
        end if
130     continue
100   continue
160   continue
        do 161 i=1,2*nlath
        do 161 j=1,nlon
        if(ist(i,j) .ne. 0)then
        l=0
        do 162 k=1,nsig
        if(satf(i,j,k) .gt. 0.)then
        l=l+1
        indl(l)=k
        rr(l)=rrs(i,j,k)/satf(i,j,k)
        end if
 162    continue
        if(l .eq. 0)go to 333
        stlat=i
        wscale=1.
        if(stlat .ge. elat)wscale=2.
        if(stlat .gt. blat .and. stlat .lt. elat)then
          wscale=(2.*(stlat-blat)+elat-stlat)/(elat-blat)
        end if
        nlevs=l
        ind=ist(i,j)
        do 101 l=1,nlevs
        do 101 m=1,nlevs
          aerr(l+(m-1)*nlevs)=saterr(indl(l),indl(m),ind)*wscale
101     continue
c       call minv(aerr,nlevs,nlevs,scrat,det,1e-12,0,1)
        call iminv(aerr,nlevs,det,iscrat(1),iscrat(nlevs+1))

        if(det .le. 1.e-12) print *,' det error',ic,det

        call chlml(aerr,el,nlevs,nlevs,nlevs,odiag)

        if(odiag)then
          print *,i,j,nlevs,(aerr(m),m=1,nlevs*nlevs)
          stop
        end if
        ipcnt=ipcnt+1
        do n=1,nsig+nsig*nsig+3
        sfile(n+iacnt)=0.
        end do
c       sfile(1+iacnt)=nlevs+.01
        sfile(1+iacnt)=i+.01
        sfile(2+iacnt)=j+.01
        sfile(3+iacnt)=indl(1)+.01
        ioff=indl(1)
        iacnt=iacnt+3
        nxig=nsig-ioff+1
        do 45 n=1,nlevs
45      indl(n)=indl(n)-ioff+1
        do 44 n=1,nlevs
44      sfile(indl(n)+iacnt)=rr(n)
        iacnt=iacnt+nsig
        do 444 nn=1,nlevs
        do 444 n=1,nlevs
444     sfile((indl(nn)-1)*nxig+indl(n)+iacnt)=el((nn-1)*nlevs+n)
        iacnt=iacnt+nsig*nsig
        nnsat=nnsat+1
 333    continue
      end if
161   continue
      print *,' iacnt ',iacnt
      sfile(1)=ipcnt
      nrec=nrec+1
      lsat=0
      do 350 k=1,nsig
        lsat=lsat+numtemps(k)
        write(6,340)numtemps(k),k
340     format(' there are ',i9,' satem obs at level ',i4)
350   continue
      print *,' total number of satem sprobs=',lsat
      print *, ' number of satellite profiles =', nnsat
440   msat=nrec
      print *,' msat= ',msat
       write(6,*)' gross error check for sattemps:'
       write(6,*)'   obs error max,min=',obermax,obermin
       write(6,*)'   for check, max ratio residual/ob error =',gross
       write(6,*)'   max residual=',resmax
       write(6,*)'   max ratio=',ratmax
       write(6,*)'   number obs that failed gross test = ',numgross
      return
      end
