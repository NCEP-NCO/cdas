      subroutine prept(drptps,dlons,dlats,dpres,rtypes,rspres,
     *  mtdat,psg,nlat,nlon,nsig,glats,glons,sigl)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    prept       preliminary stuff before res. calc. t
c   prgmmr: parrish          org: w/nmc22    date: 90-10-11
c
c abstract: preliminary stuff before residual calculation for temps.
c
c program history log:
c   90-10-11  parrish
c
c   input argument list:
c     drptps   - obs type in, obs error out (ln(ps) units)
c     dlons,dlats - obs longitudes and latitudes (radians in and out)
c     dpres    - pres (mb*10+qm in, grid coords in sigma out)
c     rtypes   - prepda observation types
c     mtdat    - number of observations
c     psg      - model guess log(psfc), p in cb
c     nlat     - number of gaussian lats pole to pole
c     nlon     - number of longitudes
c     nsig     - number of sigma levels
c     glats,glons - grid latitudes and longitudes
c     glons    - grid longitudes
c     sigl     - sigma layer midpoint values
c
c   output argument list:
c     rspres   - observation pressures
c     and as indicated above
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
c--------
      dimension drptps(mtdat),dlons(mtdat)
      dimension dlats(mtdat),dpres(mtdat)
      dimension psg(nlat+1,nlon+2)
      dimension glats(nlat),glons(nlon),sigl(nsig)
      dimension rtypes(mtdat)
      dimension rspres(mtdat)
c--------
c-------- local space
c--------
      dimension rbpres(mtdat)
      dimension rbt(mtdat)
      dimension rlow(mtdat),rhgh(mtdat)
      dimension sigll(nsig)
c--------
c-------- get log(sig)
c--------
      sigll=log(sigl)
c--------
c-------- convert obs lats and lons to grid coordinates
c--------
      call gdcrdp(dlats,mtdat,glats,nlat)
      call gdcrdp(dlons,mtdat,glons,nlon)
c--------
c-------- 3.  interpolate surface pressure
c--------
c-------- obtain guess surface pressure at obs locations
c--------
      call intrp2(psg,rbpres,dlats,dlons,
     *  nlat,nlon,mtdat)
c--------
c-------- convert obs pressure to sigma, then get grid coordinates
c--------
      rspres=10.*exp(dpres)
      dpres=dpres-rbpres
      call gdcrdn(dpres,mtdat,sigll,nsig)
      do 260 i=1,mtdat
        if(rtypes(i) .gt. 179.5 .and. rtypes(i) .lt. 189.5)then
          if(dpres(i) .gt. 4.)drptps(i)=1.e6
        end if
 260  continue
ccdir$ ivdep
      numlow=0
      numhgh=0
      hgh=0.
      xlow=0.
      do 80 i=1,mtdat
        rlow(i)=dpres(i)-1.
        rlow(i)=min(0.,rlow(i))
        rlow(i)=-rlow(i)
        if(rlow(i).ne.0.) numlow=numlow+1
        xlow=max(rlow(i),xlow)
        rhgh(i)=dpres(i)-nsig-.05
        rhgh(i)=max(0.,rhgh(i))
        if(rhgh(i).ne.0.) numhgh=numhgh+1
        hgh=max(rhgh(i),hgh)
80    continue
      write(6,900)mtdat,numhgh,numlow,hgh,xlow
900   format(' number of temps=',i8,' number extrapolated above',
     *  ' top sigma layer=',i8,/,'  number extrapolated below',
     *  ' bottom sigma layer=',i8,/,' largest extrapolation',
     *  ' above=',f12.2,/,' largest extrapolation below=',f12.2)
      do 90 i=1,mtdat
        if(drptps(i) .le. 0.)then
          print *,i,drptps(i),rtypes(i),dlats(i),dpres(i)
          drptps(i)=1.e9
          end if
        drptps(i)=1./(drptps(i)+
     *     1.e6*rhgh(i)+4.*rlow(i))**2
90    continue
      return
      end
