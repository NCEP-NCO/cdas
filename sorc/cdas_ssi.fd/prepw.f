      subroutine prepw(drptps,dlons,dlats,dpres,
     *  rtypes,rspres,
     *  mwdat,psg,fact,factor,nlat,nlon,nsig,glats,glons,sigl)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    prepw       preliminary stuff before res. calc. w
c   prgmmr: parrish          org: w/nmc22    date: 90-10-11
c
c abstract: preliminary stuff before residual calculation for winds
c
c program history log:
c   90-10-11  parrish
c
c   input argument list:
c     drptps   - obs type in, obs error out (ln(ps) units)
c     dlons,dlats - obs longitudes and latitudes (radians in and out)
c     dpres    - pres (mb*10+qm in, grid coords in sigma out)
c     rtypes   - prepda observation types
c     mwdat    - number of observations
c     psg      - model guess log(psfc), p in cb
c     factor   - array of 10m wind factors
c     nlat     - number of gaussian lats pole to pole
c     nlon     - number of longitudes
c     nsig     - number of sigma levels
c     glats,glons - grid latitudes
c     sigl     - sigma layer midpoint values
c
c   output argument list:
c     rspres   - observation pressures
c     fact     - near surface reduction in wind factor at obs.
c     and as indicated above
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
c--------
      dimension drptps(mwdat),dlons(mwdat),dlats(mwdat),dpres(mwdat)
      dimension psg(nlat+1,nlon+2)
      dimension glats(nlat),glons(nlon),sigl(nsig)
      dimension rtypes(mwdat)
      dimension rspres(mwdat),fact(mwdat)
c-------
c-------- local space
c--------
      dimension rbpres(mwdat)
      dimension rlow(mwdat),rhgh(mwdat)
      dimension sigll(nsig+1)
c--------
c-------- get log(sig)
c--------
      sigll(1)=0.
      do 100 k=1,nsig
      sigll(k+1)=log(sigl(k))
 100  continue
c--------
c-------- convert obs lats and lons to grid coordinates
c--------
      call gdcrdp(dlats,mwdat,glats,nlat)
      call gdcrdp(dlons,mwdat,glons,nlon)
c-------
c-------- 3.  interpolate surface pressure
c--------
c-------- obtain guess surface pressure at obs locations
c--------
      call intrp2(psg,rbpres,dlats,dlons,nlat,nlon,mwdat)
      call intrp2(factor,fact,dlats,dlons,nlat,nlon,mwdat)
c--------
c-------- convert obs pressure to sigma, then get grid coordinates
c--------
      rspres=10.*exp(dpres)
      dpres=dpres-rbpres
c--------
c-------- for ssmi wind speeds, set vert pos to 10m
c--------
      alog20=alog(.9976)
      alog10=alog(.9988)
      do 45 i=1,mwdat
        if(nint(rtypes(i)).eq.280) dpres(i)=alog20
        if(nint(rtypes(i)).eq.281) dpres(i)=alog10
        if(nint(rtypes(i)).eq.282) dpres(i)=alog20
        if(nint(rtypes(i)).eq.283) dpres(i)=alog20
        if(nint(rtypes(i)).eq.284) dpres(i)=alog10
        if(nint(rtypes(i)).eq.285) dpres(i)=alog10
        if(nint(rtypes(i)).eq.286) dpres(i)=alog10
45    continue
      do 46 i=1,mwdat
        if(dpres(i) .lt. alog10 .and. dpres(i) .gt. sigll(2))
     *    fact(i)=(dpres(i)-alog10+fact(i)*(sigll(2)-
     *     dpres(i)))/
     *     (sigll(2)-alog10)
        if(dpres(i) .lt. sigll(2))fact(i)=1.
 46   continue
      call gdcrdn(dpres,mwdat,sigll,nsig+1)
      numhgh=0
      numlow=0
      hgh=-1.e9
      xlow=-1.e9
      rsig=nsig
      do 58 i=1,mwdat
        rlow(i)=min(dpres(i),0.)
        dpres(i)=dpres(i)-1.
        rhgh(i)=max(dpres(i)-.001-rsig,0.)
        rhgh(i)=abs(rhgh(i))
        rlow(i)=abs(rlow(i))
        if(rhgh(i).ne.0.) numhgh=numhgh+1
        if(rlow(i).ne.0.) numlow=numlow+1
        hgh=max(rhgh(i),hgh)
        xlow=max(rlow(i),xlow)
58    continue
      write(6,900)mwdat,numhgh,numlow,hgh,xlow
900   format(' number of winds=',i8,' number extrapolated above',
     *   ' top sigma layer=',i8,/,'  number extrapolated below',
     *   ' bottom sigma layer=',i8,/,' largest extrapolation',
     *   ' above=',f12.2,/,' largest extrapolation below=',f12.2)
ccdir$ ivdep
      do 908 i=1,mwdat
        drptps(i)=1./(drptps(i)
     *    +1.e6*rhgh(i)+4.*rlow(i))**2
908   continue
      return
      end
