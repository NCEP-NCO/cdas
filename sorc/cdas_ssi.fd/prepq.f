      subroutine prepq(drptps,dlons,dlats,dpres,
     *  rtypes,rspres,mqdat,rmaxerr,rbqs,temp,
     *  psg,nlat,nlon,nsig,glats,glons,sigl)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    prepq       preliminary work before calc. q residuals
c   prgmmr: parrish          org: w/nmc22    date: 90-10-11
c
c abstract: preliminary work before calc. q residuals        
c
c program history log:
c   90-10-11  parrish
c
c   input argument list:
c     drptps   - obs type in, obs error out (ln(ps) units)
c     dlons,dlats - obs longitudes and latitudes (radians in and out)
c     dpres    - pres (mb*10+qm in, grid coords in sigma out)
c     rtypes   - prepda observation types
c     rspres   - observation pressures
c     mqdat    - number of observations
c     rmaxerr  - maximum allowed error
c     temp     - 6 hr forecast temperature array
c     psg      - model guess log(psfc), p in cb
c     nlat     - number of gaussian lats pole to pole
c     nlon     - number of longitudes
c     nsig     - number of sigma levels
c     glats,glons - grid latitudes and longitudes
c     sigl     - sigma layer midpoint values
c
c   output argument list:
c     rbqs     - saturation specific humidities at obs. locations
c     as indicated above
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
c--------
      dimension drptps(mqdat),dlons(mqdat)
      dimension dlats(mqdat),dpres(mqdat)
      dimension psg(nlat+1,nlon+2)
      dimension glats(nlat),glons(nlon),sigl(nsig)
      dimension rtypes(mqdat)
      dimension rspres(mqdat)
      dimension rmaxerr(mqdat)
c--------
      dimension temp(nlat+1,nlon+2,nsig)
      dimension rbqs(mqdat)
c--------
c-------- local space
c--------
      dimension qscal(nlat+1,nlon+2,nsig)
      dimension rbpres(mqdat)
      dimension sigll(nsig)
      dimension rlow(mqdat),rhgh(mqdat)
      dimension rbq(mqdat)
c--------
c-------- get log(sig)
c--------
      sigll=log(sigl)
c--------
c-------- convert obs lats and lons to grid coordinates
c--------
      call gdcrdp(dlats,mqdat,glats,nlat)
      call gdcrdp(dlons,mqdat,glons,nlon)
c--------
c-------- 3.  interpolate surface pressure
c--------
c-------- obtain guess surface pressure at obs locations
c--------
      call intrp2(psg,rbpres,dlats,dlons,nlat,nlon,mqdat)
c--------
c-------- convert obs pressure to sigma, then get grid coordinates
c--------
      rspres=10.*exp(dpres)
      dpres=dpres-rbpres
c--------
      call gdcrdn(dpres,mqdat,sigll,nsig)
      do 777 l=1,mqdat
        if(nint(rtypes(l)) .eq. 180) dpres(l)=1.000
        if(nint(rtypes(l)) .eq. 181) dpres(l)=1.000
        if(nint(rtypes(l)) .eq. 182) dpres(l)=1.000
        if(nint(rtypes(l)) .eq. 183) dpres(l)=1.000
        if(nint(rtypes(l)) .eq. 184) dpres(l)=1.000
        if(nint(rtypes(l)) .eq. 185) dpres(l)=1.000
 777  continue
c--------
c--------     interpolate guess q to obs locations and get residual
c--------      (also interpolate guess qsat to obs locations)
c--------
c
c   calculate saturation specific humidity
c
      qscal=0.
      call genqsat(temp,qscal,nlat/2,nlon,nsig,psg,sigl)
      call intrp3(qscal,rbqs,dlats,dlons,dpres,
     *  nlat,nlon,nsig,mqdat)
      do 49 i=1,mqdat
c----------
c---------- scale errors by guess qsat (but keep > .1 g/kg)
c----------
        rmaxerr(i)=rmaxerr(i)*rbqs(i)
        drptps(i)=drptps(i)*rbqs(i)
        rmaxerr(i)=max(.0002,rmaxerr(i))
        drptps(i)=max(.0001,drptps(i))
c--------
c-------- now we adjust the observation error to reflect
c-------- the size of the residual and if extrapolation occured, then
c-------- further adjust error according to amount of extrapolation.
c--------
49    continue
      numhgh=0
      numlow=0
      hgh=-1.e9
      xlow=-1.e9
      rsig=nsig
      do 58 i=1,mqdat
        rlow(i)=min(dpres(i)-1.,0.)
        rhgh(i)=max(dpres(i)-.001-rsig,0.)
        rhgh(i)=abs(rhgh(i))
        rlow(i)=abs(rlow(i))
        if(rhgh(i).ne.0.) numhgh=numhgh+1
        if(rlow(i).ne.0.) numlow=numlow+1
        hgh=max(rhgh(i),hgh)
        xlow=max(rlow(i),xlow)
58    continue
      write(6,900)mqdat,numhgh,numlow,hgh,xlow
900   format(' number of qs=',i8,' number extrapolated above',
     *   ' top sigma layer=',i8,/,'  number extrapolated below',
     *   ' bottom sigma layer=',i8,/,' largest extrapolation',
     *   ' above=',f12.2,/,' largest extrapolation below=',f12.2)
ccdir$ ivdep
      do 908 i=1,mqdat
        drptps(i)=1./(drptps(i)
     *    +1.e6*rhgh(i)+4.*rlow(i))**2
908   continue
      return
      end
