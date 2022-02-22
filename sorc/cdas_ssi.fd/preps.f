      subroutine preps(dlons,dlats,dpres,rspres,
     *  msdat,psg,nlat,nlon,nsig,glats,glons,sigl)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    preps       preliminary stuff before res. calc. t
c   prgmmr: parrish          org: w/nmc22    date: 90-10-11
c
c abstract: preliminary stuff before residual calculation for sat temps.
c
c program history log:
c   90-10-11  parrish
c
c   input argument list:
c     dlons,dlats - obs longitudes and latitudes (radians in and out)
c     dpres    - pres (mb*10+qm in, grid coords in sigma out)
c     mtdat    - number of observations
c     psg      - model guess log(psfc), p in cb
c     nlat     - number of gaussian lats pole to pole
c     nlon     - number of longitudes
c     nsig     - number of sigma levels
c     glats,glons - grid latitudes and longitudes
c     sigl     - sigma layer midpoint values
c
c   output argument list:
c     dlons,dlats - obs longitudinal and latitudinal grid location 
c     rspres   - observation pressures
c     and as indicated above
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
c--------
      dimension dlons(msdat)
      dimension dlats(msdat),dpres(msdat)
      dimension psg(nlat+1,nlon+2)
      dimension glats(nlat),glons(nlon),sigl(nsig)
      dimension rspres(msdat)
c--------
c-------- local space
c--------
      dimension rbpres(msdat)
      dimension sigll(nsig)
c--------
c-------- get log(sig)
c--------
      sigll=log(sigl)
c--------
c-------- convert obs lats and lons to grid coordinates
c--------
      call gdcrdp(dlats,msdat,glats,nlat)
      call gdcrdp(dlons,msdat,glons,nlon)
c--------
c-------- 3.  interpolate surface pressure
c--------
c-------- obtain guess surface pressure at obs locations
c--------
      call intrp2(psg,rbpres,dlats,dlons,
     *  nlat,nlon,msdat)
c--------
c-------- convert obs pressure to sigma, then get grid coordinates
c--------
      rspres=10.*exp(dpres)
      dpres=dpres-rbpres
      call gdcrdn(dpres,msdat,sigll,nsig)
      return
      end
