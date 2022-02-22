      subroutine prepp(drptps,dlons,dlats,dps,topogd,td,
     *  ttypes,
     *  mpsdat,tg,psg,topogm,nlat,nlon,
     *  nsig,glats,glons,sigl)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    prepp      preliminary stuff before res. calc. p
c   prgmmr: derber         org: w/nmc23    date: 90-10-10
c
c abstract: preliminary stuff before residual calculation for sfc. pres.
c
c program history log:
c   90-10-10  parrish
c
c   input argument list:
c     drptps   - obs type in, obs error out (ln(ps) units)
c     dlons,dlats - obs longitudes, latitudes (radians in and out)
c     dps      - pres (mb*10+qm in, ln(ps) residual out--p in cb)
c     topogd   - obs elevation (m) in and out
c     ttypes   - prepda observation types
c     mpsdat   - number of observations
c     tg       - model guess temperature
c     psg      - model guess log(psfc), p in cb
c     topogm   - model terrain
c     nlat     - number of gaussian lats pole to pole
c     nlon     - number of longitudes
c     nsig     - number of sigma levels
c     glats,glons - grid latitudes and longitudes
c     sigl     - sigma layer midpoint values
c
c   output argument list:
c     and as indicated above
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
c--------
      dimension drptps(mpsdat),dlons(mpsdat)
      dimension dlats(mpsdat),dps(mpsdat),td(mpsdat)
      dimension topogd(mpsdat),tg(nlat+1,nlon+2,nsig)
      dimension psg(nlat+1,nlon+2)
      dimension topogm(nlat+1,nlon+2)
      dimension glats(nlat),glons(nlon),sigl(nsig)
      dimension ttypes(mpsdat)
c--------
c-------- local arrays
c--------
      dimension rbtr(mpsdat)
      dimension rdelz(mpsdat),rbpres(mpsdat),rbprs2(mpsdat)
      dimension rbtps(mpsdat),rbtps2(mpsdat),rdp(mpsdat)
      dimension sigll(nsig)
c--------
c-------- get log(sig)
c--------
      sigll=log(sigl)
c--------
c-------- convert obs lats and lons to grid coordinates
c--------
      call gdcrdp(dlats,mpsdat,glats,nlat)
      call gdcrdp(dlons,mpsdat,glons,nlon)
c--------
c-------- 3.  interpolate surface pressure
c--------
      call intrp2(topogm,rbtr,dlats,dlons,nlat,nlon,mpsdat)
      ngrd=nlat*nlon
ccdir$ ivdep
      do 54 i=1,mpsdat
        rdelz(i)=topogd(i)-rbtr(i)
        drptps(i)=1./(drptps(i)+.000005*abs(rdelz(i)))**2
54    continue
c--------
c-------- obtain guess surface pressure at obs locations
c--------
      call intrp2(psg,rbpres,dlats,dlons,nlat,nlon,mpsdat)
c
c   find midpoint of extrapolation layer in ln(sigma) units, then
c   convert to grid coordinates
c
      rbprs2=dps-rbpres
      rbpres=0.
      call gdcrdn(rbpres,mpsdat,sigll,nsig)
      call gdcrdn(rbprs2,mpsdat,sigll,nsig)
c--------
c--------     interpolate temps
c--------
      call intrp3(tg,rbtps,dlats,dlons,rbpres,
     *  nlat,nlon,nsig,mpsdat)
      call intrp3(tg,rbtps2,dlats,dlons,rbprs2,
     *  nlat,nlon,nsig,mpsdat)
      do 78 i=1,mpsdat
        if(td(i).gt.150..and.td(i).lt.350.) then
          rbtps(i)=.5*(rbtps(i)+td(i))
        else
          rbtps(i)=.5*(rbtps(i)+rbtps2(i))
        end if
78    continue
c
c   extrapolate surface temperature below ground at 6.5 k/km
c   note only extrapolating .5dz, if no surface temperature available.
c
ccdir$ ivdep
      do 88 i=1,mpsdat
        if((td(i).lt.150..or.td(i).gt.350.).and.
     *    rdelz(i).lt.0.) rbtps(i)=
     *    rbtps(i)-.00325*rdelz(i)
 88   continue
      gorm=9.8076/287.16
      rdp=gorm*rdelz/rbtps
      ip10=10
      dps=dps+rdp
      return
      end
