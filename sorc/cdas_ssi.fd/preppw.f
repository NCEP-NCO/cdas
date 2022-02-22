      subroutine preppw(drptpw,dlons,dlats,dpw,rmaxerr,
     *  ttypes,npwdat,nsig,nlat,nlon,glats,
     *  glons)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    preppw     preliminary stuff before res. calc. pw
c   prgmmr: derber          org: w/nmc23    date: 91-02-25
c
c abstract: preliminary stuff before residual calculation for prec. water
c
c program history log:
c   91-03-25  derber
c
c   input argument list:
c     drptpw   - obs type in, obs error out (mm units)
c     dlons,dlats - obs longitudes and latitudes (radians in and out)
c     dpw      - p.w. (mm; full value in,  residual out)
c     npwdat   - number of observations
c     nsig     - number of sigma levels
c     nlat     - number of gaussian lats pole to pole
c     nlon     - number of longitudes
c     glats,glons - grid latitudes and longitudes
c
c   output argument list:
c     rmaxerr  - maximum allowed error
c     ttypes   - prepda observation types
c     and as indicated above
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
c--------
      dimension drptpw(npwdat),dlons(npwdat)
      dimension dlats(npwdat),dpw(npwdat)
      dimension glats(nlat),glons(nlon)
      dimension ttypes(npwdat)
      dimension rmaxerr(npwdat)
c-------
c-------- convert obs lats and lons to grid coordinates
c--------
      call gdcrdp(dlats,npwdat,glats,nlat)
      call gdcrdp(dlons,npwdat,glons,nlon)
      do 1 i=1,npwdat
      drptpw(i)=1./drptpw(i)**2
   1  continue
      return
      end
