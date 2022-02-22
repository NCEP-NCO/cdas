      subroutine ressat(dpres,dt,rtypes,rspres,mtdat)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    ressat      print stats for sat. temperatures
c   prgmmr: parrish          org: w/nmc22    date: 90-10-11
c
c abstract: print stats for sat. temperatures.
c
c program history log:
c   90-10-11  parrish
c
c   input argument list:
c     dpres    - pres (mb*10+qm in, grid coords in sigma out)
c     dt       - temps in, residual out
c     rtypes   - prepda observation types
c     pspres   - observation pressures
c     mtdat    - number of observations
c
c   output argument list:
c     none              
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
c--------
      dimension dpres(mtdat),dt(mtdat)
      dimension rtypes(mtdat)
      dimension rspres(mtdat)
c--------
c--------
c-------- now do statistics summary
c--------
      scale=1.
      ptop=800.
      pbot=900.
      call dtast(rtypes,dt,scale,mtdat,rspres,pbot,ptop,
     *  'current fit of 850mb sat. temperature data, ranges in k$')
      ptop=450.
      pbot=550.
      call dtast(rtypes,dt,scale,mtdat,rspres,pbot,ptop,
     *  'current fit of 500mb sat. temperature data, ranges in k$')
      ptop=225.
      pbot=275.
      call dtast(rtypes,dt,scale,mtdat,rspres,pbot,ptop,
     *  'current fit of 250mb sat. temperature data, ranges in k$')
      ptop=75.
      pbot=125.
      call dtast(rtypes,dt,scale,mtdat,rspres,pbot,ptop,
     *  'current fit of 100mb sat. temperature data, ranges in k$')
      ptop=0.
      pbot=74.9
      call dtast(rtypes,dt,scale,mtdat,rspres,pbot,ptop,
     *  'current fit of  50mb sat. temperature data, ranges in k$')
      ptop=0.
      pbot=2000.
      call dtast(rtypes,dt,scale,mtdat,rspres,pbot,ptop,
     *  'current fit of all sat. temperature data, ranges in k$')
      return
      end
