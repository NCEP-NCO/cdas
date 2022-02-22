      subroutine residw(dpres,du,dv,
     *  rtypes,rspres,mwdat)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    residw      compute wind residuals.
c   prgmmr: parrish          org: w/nmc22    date: 90-10-11
c
c abstract: form wind residuals, get obs error, and print stats.
c
c program history log:
c   90-10-11  parrish
c
c   input argument list:
c     dpres    - pres (mb*10+qm in, grid coords in sigma out)
c     du,dv    - u,v wnd in, residuals out
c     rtypes   - prepda observation types
c     rspres   - observation pressures
c     mwdat    - number of observations
c
c   output argument list:
c     as indicated above
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
c--------
      dimension dpres(mwdat)
      dimension du(mwdat),dv(mwdat)
      dimension rtypes(mwdat)
      dimension rspres(mwdat)
c--------
c--------  limit to be above bottom of model
c--------
      do 49 i=1,mwdat
        if(dpres(i) .lt. 1.) dpres(i)=1.
49    continue
c--------
c-------- now do statistics summary
c--------
      scale=1.
      ptop=800.
      pbot=900.
      call dvast(rtypes,du,dv,scale,mwdat,rspres,pbot,ptop,
     *  'current vfit of 850mb wind data, ranges in m/s$')
      ptop=450.
      pbot=550.
      call dvast(rtypes,du,dv,scale,mwdat,rspres,pbot,ptop,
     *  'current vfit of 500mb wind data, ranges in m/s$')
      ptop=225.
      pbot=275.
      call dvast(rtypes,du,dv,scale,mwdat,rspres,pbot,ptop,
     *  'current vfit of 250mb wind data, ranges in m/s$')
      ptop=75.
      pbot=125.
      call dvast(rtypes,du,dv,scale,mwdat,rspres,pbot,ptop,
     *  'current vfit of 100mb wind data, ranges in m/s$')
      ptop=0.
      pbot=74.
      call dvast(rtypes,du,dv,scale,mwdat,rspres,pbot,ptop,
     *  'current vfit of 50mb wind data, ranges in m/s$')
      ptop=0.
      pbot=2000.
      call dvast(rtypes,du,dv,scale,mwdat,rspres,pbot,ptop,
     *  'current vfit of all wind data, ranges in m/s$')
      return
      end
