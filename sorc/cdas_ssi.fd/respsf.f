      subroutine respsf(dps,ttypes,mpsdat)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    respsf      print surface stats.
c   prgmmr: parrish          org: w/nmc22    date: 90-10-10
c
c abstract: print surface stats.
c
c program history log:
c   90-10-10  parrish
c
c   input argument list:
c     dps      - pres (mb*10+qm in, ln(ps) residual out--p in cb)
c     ttypes   - prepda observation types
c     mpsdat   - number of observations
c
c   output argument list:
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
c--------
      dimension dps(mpsdat)
      dimension ttypes(mpsdat)
      dimension pp(mpsdat)
c--------
c-------- now do statistics summary
c--------
      scale=1000.
      pp=1000.
      ptop=0.
      pbot=2000.
      call dtast(ttypes,dps,scale,mpsdat,pp,pbot,ptop,
     *  'current fit of surface pressure data, ranges in mb$')
      return
      end
