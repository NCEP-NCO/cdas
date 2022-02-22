      subroutine respw(drptpw,dpw,rmaxerr,ttypes,mpwdat)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    respw      check for gross errors and print stats
c   prgmmr: derber          org: w/nmc23    date: 91-02-25
c
c abstract: check p.w. for gross errors and print stats.
c
c program history log:
c   91-03-25  derber
c
c   input argument list:
c     drptpw   - obs type in, obs error out (mm units)
c     dpw      - p.w. (mm; full value in,  residual out)
c     rmaxerr  - maximum error
c     ttypes   - prepda observation types
c     mpwdat   - number of observations
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
      dimension drptpw(mpwdat)
      dimension dpw(mpwdat)
      dimension ttypes(mpwdat)
      dimension rmaxerr(mpwdat)
c--------
c-------- local arrays
c--------
      dimension rspres(mpwdat)
c--------
c-------- check for gross errors in the precipitable water
c--------
      ngross=0
      grsmlt=3.
      do 118 i=1,mpwdat
        if(abs(dpw(i)).gt.grsmlt*rmaxerr(i))then
          drptpw(i)=0.0
          ngross=ngross+1
        end if
118   continue
      if(ngross .gt. 0)write(6,950)grsmlt,ngross
950   format(' grsmlt=',f7.1,' number of bad p.w. obs=',i8)
c--------
c-------- now do statistics summary
c--------
      scale=1
      ptop=0.
      pbot=2000.
      rspres=1.
      call dtast(ttypes,dpw,scale,mpwdat,rspres,pbot,ptop,
     *  'current fit of precip. water data, ranges in mm$')
      return
      end
