      subroutine resq(drptps,dpres,dq,
     *  rtypes,rspres,mqdat,rmaxerr,rbqs)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    resq        compute moisture residuals.
c   prgmmr: parrish          org: w/nmc22    date: 90-10-11
c
c abstract: form temp residuals, get obs error, and print stats.
c
c program history log:
c   90-10-11  parrish
c
c   input argument list:
c     drptps   - obs type in, obs error out (ln(ps) units)
c     dpres    - pres (mb*10+qm in, grid coords in sigma out)
c     dq       - wets in, residual out
c     rtypes   - prepda observation types
c     rspres   - observation pressures
c     mqdat    - number of observations
c     rmaxerr  - maximum allowed errors
c     rbqs     - saturation specific humidity
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
      dimension drptps(mqdat)
      dimension dpres(mqdat)
      dimension dq(mqdat)
      dimension rtypes(mqdat)
      dimension rspres(mqdat)
      dimension rmaxerr(mqdat)
      dimension rbqs(mqdat)
c
c   calculate saturation specific humidity
c
	write(*,*) '>>resq'
      grsmlt=5.
      ngross=0
      do 49 i=1,mqdat
c--------
c-------- check for gross errors
c--------
        if(dpres(i) .lt. 1.) dpres(i)=1.
        if(abs(dq(i)).gt.grsmlt*rmaxerr(i)) then
c       write(6,*)' rmaxerr of q= ',rmaxerr(i)
        drptps(i)=0.
        ngross=ngross+1
        end if
49    continue
      write(6,901)grsmlt,ngross
901   format(' grsmlt=',f7.1,' num bad qs=',i8)
c--------
c-------- now do statistics summary
c--------  (scale residuals by guess qsat)
c--------
      do 2048 i=1,mqdat
        rbqs(i)=dq(i)*100./rbqs(i)
2048  continue
      scale=1.
      pbot=2000.
      ptop=0.
      call dtast(rtypes,rbqs,scale,mqdat,rspres,pbot,ptop,
     *  'current fit of q data, units in per-cent of guess q-sat$')
	write(*,*) '<<resq'
      return
      end
