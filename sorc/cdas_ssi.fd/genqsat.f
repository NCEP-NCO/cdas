      subroutine genqsat(t,qsat,nlath,nlon,nsig,ps,slg)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    genqsat     obtain qsat for given t.
c   prgmmr: parrish          org: w/nmc22    date: 90-10-11
c
c abstract: obtain saturation specific humidity for given temperature.
c
c program history log:
c   90-10-11  parrish
c
c   input argument list:
c     t,ps     - temperature, log(psfc) on gaussian grid
c     nlath    - number of gaussian lats in one hemisphere
c     nlon     - number of longitudes
c     nsig     - number of sigma levels
c     slg      - sigma values at mid-point of model levels
c
c   output argument list:
c     qsat     - saturation specific humidity 
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      dimension t(2*nlath+1,nlon+2,nsig)
      dimension qsat(2*nlath+1,nlon+2,nsig)
      dimension ps(2*nlath+1,nlon+2),slg(nsig)
      real l0
      eps=.622
      cp=1005.
      cl=4187.
      cpv=1876.5
      rv=461.5
      l0=2.501e6
      t0=273.16
      es0=611.0
      fact1 = (cpv - cl) / rv
      fact2 = (l0 + (cl - cpv) * t0) / rv
      fact3 = 1. / t0
      omeps=1.-eps
c--------
      do 200 k=1,nsig
        do 100 i=1,nlon
          do 100 j=1,2*nlath
            pw=exp(ps(j,i))*slg(k)
            if(qsat(j,i,k) .lt. 0.)qsat(j,i,k)=0.
            tv=t(j,i,k)/(1.+0.61*qsat(j,i,k))
            if(pw.lt.5.) pw=5.
            pw=1000.*pw
            es = es0 * (tv / t0) ** fact1 *
     1          exp ( fact2 * (fact3 - 1. / tv))
            qs = eps * es / (pw - omeps * es)
            if(qs .lt. qsat(j,i,k))then
            tv=t(j,i,k)/(1.+0.61*qs)
            es = es0 * (tv / t0) ** fact1 *
     1          exp ( fact2 * (fact3 - 1. / tv))
            qs = eps * es / (pw - omeps * es)
            tv=t(j,i,k)/(1.+0.61*qs)
            es = es0 * (tv / t0) ** fact1 *
     1          exp ( fact2 * (fact3 - 1. / tv))
            qs = eps * es / (pw - omeps * es)
            end if
            qsat(j,i,k)=qs
100       continue
200   continue
      return
      end
