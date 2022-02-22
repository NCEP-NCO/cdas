      subroutine m1glat(khalf,colrad,wgt,wgtcs,rcs2)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    m1glat      compute gaussian lats and weights
c   prgmmr: sela             org: w/nmc22    date: 79-03-03
c
c abstract: compute gaussian latitudes and weights. see p887, 25.4.29,
c   handbook of math functions, abramowitz and stegun, for details.
c
c program history log:
c   79-03-03  sela
c   88-04-08  parrish   add docblock
c
c   input argument list:
c     khalf    - number of gaussian latitudes to compute (pole to
c              - equator)
c
c   output argument list:
c     colrad   - gaussian colatitudes in radians (full precision)
c     wgt      - integration weights (full precision)
c     wgtcs    - wgt/(cos(lat))**2
c     rcs2     - 1/(cos(lat))**2
c
c attributes:
c   language: cft77
c   machine:  cray
c
c$$$
      real colrad(1),wgt(1),wgtcs(1),rcs2(1)
      eps=1.e-12
      si=1.e0
      k2=2*khalf
      rk2=k2
      scale=2.  /(rk2*rk2)
      k1=k2-1
      pi=atan(si)*4.e0
      dradz=pi/360.e0
      rad=0.e0
      do 1000 k=1,khalf
        iter=0
        drad=dradz
1       call m1poly(k2,rad,p2)
2     p1 =p2
      iter=iter+1
      rad=rad+drad
      call m1poly(k2,rad,p2)
      if(sign(si,p1).eq.sign(si,p2))go to 2
      if(drad.lt.eps)go to 3
      rad=rad-drad
      drad=drad*0.25e0
      go to 1
3     continue
      colrad(k)=rad
      phi=rad   *180.e0/pi
      call m1poly(k1,rad,p1)
      x=  cos(rad)
      w=scale*(1.e0-x*x)/(p1*p1)
      wgt(k)=    w
      sn=  sin(rad)
      w=w/(sn*sn)
      wgtcs(k)=    w
      rc=1.e0/(sn*sn)
      rcs2(k)=   rc
      call m1poly(k2,rad,p1)
      prphi =    phi
      prcol =    colrad(k)
1000  continue
      return
      end
