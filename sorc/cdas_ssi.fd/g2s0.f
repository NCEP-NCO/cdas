       subroutine g2s0(ts,t,jcap,nlon,nlath,wgts,pln,trigs,ifax)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    ts2g0       transpose of s2g0
c   prgmmr: parrish          org: w/nmc22    date: 90-09-21
c
c abstract: summation of scalar spherical harmonic series.
c
c program history log:
c   90-09-21  parrish
c
c   input argument list:
c     ts       - spectral coefs
c     jcap     - triangular truncation
c     nlon     - number of longitudes
c     nlath    - number of gaussian lats in one hemisphere
c     pln      - spherical harmonics
c     trigs,ifax - used by fft
c
c   output argument list:
c     t        - values of desired field on gaussian grid
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
         dimension ts((jcap+1)*(jcap+2))
         dimension t(2*nlath+1,nlon+2)
         dimension trigs(nlon*2),ifax(10)
         dimension pln((jcap+1)*(jcap+2),nlath)
         dimension work(2*(2*nlath+1)*(nlon+2))
         dimension wgts(2*nlath),te(2*jcap+2),to(2*jcap+2)
c--------
c-------- first do fourier analysis in longitude
c--------
         lot=nlath*2
         nlax=lot+1

         call rfftmlt(t,work,trigs,ifax,nlax,1,nlon,lot,-1)

         ts=0.
         do j=1,nlath
          jr=2*nlath+1-j
c---------- separate even and odd parts
          do ll=1,2*jcap+2
           te(ll)=(t(j,ll)+t(jr,ll))*wgts(j)
           to(ll)=(t(j,ll)-t(jr,ll))*wgts(j)
          end do
          ii0=0
          do m=0,jcap,2
           do ll=1,2*(jcap+1-m)
            ts(ii0+ll)=ts(ii0+ll)+pln(ii0+ll,j)*te(ll)
           end do
           if(m.lt.jcap) then
            ii0=ii0+2*(jcap+1-m)
            do ll=1,2*(jcap-m)
             ts(ii0+ll)=ts(ii0+ll)+pln(ii0+ll,j)*to(ll)
            end do
            ii0=ii0+2*(jcap-m)
           end if
          end do
         end do
       return
       end
