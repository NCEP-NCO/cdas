       subroutine tg2s0(ts,t,jcap,nlon,nlath,wgts,pln,trigs,ifax)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    transpose of g2s0
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
         dimension wgts(nlath)
         dimension work(2*(2*nlath+1)*(nlon+2))
         dimension te(2*jcap+2),to(2*jcap+2)
         dimension factor(2*jcap+2,nlath)
c--------
         do j=1,nlath
          factor(1,j)=wgts(j)/nlon
          factor(2,j)=0.
          do i=3,2*jcap+2
           factor(i,j)=.5*factor(1,j)
          end do
         end do
         t=0.
         do j=1,nlath
          jr=2*nlath+1-j
          ii0=0
          te=0.
          to=0.
          do m=0,jcap,2
           do ll=1,2*(jcap+1-m)
            te(ll)=te(ll)+pln(ii0+ll,j)*ts(ii0+ll)
           end do
           if(m.lt.jcap) then
            ii0=ii0+2*(jcap+1-m)
            do ll=1,2*(jcap-m)
             to(ll)=to(ll)+pln(ii0+ll,j)*ts(ii0+ll)
            end do
            ii0=ii0+2*(jcap-m)
           end if
          end do
c----------
c---------- now combine even and odd parts
c----------
          do ll=1,2*(jcap+1)
           t(j,ll)=(te(ll)+to(ll))*factor(ll,j)
           t(jr,ll)=(te(ll)-to(ll))*factor(ll,j)
          end do
         end do
c--------
c-------- finally do fourier sums in longitude
c--------
         lot=nlath*2
         nlax=lot+1
         call rfftmlt(t,work,trigs,ifax,nlax,1,nlon,lot,1)
       return
       end
