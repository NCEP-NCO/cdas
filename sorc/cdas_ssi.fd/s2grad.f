       subroutine s2grad(ds,u,v,jcap,nlon,nlath,qln,rln,trigs,ifax)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    s2grad     div to u,v
c   prgmmr: parrish          org: w/nmc22    date: 94-04-08
c
c abstract: spectral divergence coefs to grid u,v.
c
c program history log:
c   94-04-08  parrish
c
c   input argument list:
c     ds       - divergence coefficients
c     jcap     - triangular truncation
c     nlon     - number of longitudes
c     nlath    - number of gaussian lats in one hemisphere
c     qln      - q(n,l)
c     rln      - r(n,l)
c
c   output argument list:
c     u        - longitude component of winds
c     v        - latitude component of winds
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
         dimension ds((jcap+1)*(jcap+2))
         dimension u(2*nlath+1,nlon+2)
         dimension v(2*nlath+1,nlon+2)
         dimension qln((jcap+1)*(jcap+2),nlath)
         dimension rln((jcap+1)*(jcap+2),nlath)
         dimension trigs(2*nlon),ifax(10)
c--------
c-------- internal scratch dynamic space follows:
c--------
         dimension work(2*(2*nlath+1)*(nlon+2))
         dimension ue(2*(jcap+1)),uo(2*(jcap+1))
         dimension ve(2*(jcap+1)),vo(2*(jcap+1))
c---------------
         v=0.
         do j=1,nlath
          jr=2*nlath+1-j
          ii0=0
          ve=0.
          vo=0.
          do m=0,jcap,2
           do ll=1,2*(jcap+1-m)
            vo(ll)=vo(ll)-rln(ii0+ll,j)*ds(ii0+ll)
           end do
           if(m.lt.jcap) then
            ii0=ii0+2*(jcap+1-m)
            do ll=1,2*(jcap-m)
             ve(ll)=ve(ll)-rln(ii0+ll,j)*ds(ii0+ll)
            end do
            ii0=ii0+2*(jcap-m)
           end if
          end do
c----------
c---------- now combine even and odd parts
c----------
          do ll=1,2*(jcap+1)
           v(j,ll)=ve(ll)+vo(ll)
           v(jr,ll)=ve(ll)-vo(ll)
          end do
         end do
c--------
c-------- finally do fourier sums in longitude
c--------
         lot=nlath*2
         nlax=lot+1
         call rfftmlt(v,work,trigs,ifax,nlax,1,nlon,lot,1)
c---------------
c-------------multiply div by i
         do i=1,(jcap+1)*(jcap+2),2
          divr=ds(i)
          divi=ds(i+1)
          ds(i)=divi
          ds(i+1)=-divr
         end do
         u=0.
         do j=1,nlath
          jr=2*nlath+1-j
          ii0=0
          ue=0.
          uo=0.
          do m=0,jcap,2
           do ll=1,2*(jcap+1-m)
            ue(ll)=ue(ll)+qln(ii0+ll,j)*ds(ii0+ll)
           end do
           if(m.lt.jcap) then
            ii0=ii0+2*(jcap+1-m)
            do ll=1,2*(jcap-m)
             uo(ll)=uo(ll)+qln(ii0+ll,j)*ds(ii0+ll)
            end do
            ii0=ii0+2*(jcap-m)
           end if
          end do
c----------
c---------- now combine even and odd parts
c----------
          do ll=1,2*(jcap+1)
           u(j,ll)=ue(ll)+uo(ll)
           u(jr,ll)=ue(ll)-uo(ll)
          end do
         end do
c--------
c-------- finally do fourier sums in longitude
c--------
         call rfftmlt(u,work,trigs,ifax,nlax,1,nlon,lot,1)
       return
       end
