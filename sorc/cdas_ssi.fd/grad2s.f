       subroutine grad2s(ds,u,v,jcap,nlon,nlath,qln,rln,trigs,ifax,
     *            wgts,del2)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    transpose of s2grad  
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
         dimension wgts(2*nlath)
         dimension del2((jcap+1)*(jcap+2))
c--------
c-------- internal scratch dynamic space follows:
c--------
         dimension work(2*(2*nlath+1)*(nlon+2))
         dimension ue(2*(jcap+1)),uo(2*(jcap+1))
         dimension ve(2*(jcap+1)),vo(2*(jcap+1))
c--------
         ds=0.
c--------
c-------- first do fourier analysis in longitude
c--------
         lot=nlath*2
         nlax=lot+1
         call rfftmlt(u,work,trigs,ifax,nlax,1,nlon,lot,-1)
         do j=1,nlath
          jr=2*nlath+1-j
c---------- separate even and odd parts
          do ll=1,2*jcap+2
           ue(ll)=(u(j,ll)+u(jr,ll))*wgts(j)
           uo(ll)=(u(j,ll)-u(jr,ll))*wgts(j)
          end do
          ii0=0
          do m=0,jcap,2
           do ll=1,2*(jcap+1-m)
            ds(ii0+ll)=ds(ii0+ll)+qln(ii0+ll,j)*ue(ll)
           end do
           if(m.lt.jcap) then
            ii0=ii0+2*(jcap+1-m)
            do ll=1,2*(jcap-m)
             ds(ii0+ll)=ds(ii0+ll)+qln(ii0+ll,j)*uo(ll)
            end do
            ii0=ii0+2*(jcap-m)
           end if
          end do
         end do
c---------------
c-------------multiply div by i
         do i=1,(jcap+1)*(jcap+2),2
          divr=ds(i)
          divi=ds(i+1)
          ds(i)=-divi
          ds(i+1)=divr
         end do
c--------
c-------- next v, do fourier sums in longitude
c--------
         call rfftmlt(v,work,trigs,ifax,nlax,1,nlon,lot,-1)
c---------------
         do j=1,nlath
          jr=2*nlath+1-j
c---------- separate even and odd parts
          do ll=1,2*jcap+2
           ve(ll)=(v(j,ll)+v(jr,ll))*wgts(j)
           vo(ll)=(v(j,ll)-v(jr,ll))*wgts(j)
          end do
          ii0=0
          do m=0,jcap,2
           do ll=1,2*(jcap+1-m)
            ds(ii0+ll)=ds(ii0+ll)-rln(ii0+ll,j)*vo(ll)
           end do
           if(m.lt.jcap) then
            ii0=ii0+2*(jcap+1-m)
            do ll=1,2*(jcap-m)
             ds(ii0+ll)=ds(ii0+ll)-rln(ii0+ll,j)*ve(ll)
            end do
            ii0=ii0+2*(jcap-m)
           end if
          end do
         end do
         ds=del2*ds
       return
       end
