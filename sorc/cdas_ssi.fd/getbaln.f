       subroutine getbaln(baln,jcap)
c--------
c-------- obtain constants for spectral application of linear
c-------- balance operator--- del**(-2) del dot f del del**(-2)
c--------
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    getbaln    get consts for spectral lin-bal operator.
c   prgmmr: parrish          org: w/nmc22    date: 94-02-11
c
c abstract: get consts for spectral linear-balance operator.
c
c program history log:
c   94-02-11  parrish
c
c   input argument list:
c     jcap     - triangular truncation
c
c   output argument list:
c     baln     - balance operator constants.
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
         dimension baln((jcap+1)*(jcap+2))
c--------
         rerth=conmc('rerth$')
         abc=-2.*conmc('omega$')*rerth**2/conmc('g$')
         baln(1:2*(jcap+1))=0.
         ii=2*(jcap+1)-1
         do m=1,jcap
          ii=ii+2
          rn=m
          eps=sqrt(rn**2/(4.*rn**2-1.))
          baln(ii)=abc*eps/(rn*rn)
          baln(ii+1)=0.
          if(m.lt.jcap) then
           do l=1,jcap-m
            ii=ii+2
            rl=l
            rn=m+l
            eps=sqrt((rn**2-rl**2)/(4.*rn**2-1.))
            baln(ii)=abc*eps/(rn*rn)
            baln(ii+1)=baln(ii)
           end do
          end if
         end do
         baln(2*(jcap+1)+1)=0.
       return
       end
