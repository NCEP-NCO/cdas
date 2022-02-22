       subroutine gtbdivt(idivt,bdivt,nsig,jcap,nsigdivt,
     *            jcapdivt,sigl)
c-------------
c-------------bring in estimates of divtend error variance,
c-----------  interpolate in vertical, and truncate in horizontal
c-----------
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    gtbdivt    read divtend error variances.
c   prgmmr: parrish          org: w/nmc22    date: 94-02-11
c
c abstract: read divtend error variance, interpolate in vert, truncate
c            in horizontal, as necessary.
c
c program history log:
c   94-02-11  parrish
c
c   input argument list:
c     idivt    - input unit number containing divtend error variances
c     nsig     - number of sigma levels
c     jcap     - triangular truncation
c     nsigdivt - number of sigma levels in input divtend errors
c     jcapdivt - triangular truncation for input divtend errors
c     sigl     - analysis sigma levels
c
c   output argument list:
c     bdivt    - inverse of divtend error variances.
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
         dimension bdivt(0:jcap,nsig),sigl(nsig)
         dimension cdivt(0:jcapdivt,nsigdivt),sigldivt(nsigdivt)
         dimension tdivt(0:jcapdivt,nsig)
         dimension rlsg(nsig),rlsgdivt(nsigdivt)
         dimension grid(nsig)
         integer*4 itmp4(2)
c-----------
         rewind idivt
c        read(idivt)msigdivt,mcapdivt,sigldivt,cdivt
         read(idivt) (itmp4(i),i=1,2),sigldivt,cdivt
         msigdivt = itmp4(1)
         mcapdivt = itmp4(2)

         close(idivt)
         rlsgdivt=log(sigldivt)
         rlsg=log(sigl)
         grid=rlsg
         call gdcrdn(grid,nsig,rlsgdivt,nsigdivt)
         do k=1,nsig
          i0=grid(k)
          i0=max(1,min(nsigdivt-1,i0))
          i1=i0+1
          del=grid(k)-i0
          w0=1.-del
          w1=del
          do n=0,jcapdivt
           tdivt(n,k)=w0*cdivt(n,i0)+w1*cdivt(n,i1)
          end do
          do n=0,min(jcap,jcapdivt)
           bdivt(n,k)=tdivt(n,k)
          end do
          if(jcap.gt.jcapdivt) then
           do n=jcapdivt+1,jcap
            bdivt(n,k)=tdivt(jcapdivt,k)
           end do
          end if
          do n=1,jcap
           bdivt(n,k)=1./bdivt(n,k)
          end do
         end do
       return
       end
