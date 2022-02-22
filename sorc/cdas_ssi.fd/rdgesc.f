      subroutine rdgesc(zc,dc,tc,qc,pc,rc,hourg,idateg,sigi,sigl,
     *  inges,jcap,nsig,on85,ml2lm,factslm,factvlm)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    rdgesc     read sigma coefs and reorder.
c   prgmmr: parrish          org: w/nmc22    date: 90-10-10
c
c abstract: read guess sigma coefs, and reorder to internal format.
c
c program history log:
c   90-10-10  parrish
c
c   input argument list:
c     inges    - unit number of guess coefs
c     jcap     - triangular truncation
c     nsig     - number of sigma levels
c
c   output argument list:
c     zc,dc,tc,qc,pc,rc - ges sig coefs of vort,div,t,q,ln(ps),z0
c     hourg    - guess forecast hour
c     idateg   - initial date of guess
c     sigi     - sigma values at interface of each sigma layer
c     sigl     - sigma values at mid-point of each sigma layer
c     on85     - on85 date record for guess coefs
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      dimension zc((jcap+1)*(jcap+2),nsig)
      dimension dc((jcap+1)*(jcap+2),nsig)
      dimension tc((jcap+1)*(jcap+2),nsig)
      dimension qc((jcap+1)*(jcap+2),nsig)
      dimension pc((jcap+1)*(jcap+2))
      dimension rc((jcap+1)*(jcap+2))
      dimension idateg(4), sigi(nsig+1),sigl(nsig)
      character*4 on85(8)
      integer*4 idate4(4)
      dimension ml2lm((jcap+1)*(jcap+2))
      dimension factslm((jcap+1)*(jcap+2))
      dimension factvlm((jcap+1)*(jcap+2))
c--------
c-------- local space
c--------
      dimension z((jcap+1)*(jcap+2))
c-------
      nc=(jcap+1)*(jcap+2)
      rewind inges
c-------- hour,idate, etc.
      read(inges)on85
c     read(inges)hourg,idateg,sigi,sigl
      read(inges)hourg,idate4,sigi,sigl
      idateg = idate4
c-------- terrain coefs
      read(inges)z
      do i=1,nc
       rc(i)=factslm(i)*z(ml2lm(i))
      end do
c-------- sfcp coefficients
      read(inges)z
      do i=1,nc
       pc(i)=factslm(i)*z(ml2lm(i))
      end do
c-------- temp coefficients
      do k=1,nsig
        read(inges)z
        do i=1,nc
         tc(i,k)=factslm(i)*z(ml2lm(i))
        end do
      end do
c-------- div and vort
      do k=1,nsig
        read(inges)z
        do i=1,nc
         dc(i,k)=factvlm(i)*z(ml2lm(i))
        end do
        read(inges)z
        do i=1,nc
         zc(i,k)=factvlm(i)*z(ml2lm(i))
        end do
      end do
c-------- q coefs
      do k=1,nsig
        read(inges)z
        do i=1,nc
         qc(i,k)=factslm(i)*z(ml2lm(i))
        end do
      end do
      write(6,700)jcap,nsig,hourg,idateg
700   format(' guess sigma coefficients read in, jcap,nsig=',
     *  2i6,/,' hour,idate=',f10.1,4i4)
      close (inges)
      return
      end
