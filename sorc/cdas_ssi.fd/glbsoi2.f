      subroutine glbsoi(inprep,inges,iianl,jcap,igen,nsig,
     *  nlath,nlon,ineofs,niter,miter,ioanl,a,
     *  isat,jsat,
     *  isfc,iscra,nblk,iscra3,ampdivt,dampdivt,idivt,
     *  on85dt,ntdata,nsdata,nwdata,npdata,nqdata,npwdat,nqtdata,
     *  nsprof,
     *   ermaxt,ermaxw,ermaxp,ermaxq,ermaxpw,
     *   ermint,erminw,erminp,erminq,erminpw,
     *   grosst,grossst,grossw,grossp,grossq,grosspw)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    glbsoi     spectral oi
c   prgmmr: parrish          org: w/nmc22    date: 90-10-06
c
c abstract: executes spectral oi analysis.
c
c program history log:
c   90-10-06  parrish
c   94-02-11  parrish
c
c   input argument list:
c     inprep   - unit number for prepda data file.
c     inges    - unit number for guess spectral coefficients.
c     iianl    - unit number for previous analysis.
c     jcap     - triangular truncation
c     igen     - generating process identifier
c     nsig     - number of sigma levels
c     nlath    - number of gaussian lats in one hemisphere
c     nlon     - number of longitudes
c     ineofs   - input unit for statistics
c     niter    - max number of iterations for conjugate gradient.
c     miter    - number of outer iterations
c     ioanl    - output unit for updated analysis.
c     a        - a(4): scaling factors for forecast error spectra
c     isat     - input file for sat. error covariances
c     jsat     - scratch file used for sat. data
c     isfc     - input surface bges file
c     iscra    - scratch file for infor. from conventional data
c     nblk     - blocking factor for scratch files
c     iscra3   - file to save grid fields needed by tan linear divt
c     ampdivt,dampdivt - parameters for divtend penalty
c     idivt    - unit number for file with divtend error variances
c     ermaxt, etc. - limits to obs errors for 
c     ermint, etc. -  gross error check
c     grosst, etc. - toss limits (scaled by obs error) for gross check
c
c   output argument list:
c     no output arguments
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      character*4 on85dt(8)
      real a(4)
c--------
c-------- scratch space follows
c--------
      dimension rpw(2*nlath+1,nlon+2),sigi(nsig+1)
      dimension xhat((jcap+1)*(jcap+2),nsig,4),xhatp((jcap+1)*(jcap+2))
      dimension del2((jcap+1)*(jcap+2))
      dimension trigs(nlon*2),ifax(10)
      dimension rlats(nlath*2),slat(nlath),clat(nlath)
      dimension wgts(nlath*2),sigl(nsig),pwcon(nsig)
      dimension isatv(nsig)
      dimension rt(2*nlath+1,nlon+2,nsig)
      dimension ru(2*nlath+1,nlon+2,nsig)
      dimension rv(2*nlath+1,nlon+2,nsig)
      dimension rq(2*nlath+1,nlon+2,nsig)
      dimension rp(2*nlath+1,nlon+2)
      dimension rus(2*nlath+1,nlon+2,nsig)
      dimension rvs(2*nlath+1,nlon+2,nsig)
      dimension rts(2*nlath+1,nlon+2,nsig)
      dimension rvorts(2*nlath+1,nlon+2,nsig)
      dimension rplons(2*nlath+1,nlon+2)
      dimension rplats(2*nlath+1,nlon+2)
      dimension rlsg(nsig),aibw(nsig,nsig),w(nsig),tbar(nsig)
      dimension a3(nsig,nsig),dstlast((jcap+1)*(jcap+2),nsig)
      dimension dstb((jcap+1)*(jcap+2),nsig)
      dimension in((jcap+1)*(jcap+2))
      dimension pln((jcap+1)*(jcap+2),nlath)
      dimension qln((jcap+1)*(jcap+2),nlath)
      dimension rln((jcap+1)*(jcap+2),nlath)
      dimension mlad(0:jcap,0:jcap)
      dimension ml2lm((jcap+1)*(jcap+2))
      dimension factslm((jcap+1)*(jcap+2))
      dimension factvlm((jcap+1)*(jcap+2))
      dimension lmad(0:jcap,0:jcap)
      dimension lm2ml((jcap+1)*(jcap+2))
      dimension factsml((jcap+1)*(jcap+2))
      dimension factvml((jcap+1)*(jcap+2))
      dimension qfile(17*nqdata),uvfile(18*nwdata),tfile(17*ntdata),
     *     sfile((4+(28+2)*30)*nsprof),pwfile(12*npwdat),
     *     psfile(11*npdata)
      integer*4 itmp4(4)
c--------
c-------- setup various constants
c--------
c-------- set up index arrays to convert coefs to internal format
c--------
c wne
	xhat = 0.0
	xhatp = 0.0

      ii=-1
      do l=0,jcap
       do m=0,jcap-l
        ii=ii+2
        mlad(m,l)=ii
       end do
      end do
      ii=-1
      do m=0,jcap
       do l=0,jcap-m
        ii=ii+2
        lmad(m,l)=ii
       end do
      end do
      ii=-1
      do m=0,jcap
       do l=0,jcap-m
        ii=ii+2
        ml2lm(ii)=mlad(m,l)
        ml2lm(ii+1)=ml2lm(ii)+1
       end do
      end do
      ii=-1
      do l=0,jcap
       do m=0,jcap-l
        ii=ii+2
        lm2ml(ii)=lmad(m,l)
        lm2ml(ii+1)=lm2ml(ii)+1
       end do
      end do
      ii=-1
      do m=0,jcap
       ii=ii+2
       factslm(ii)=1.
       factslm(ii+1)=0.
       if(m.lt.jcap) then
        do l=1,jcap-m
         ii=ii+2
         factslm(ii)=1.
         factslm(ii+1)=1.
        end do
       end if
      end do
      factvlm=factslm
      factvlm(1)=0.
      ii=-1
      do l=0,jcap
       one=1.
       zero=min(1,l)
       do m=0,jcap-l
         ii=ii+2
        factsml(ii)=one
        factsml(ii+1)=zero
       end do
      end do
      factvml=factsml
      factvml(1)=0.
c------------------pick up hydrostatic matrix from background stats file
       rewind ineofs
	write(*,*) 'glbsoi2: rewind ineofs'

c      read(ineofs)nsigstat,idum,nmdszh,jcapstat,
       read(ineofs) (itmp4(i),i=1,4),
     *     rlsg,aibw,w,rogc,tbar,a3
       nsigstat = itmp4(1)
       idum = itmp4(2)
       nmdszh = itmp4(3)
       jcapstat = itmp4(4)
       close(ineofs)
c---------------------pick up vert dim for sat er covar matrices
       rewind isat
       read(isat,5050)nsigsat
5050   format(1x,i3)
       close(isat)
c--------------------pick up vert and horiz numbers for divt errors
       rewind idivt
c      read(idivt)nsigdivt,jcapdivt
       read(idivt) (itmp4(i),i=1,2)
       nsigdivt = itmp4(1)
       jcapdivt = itmp4(2)

       close(idivt)
      ii=-1
      do m=0,jcap
       do l=0,jcap-m
        ii=ii+2
        in(ii)=m+l
        in(ii+1)=m+l
       end do
      end do
      numcoefs=ii+1
      nc=(jcap+1)*(jcap+2)
      write(6,*)' (jcap+1)*(jcap+2)=',nc,' numcoefs=',numcoefs
c---------------------test various adjoints
c     call tsthoper(ineofs,pwcon,jcap,nsig,nlath,nlon,ap,bp,aqr,
c    *    bqr,gr,del2,wgts,slat,clat,trigs,ifax,pe0,qe0,ro0,lmix,
c    *    lastmix,lpairs,in,rlats,rt,ru,rv,rq,rp,
c    *    nsigstat,nmdszh,jcapstat,a)
c----------------------
c--------
c-------- initialize various transform constants
c-------- read data, do various stuff, output f0 and
c-------- inverse of superob error variances
c--------
      do jiter=1,miter
       inext=inges
       if(jiter.gt.1) inext=ioanl
       call setuprhs(sigl,jiter,rpw,sigi,
     *   inext,iianl,jcap,nsig,
     *   nlath,nlon,pwcon,
     *   ntdata,nsdata,nwdata,npdata,nqdata,npwdat,nqtdata,
     *   ntrecs,nwrecs,nprecs,nqrecs,npwrecs,
     *   isat,nsigsat,jsat,msat,
     *   rlats,del2,pln,qln,rln,wgts,trigs,ifax,in,
     *   isfc,isatv,iscra,nblk,rt,ru,rv,rq,rp,
     *   a3,ampdivt,dampdivt,dstlast,dstb,iscra3,
     *   ermaxt,ermaxw,ermaxp,ermaxq,ermaxpw,
     *   ermint,erminw,erminp,erminq,erminpw,
     *   grosst,grossst,grossw,grossp,grossq,grosspw,
     *   mlad,ml2lm,factslm,factvlm,
     *   lmad,lm2ml,factsml,factvml,
     *   rus,rvs,rts,rvorts,rplons,rplats,
     *   qfile,uvfile,tfile,sfile,pwfile,psfile,nsprof)
c--------
c-------- solve oi equation, add increment to guess coefs and
c-------- write out.
c--------
       call pcgsoi(inprep,ineofs,xhat,xhatp,pwcon,jsat,msat,
     *   niter,miter,jiter,jcap,igen,nsig,nlath,
     *   nlon,del2,wgts,pln,qln,rln,trigs,ifax,in,isatv,
     *   ntdata,nwdata,npdata,nqdata,npwdat,
     *   ntrecs,nwrecs,nprecs,nqrecs,npwrecs,
     *   iscra,nblk,on85dt,ioanl,inext,inges,rlats,a,rt,ru,rv,rpw,rq,rp,
     *   nsigstat,nmdszh,jcapstat,ampdivt,dampdivt,idivt,
     *   nsigdivt,jcapdivt,a3,sigl,sigi,dstlast,dstb,iscra3,rrm0,
     *   mlad,ml2lm,factslm,factvlm,
     *   lmad,lm2ml,factsml,factvml,
     *   rus,rvs,rts,rvorts,rplons,rplats,
     *   qfile,uvfile,tfile,sfile,pwfile,psfile,nsprof)
      end do
      return
      end
