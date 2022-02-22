      program ssi
c$$$  main program documentation block
c                .      .    .                                       .
c main program:  ssi    spectral statistical interpolation
c   prgmmr: parrish          org: w/nmc22     date: 92-09-16
c
c abstract: the spectral statistical interpolation analysis routine
c   performs a global analysis for the global spectral model.  this
c   analysis is performed directly in spectral coefficients and in
c   sigma coordinates.  further details of the analysis system can
c   be found in parrish and derber (1992) " the national
c   meteorological center's spectral statistical interpolation
c   analysis system" mon. wea. rev.
c
c program history log:
c   91-xx-xx  parrish/derber
c   91-12-10  parrish/derber   fixed coding error in near sfc anal
c   92-09-14  derber           improved version of global analysis
c
c **** note: the following rules are mandatory for cray programs ****
c *                                                                 *
c *          (1) use unit numbers 11-49 for all input files.        *
c *          (2) use unit numbers 51-89 for all output files.       *
c *          (3) use unit numbers 90-99 for work files that are     *
c *              internal to the program and used only in the       *
c *              program unit.                                      *
c *          (4) unit numbers 1-4, 8-10 and 50 are reserved for     *
c *              future use.                                        *
c *                                                                 *
c ****  except for work files, a unit number should not be used     *
c *     for both input and output.                                  *
c *******************************************************************
c
c usage:
c   input files:
c **************************
c    unit numbers are specified for most files on
c    input data cards. can be changed by changing data cards
c **************************
c     inprep   - input bufr data file from quality control routine
c     inges    - six hour forecast guess field
c     idivt    - divergence tendency error variance estimates
c     ineofs   - input statistics file
c     isfc     - bges file
c     isat     - satellite error covariance file
c     iianl    - previous analysis file          
c
c   output files:  (including scratch files)
c     ioanl    - output analysis file in sigma coordinates
c     jsat     - work file satellite data
c     iscra    - work file conventional data
c     iscra3   - work file--save ref fields for div-tend tan lin model
c     fort.6   - printout
c
c   subprograms called:
c     unique:    - subroutines:
c                - chlml,converc,dtast,dvast,g2s0,g2s1,gdcrdn,
c                - gdcrdp,genqsat,getlalo,getlaloh,glbsoi,gtbhalf,
c                - hoper,htoper,inguess,initps,initqpw,initsat,initt,initw,
c                - intps,intqpw,intrp2,intrp3,intt,intw,m1fax,m1ffta,m1fftb,
c                - m1ftrg,m1glat,m1ipqr,m1poly,m1rcons,m1vpas,m2fftm,pcgsoi,
c                - prepp,preppw,prepq,preps,prept,prepw,prnon85,rdfact,rdgesc,
c                - rdprep,rdtest,residw,respsf,respw,resq,ressat,restmp,s2g0,
c                - s2g1,s2mg2x,satc,satcov,satop4,setuprhs,sprp,sprqpw,sprs,
c                - sprt,spruv,tg2s0.f,tranpw,w3fa03,wranlc             
c
c                - function conmc
c     library:
c       w3lib    -
c
c   exit states:
c     cond =   0 - successful run
c          =  55 - incompatable eof file
c          =  56 - trouble in conmc
c
c remarks: resolution, unit numbers and several constants are
c          in the input data cards
c
c attributes:
c   language: fortran 8x (extensive use of dynamic memory feature)
c   machine:  cray
c
      dimension a(4)
      character*4 on85dt(8)
c--------
      namelist/namanal/a,inprep,inges,iianl,jcap,igen,
     *   nsig,nlath,nlon,isfc,
     *   niter,ioanl,ineofs,
     *   isat,jsat,iscra,miter,iscra3,ampdivt,dampdivt,
     *   ermaxt,ermaxw,ermaxp,ermaxq,ermaxpw,
     *   ermint,erminw,erminp,erminq,erminpw,
     *   grosst,grossst,grossw,grossp,grossq,grosspw
      data a/4*0.2737/,inprep/30/
      data inges/35/,iianl/36/,igen/80/,jcap/126/,nsig/28/
      data isfc/37/
      data nlath/96/,nlon/384/,nprcp/72960/,niter/100/
      data ioanl/51/,ineofs/49/,nblk/40/
      data isat/48/,jsat/94/,iscra/97/,miter/2/
      data iscra3/98/,ampdivt/1./,dampdivt/1./,idivt/47/
      data ermaxt/5.6/,ermaxw/6.1/,ermaxp/3./,ermaxq/100./
      data ermint/1.3/,erminw/1.4/,erminp/1./,erminq/10./
      data ermaxpw/8./,erminpw/2./
      data grosst/20./,grossst/20./,grossw/20./
      data grossp/20./,grossq/20./,grosspw/20./
c--------
c     call w3log('$92290.75','analyf  ')
      call w3tagb('analyf  ',0092,0290,0075,'nmc23 ')
c--------
c--------
c--------  read remaining input parameters
c--------
      read(5,namanal)
      write(6,200)
200   format(' calling glbsoi with following input parameters:',//)
      write(6,namanal)
c--------
       call rdtest(inprep,on85dt,ntdata,nsdata,nwdata,
     *  npdata,nqdata,npwdat,nqtdata,nsprof)
	write(*,*) '>>glbsoi'
      call glbsoi(inprep,inges,iianl,jcap,igen,nsig,
     *   nlath,nlon,ineofs,niter,miter,ioanl,a,isat,jsat,
     *   isfc,iscra,nblk,iscra3,ampdivt,dampdivt,idivt,
     *   on85dt,ntdata,nsdata,nwdata,npdata,nqdata,npwdat,nqtdata,
     *   nsprof,
     *   ermaxt,ermaxw,ermaxp,ermaxq,ermaxpw,
     *   ermint,erminw,erminp,erminq,erminpw,
     *   grosst,grossst,grossw,grossp,grossq,grosspw)
c     call w3log('$e',' ')
      call w3tage('analyf  ')
      stop
      end
