      subroutine rdfact(factor,idateg,hourg,nlath,nlon,isfc)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    rdfact  read and compute factor       
c   prgmmr: derber           org: w/nmc23    date: 92-09-08
c
c abstract: read in factor for use in reducing to 10m winds          
c
c program history log:
c   92-09-08  derber 
c
c   input argument list:
c     isfc     - unit number of bges file
c     idateg   - date time array for guess
c     hourg    - hour of guess
c     nlath    - number of latitudes on gaussian grid
c     nlon     - number of longitudes on gaussian grid
c
c   output argument list:
c     factor   - factor for reducing bottom sigma values to 10m
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      real factor(2*nlath+1,nlon+2)
      integer idateg(4)
c--------
c-------- scratch space
c--------
c
      dimension fldr (nlon,2*nlath-2)
      integer*4 idate(4)          
      integer idate5(5)
      character lab85*32
	integer dates(8), dateg(8)
	real rinc(5)
c
c   read surface file to get 10m winds
c
      factor=1.   
      print *, 'calculating factor'
      rewind isfc
      read (isfc,end=7781,err=7781) lab85
      read (isfc,end=7781,err=7781) fhour,idate

c	date from sfc file
	dates(1) = idate(4)
	dates(2) = idate(2)
	dates(3) = idate(3)
	dates(4) = 0
	dates(5) = idate(1)
	dates(6) = 0
	dates(7) = 0
	dates(8) = 0

c	date from guess file
	dateg(1) = idateg(4)
	dateg(2) = idateg(2)
	dateg(3) = idateg(3)
	dateg(4) = 0
	dateg(5) = idateg(1)
	dateg(6) = 0
	dateg(7) = 0
	dateg(8) = 0
	call w3difdat(dateg,dates,3,rinc)
	nmin = rinc(3) + 60*(hourg-fhour)

c     idate5(1)=idateg(4)
c     idate5(2)=idateg(2)
c     idate5(3)=idateg(3)
c     idate5(4)=idateg(1)
c     idate5(5)=0
c     call w3fs21(idate5,nming)
c     nming=nming+60*hourg
c     idate5(1)=idate(4)
c     idate5(2)=idate(2)
c     idate5(3)=idate(3)
c     idate5(4)=idate(1)
c     idate5(5)=0
c     call w3fs21(idate5,nmins)
c     nmins=nmins+60*fhour
      print 101,fhour,idate
  101 format(' fhour=',f5.0,' idate=',4i5)
c     print *,' for bges file, nmins=',nmins
c     print *,' for ges file, nming=',nming
c     if(nmins.ne.nming) go to 7781
	if (nmin.lt.0) goto 7781

c
      do 102 j = 1,13
c
        read(isfc,end=7781,err=7781)
  102 continue
      read(isfc,end=7781,err=7781) fldr
      do 1101 j=1,nlon
      do 1101 i = 1, 2*nlath-2
        factor(i+1,j)=fldr(j,2*nlath-1-i)
 1101 continue
c
      sumn=0.
      sums=0.
      do 780 j=1,nlon
        sumn=factor(2*nlath-1,j) +sumn
        sums=factor(2,j) +sums
 780  continue
      sumn=sumn/nlon
      sums=sums/nlon
      do 781 j=1,nlon
        factor(2*nlath,j)=sumn
        factor(1,j)=sums
 781  continue
      xmax=-1.e20
      xmin=1.e20
      do 1102 j=1,nlon
      do 1102 i = 1, 2*nlath-2
        xmax=max(factor(i,j),xmax)
        xmin=min(factor(i,j),xmin)
 1102 continue
      print *, 'factor  max and min = ',xmax,xmin
7781  continue
      close(isfc)
      return    
      end
