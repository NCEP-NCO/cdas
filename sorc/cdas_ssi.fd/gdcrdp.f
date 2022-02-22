      subroutine gdcrdp(d,nd,x,nx)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    gdcrdp      get grid coords for monotonic increasing
c   prgmmr: parrish          org: w/nmc22    date: 90-10-11
c
c abstract: get grid coords for monotonic increasing points.
c
c program history log:
c   90-10-11  parrish
c
c   input argument list:
c     d,nd     - input points, number of input points.
c     x,nx     - values, number of reference grid points.
c
c   output argument list:
c     d        - converted to grid units.
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      dimension d(nd),x(nx)
c--------
      do 400 id=1,nd
        dt=d(id)
        if(dt .le. x(1))then
          d(id)=1.+(dt-x(1))/(x(2)-x(1))
        else
          ix=isrchfge(nx-1,x,1,dt)-1
          d(id)=float(ix)+(dt-x(ix))/(x(ix+1)-x(ix))
        end if
400   continue
      return
      end
