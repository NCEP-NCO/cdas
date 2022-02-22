      subroutine intrp2(f,g,dx,dy,nx,ny,n)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    intrp2      linear interpolation in two dimensions.
c   prgmmr: parrish          org: w/nmc22    date: 90-10-11
c
c abstract: linear interpolate in 2 dims (2nd dim always periodic).
c
c program history log:
c   90-10-11  parrish
c
c   input argument list:
c     f        - input interpolator
c     dx,dy    - input x,y -coords of interpolation points (grid units)
c     nx,ny    - x,y-dimensions of interpolator grid
c     ny       - ditto y
c     n        - number of interpolatees
c
c   output argument list:
c     g        - output interpolatees
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
c--------
      dimension f(nx+1,ny+2),g(n),dx(n),dy(n)
c--------
      do 100 i=1,n
        ix=dx(i)
        iy=dy(i)
        ix=max(1,min(ix,nx))
        ixp=ix+1
        ixp=min(ixp,nx)
        delx=dx(i)-ix
        dely=dy(i)-iy
        if(iy.lt.1) iy=iy+ny
        if(iy.gt.ny) iy=iy-ny
        iyp=iy+1
        if(iyp.gt.ny) iyp=iyp-ny
        delx=max(0.,min(delx,1.))
        g(i)=f(ix,iy)*(1.-delx)*(1.-dely)
     *      +f(ixp,iy)*delx*(1.-dely)
     *      +f(ix,iyp)*(1.-delx)*dely
     *      +f(ixp,iyp)*delx*dely
100   continue
      return
      end
