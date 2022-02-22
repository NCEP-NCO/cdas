      subroutine intrp3(f,g,dx,dy,dz,nx,ny,nz,n)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    intrp3      linear interpolation in 3 dimensions.
c   prgmmr: parrish          org: w/nmc22    date: 90-10-11
c
c abstract: linear interpolate in 3 dims (2nd dim always periodic).
c
c program history log:
c   90-10-11  parrish
c
c   input argument list:
c     f        - input interpolator
c     dx,dy,dz - input x,y,z-coords of interpolation points (grid units)
c     nx,ny,nz - x,y,z-dimensions of interpolator grid
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
      dimension f(nx+1,ny+2,nz),g(n),dx(n),dy(n),dz(n)
c--------
      do 100 i=1,n
        ix=dx(i)
        iy=dy(i)
        iz=dz(i)
        ix=max(1,min(ix,nx))
        iz=max(1,min(iz,nz))
        ixp=ix+1
        izp=iz+1
        ixp=min(ixp,nx)
        izp=min(izp,nz)
        delx=dx(i)-ix
        dely=dy(i)-iy
        delz=dz(i)-iz
        if(iy.lt.1) iy=iy+ny
        if(iy.gt.ny) iy=iy-ny
        iyp=iy+1
        if(iyp.gt.ny) iyp=iyp-ny
        delx=max(0.,min(delx,1.))
        delz=max(0.,min(delz,1.))
        g(i)=f(ix,iy,iz)*(1.-delx)*(1.-dely)*(1.-delz)
     *      +f(ixp,iy,iz)*delx*(1.-dely)*(1.-delz)
     *      +f(ix,iyp,iz)*(1.-delx)*dely*(1.-delz)
     *      +f(ixp,iyp,iz)*delx*dely*(1.-delz)
     *      +f(ix,iy,izp)*(1.-delx)*(1.-dely)*delz
     *      +f(ixp,iy,izp)*delx*(1.-dely)*delz
     *      +f(ix,iyp,izp)*(1.-delx)*dely*delz
     *      +f(ixp,iyp,izp)*delx*dely*delz
100   continue
      return
      end
