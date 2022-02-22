      subroutine m1poly(n,rad,p)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    m1poly      used for computing gaussian lats
c   prgmmr: sela             org: w/nmc22    date: 79-03-03
c
c abstract: compute legendre polynomial of order n 
c
c program history log:
c   79-03-03  sela
c   88-04-08  parrish   add docblock
c
c   input argument list:
c     n,rad    - order and argument of desired legendre polynomial
c
c   output argument list:
c     p        - value of desired legendre polynomial
c
c attributes:
c   language: cft77
c   machine:  cray
c
c$$$
      x=  cos(rad)
      y1=1.e0
      y2=x
      do 1 i=2,n
        g=x*y2
        y3=g-y1+g-(g-y1)/float(i)
        y1=y2
        y2=y3
1     continue
      p=y3
      return
      end
