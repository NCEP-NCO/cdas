      subroutine prnon85(on85)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    prnon85   print on85 information
c   prgmmr: parrish        org: w/nmc22    date: 90-10-10
c
c abstract: print on85 information.
c
c program history log:
c   90-10-10  parrish
c
c   input argument list:
c     on85     - office note 85 record                     
c
c   output argument list:
c     none
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
c--------
      character*4 on85(8)
      write(6,100)on85
100   format(1h ,4z19)
      return
      end
