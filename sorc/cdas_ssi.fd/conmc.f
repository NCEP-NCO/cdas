      function conmc(cname)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    conmc       provide nmc*s value for physical const.
c   prgmmr: parrish          org: w/nmc22    date: 88-09-09
c
c abstract: return nmc handbook value for requested physical const.
c   note that some values have been modified based on bill collins
c   suggestion.  (g, rd, cp)
c
c program history log:
c   88-09-09  parrish
c
c usage:    const=conmc(cname)
c   input argument list:
c     cname    - character array containing name of constant as
c              - suggested in nmc handbook, sec. 3.4.2.
c              - must end in a "$" sign.
c
c attributes:
c   language: cft77
c   machine:  cray
c
c$$$
      character*1 cname(1),dollar
      data dollar/'$'/
      character*8  tabone(15)
      data  tabone/
     *  'rerth   ','g       ','omega   ','rd      ','cp      ',
     *  'cv      ','rv      ','cvap    ','cliq    ','hvap    ',
     *  'hfus    ','psat    ','sbc     ','solr    ','pi      '/
      character*1 tabnam(8,15)
      integer lnam(15)
      data lnam/5,1,5,2,2,2,2,4,4,4,4,4,3,4,2/
      equivalence (tabone,tabnam)
c******
c****** values from nmc handbook
c******
c     double precision consts(15)
c     data consts/
c    *  6.3712e6,    9.8062, 7.2921e-5,  2.8704e2,  1.0046e3,
c    *  7.1760e2,  4.6150e2,  1.8460e3,  4.1855e3,  2.5000e6,
c    *  3.3358e5,  6.1078e2, 5.6730e-8,  1.3533e3,    3.1416/
c******
c****** modified values, based on bill collins suggestions.
c******
       dimension consts(15)
       data consts/
     *   6.3712e6,    9.8000, 7.2921e-5,  2.8705e2,  1.0045e3,
     *   7.1760e2,  4.6150e2,  1.8460e3,  4.1855e3,  2.5000e6,
     *   3.3358e5,  6.1078e2, 5.6730e-8,  1.3533e3,    3.1416/
c--------
c-------- first find number of characters in cname
c--------
      ii=0
      do 100 i=2,9
        ii=ii+1
        if(cname(i).eq.dollar) go to 200
100   continue
      go to 500
200   continue
c--------
c-------- now find a match
c--------
      do 400 k=1,15
        jj=lnam(k)
        if(ii.eq.jj) then
          match=0
          do 300 i=1,ii
            if(cname(i).eq.tabnam(i,k)) match=match+1
300       continue
          if(match.eq.ii) then
            conmc=consts(k)
            return
          end if
        end if
400   continue
500   continue
c--------
c-------- here for trouble only
c--------
      print *,'trouble in conmc'
      stop 56
      end
