      subroutine dtast(type,res,scale,n,rspres,pbot,ptop,mesage)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    dtast       print table of residuals by data type.
c   prgmmr: parrish          org: w/nmc22    date: 90-10-11
c
c abstract: print table of residuals by data type.
c
c program history log:
c   90-10-11  parrish
c
c usage call dtast(type,res,scale,n,rspres,pbot,ptop,mesage)
c   input argument list:
c     type     - obs types (prepda numbers)
c     res      - obs residuals
c     scale    - rescaling unit
c     n        - number of residuals
c     rspres   - observation pressure
c     pbot     - pressure at bottom of layer
c     ptop     - pressure at top of layer
c     mesage   - message to appear at top of table ($ signals end)
c
c   output argument list:
c     none
c
c attributes:
c   language: cft77
c   machine:  cray ymp
c
c$$$
      dimension type(n),res(n)
      dimension count(7,101),rms(7,101),bias(7,101)
      dimension rspres(n)
      character*1 mesage(100),dollar
      data dollar/'$'/
c--------
      count=0.
      rms=0.
      bias=0.
      ioff=int(type(1)/100)*100
c--------
      do 100 i=1,n
        if(rspres(i).lt.ptop.or.rspres(i).gt.pbot) go to 99
        ress=res(i)*scale
        absr=abs(ress)
        k=1
        if(absr.gt.1.) k=2
        if(absr.gt.2.) k=3
        if(absr.gt.4.) k=4
        if(absr.gt.7.) k=5
        if(absr.gt.10.) k=6
        if(absr.gt.50.) k=7
        itype=nint(type(i))-ioff
        itype=max(1,min(itype,100))
        do 90 l=k,7
          count(l,itype)=count(l,itype)+1.
          bias(l,itype)=bias(l,itype)+ress
          rms(l,itype)=rms(l,itype)+ress*ress
          count(l,101)=count(l,101)+1.
          bias(l,101)=bias(l,101)+ress
          rms(l,101)=rms(l,101)+ress*ress
90      continue
99      continue
100   continue
      if(count(7,101) .le. 0.)return
      imsg=1
      do 400 k=1,100
        if(mesage(k).eq.dollar) go to 410
        imsg=imsg+1
400   continue
410   continue
      imsg=max(1,imsg-1)
      write(6,500)(mesage(i),i=1,imsg)
500   format(//,1h ,100a1,//,'  count/bias/rms:',/)
      write(6,600)
600   format(t2,'type',t12,'(e<1)',t22,'(e<2)',t32,'(e<4)',
     *         t42,'(e<7)',t52,'(e<10)',t62,'(e<50)',t72,'(50<e)',/)
      do 110 i=1,101
      if(count(7,i).gt.0.) then
        do 200 k=1,7
          if(count(k,i).gt.0.) then
            bias(k,i)=bias(k,i)/count(k,i)
            rms(k,i)=sqrt(rms(k,i)/count(k,i))
          end if
200     continue
        i2=i+ioff
        if(i .le. 100)then
          write(6,700)i2,(count(k,i),k=1,7)
        else
          write(6,950)(count(k,i),k=1,7)
        end if
        write(6,800)(bias(k,i),k=1,7)
        write(6,810)(rms(k,i),k=1,7)
      end if
110   continue
700   format(t2,i3,t8,f9.0,t18,f9.0,t28,f9.0,t38,f9.0,t48,f9.0,
     *              t58,f9.0,t68,f9.0)
800   format(t4,'bias',
     *        t8,g9.2,t18,g9.2,t28,g9.2,t38,g9.2,t48,g9.2,t58,g9.2,
     *           t68,g9.2)
810   format(t4,'rms',
     *        t8,g9.2,t18,g9.2,t28,g9.2,t38,g9.2,t48,g9.2,t58,g9.2,
     *           t68,g9.2)
950   format(t2,'all',t8,f9.0,t18,f9.0,t28,f9.0,t38,f9.0,
     *             t48,f9.0,t58,f9.0,t68,f9.0)
      return
      end
