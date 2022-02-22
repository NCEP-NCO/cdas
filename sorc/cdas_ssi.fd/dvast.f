      subroutine dvast(type,resu,resv,scale,n,rspres,pbot,ptop,mesage)
c$$$  subprogram documentation block
c                .      .    .                                       .
c subprogram:    dvast       print table of vector res. by data type.
c   prgmmr: parrish          org: w/nmc22    date: 90-10-11
c
c abstract: print table of vector residuals by data type.
c
c program history log:
c   90-10-11  parrish
c
c usage: call dvast(type,resu,resv,scale,n,rspres,pbot,ptop,mesage)
c   input argument list:
c     type     - obs types (prepda numbers)
c     resu     - obs u residuals
c     resv     - obs v residuals
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
      dimension type(n),resu(n),resv(n)
      dimension count(7,101),rms(7,101)
      dimension rspres(n)
      character*1 mesage(100),dollar
      data dollar/'$'/
c-------
      count=0.
      rms=0.
c--------
      ioff=int(type(1)/100)*100
      do 100 i=1,n
        if(rspres(i).lt.ptop.or.rspres(i).gt.pbot) go to 99
c       if(nint(type(i)) .ne. 283)then
        ress=scale*sqrt(resu(i)**2+resv(i)**2)
c       else
c       ress=scale*(resu(i)-resv(i))
c       end if
        absr=abs(ress)
        k=1
        if(absr.gt.2.) k=2
        if(absr.gt.4.) k=3
        if(absr.gt.8.) k=4
        if(absr.gt.15.) k=5
        if(absr.gt.30.) k=6
        if(absr.gt.80.) k=7
        itype=nint(type(i))-ioff
        itype=max(1,min(itype,100))
        do 90 l=k,7
          count(l,itype)=count(l,itype)+1.
          rms(l,itype)=rms(l,itype)+ress*ress
          count(l,101)=count(l,101)+1.
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
500   format(//,1h ,100a1,//,'  count/vrms:',/)
      write(6,600)
600   format(t2,'type',t12,'(e<2)',t22,'(e<4)',t32,'(e<8)',
     *         t42,'(e<15)',t52,'(e<30)',t62,'(e<80)',t72,'(80<e)',/)
      do 900 i=1,100
        i2=i+ioff
        if(count(7,i).gt.0.) then
          do 200 k=1,7
            if(count(k,i).gt.0.) then
              rms(k,i)=sqrt(rms(k,i)/count(k,i))
            end if
200       continue
          write(6,700)i2,(count(k,i),k=1,7)
          write(6,810)(rms(k,i),k=1,7)
        end if
700     format(t2,i3,t8,f9.0,t18,f9.0,t28,f9.0,t38,f9.0,t48,f9.0,
     *              t58,f9.0,t68,f9.0)
810     format(t4,'vrms',
     *        t8,f9.2,t18,f9.2,t28,f9.2,t38,f9.2,t48,f9.2,t58,f9.2,
     *           t68,f9.2)
900   continue
      do 920 l=1,7
      if(count(l,101) .gt. 0.)rms(l,101)=sqrt(rms(l,101)/count(l,101))
 920  continue
      write(6,950)(count(k,101),k=1,7)
      write(6,810)(rms(k,101),k=1,7)
950   format(t2,'all',t8,f9.0,t18,f9.0,t28,f9.0,t38,f9.0,
     *              t48,f9.0,t58,f9.0,t68,f9.0)
      return
      end
