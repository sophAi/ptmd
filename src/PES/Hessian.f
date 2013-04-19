*========================================================================
* File Name : Hessian.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時53分49秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      Program Hessian               !2008.04.17 
      IMPLICIT NONE                 !NCTU B.J.Huang 
      real*8 pes_function,DATT,DDATT,REP,DREP,DDREP
      real*8 ZETA_AB,EPSILON_AB,P_AB,Q_AB,RZERO_AB
      real*8 M_Ag,M_Cu,M_AB
      real*8 ATT(300),ATT_DUMMY,REP_DUMMY
      real*8 POT_REP,POT_ATT,x(42)
      real*8 POT_REPI,POT_ATTI
      real*8 GRAD_ATTM(300,300),GRAD_REPM(300,300),GRAD(300)
      real*8 DUMMYX,DUMMYY,DUMMYZ,XMUL2,XMUL3,GRAD_ATT(300)

      integer NDIM,atom_num,atom_num_a,J1,J2,J3,J4,Js,I,km,pn,Ic
      integer indx,indy,I1,I2,nc,pev,nev,a1,a2,a3,a4,a5,a6
      integer mm
      character*10 frame,potname,Agname,guptaname
      character eq
      real*8 xx1,xx2,xx3,xx4,xx5,mass
      real*8 Eng,bind,xx,A_term,B_term,C_term,S_term
      real*8 A1_term,B1_term,C1_term
      real*8 u(14),du(14,3),tmp!pot_per_atom , dpot_per_atom
      real*8 h(42,42),hp(42,42),S(42,42),rij(14,14,3),DIST(14,14)
      real*8 d(42),e(42)
      real*8 DOS(500),w,eup,elow,dev,idev
      common /parabk/ EPSILON_AB,ZETA_AB,RZERO_AB,P_AB,Q_AB
      common /mbk/    M_Ag,M_Cu,M_AB
      common /nbk/ atom_num_a
!      open(10,FILE='./Ag14_local_min/BIMD_'//
!     &'BIMD_pes_gupta_pure_Ag0014_0050_hist.xyz')
      open(10,FILE='0100.dat')
      open(20,FILE='w0100K.dat')
      mm=0
      nc=1
      nev=250
      dev=0.51d0
      idev=-30.d0
      do I=1,nev
         DOS(I) = 0.d0
      enddo
      do 1000 Ic=1,nc
      NDIM=14
      atom_num=14
      atom_num_a=14
c

      do J1=1,3*atom_num
           d(J1)=0.d0
           e(J1)=0.d0
        do J2=1,3*atom_num
           h(J1,J2)=0.d0
           hp(J1,J2)=0.d0
           S(J1,J2)=0.d0
        enddo
      enddo
      READ(10,*)a1,frame,a2,Agname,eq,a3,Agname,guptaname,Agname
      READ(10,*)Potname,Eng,frame,Eng,frame,a3,frame,xx1
      write(*,*)Ic,Eng
!      READ(10,*)a1,a2,a3
!      READ(10,*)Eng
      DO I=1,atom_num
           READ(10,*)Agname,x(3*I-2),x(3*I-1),x(3*I),frame,xx1,xx2,xx3
     &,xx4,xx5,a1
!           write(*,*)'I= ',I,' ',x(I),x(I+1),x(I+2)
      ENDDO
!      cycle
!      if(mod(Ic,100).ne.0)cycle
      mm = mm + 1
!      write(*,*)Ic,Eng
!      stop
c==================================
c     set DIST , rij 
c==================================
      do J1=1,atom_num
         do J2=1,atom_num
         J3=3*J1
         J4=3*J2
          rij(J1,J2,1)= x(J4-2)- x(J3-2)    ! x vector 
          rij(J1,J2,2)= x(J4-1)- x(J3-1)    ! y vector
          rij(J1,J2,3)= x(J4)  - x(J3)      ! z vector
          DIST(J1,J2)=rij(J1,J2,1)**2+ rij(J1,J2,2)**2 + rij(J1,J2,3)**2
          DIST(J1,J2)=DSQRT(DIST(J1,J2))
c         write(*,'(2I5,4f10.5)')J1,J2,DIST(J1,J2),rij(J1,J2,0)
c     &,rij(J1,J2,1),rij(J1,J2,2)    
         enddo
      enddo
c==================================
c     set u, du
c==================================
      do J1=1,atom_num
      u(J1)=0.d0
        do Js=1,3
        du(J1,Js)=0.d0
        enddo
         do J2=1,atom_num
         if(J1.eq.J2)cycle
         J3=3*J1
         J4=3*J2
        u(J1) = u(J1) + REP(J1,J2,DIST) 
        du(J1,1) = du(J1,1) + DREP(J1,J2,DIST)*rij(J1,J2,1)/DIST(J1,J2) 
        du(J1,2) = du(J1,2) + DREP(J1,J2,DIST)*rij(J1,J2,2)/DIST(J1,J2) 
        du(J1,3) = du(J1,3) + DREP(J1,J2,DIST)*rij(J1,J2,3)/DIST(J1,J2) 
         enddo
c         write(*,'(4f10.5)')u(J1),du(J1,1),du(J1,2),du(J1,3)
c         write(*,'(I5,f10.5)')J1,u(J1)
      enddo
c===================================
c     pairwise term matrix --hp
c     ATT part
c===================================
      DO 100 J1=1,atom_num
        J3=3*J1 !index for J1 x,y,z
        DO 200 J2=1,atom_num
          IF(J2.EQ.J1)cycle

          J4=3*J2 !index for J2 x,y,z
!         write(*,'(2I5,2f12.8)')J1,J2,DATT(J1,J2,DIST),DDATT(J1,J2,DIST)
!           for J1 != J2 , off-diagonal element
            do km=2,0,-1
                do pn=2,0,-1
      hp(J3-km,J4-pn)=( DDATT(J1,J2,DIST)-DATT(J1,J2,DIST)/DIST(J1,J2))
     &*rij(J1,J2,3-km)*rij(J1,J2,3-pn)/(DIST(J1,J2)**2.0)     

       if(km.eq.pn)then
       hp(J3-km,J4-pn) = hp(J3-km,J4-pn) 
     &+ DATT(J1,J2,DIST)/DIST(J1,J2) 
       endif
        hp(J3-km,J4-pn) = -2.0*hp(J3-km,J4-pn)  
                enddo
            enddo
200   continue      
100   continue     
!      do J1=1,12
!       write(*,'(6e15.5)')(hp(J1,J2),J2=1,6)
!       if(mod(J1,3).eq.0)write(*,*)"--------------------------------"
!      enddo
!      stop
c      do J1=1,atom_num
c         do J2=1,atom_num
c         if(J1.eq.J2)cycle
c         write(*,'(2I5,2f12.8)')J1,J2,DREP(J1,J2,DIST),DDREP(J1,J2,DIST)
c         enddo
c      enddo
c      stop
!==================================
!     non-pairwise tem  -- h + S                  
      !REP part
!==================================            
      do J1=1,atom_num
        do J2=1,atom_num
        if(J1.eq.J2)cycle
        J3=3*J1 !index for J1 x,y,z
        J4=3*J2 !index for J2 x,y,z
            do km=2,0,-1
                do pn=2,0,-1
      A_term=  -0.25d0*u(J1)**(-1.5d0)*du(J1,3-km)*DREP(J1,J2,DIST) 
     & *rij(J1,J2,3-pn)/DIST(J1,J2) 
      A1_term= -0.25d0*u(J2)**(-1.5d0)*du(J2,3-pn)*DREP(J1,J2,DIST) 
     & *rij(J2,J1,3-km)/DIST(J1,J2)

!      write(*,'(4i5,2f12.8)')J1,J2,2-km,2-pn,A_term,A1_term

      B_term  = 0.5d0*u(J1)**(-0.5d0)*DDREP(J1,J2,DIST)
     &*rij(J1,J2,3-km)*rij(J1,J2,3-pn)
     &/DIST(J1,J2)/DIST(J1,J2)

      B1_term = 0.5d0*u(J2)**(-0.5d0)*DDREP(J1,J2,DIST)
     &*rij(J1,J2,3-km)*rij(J1,J2,3-pn)
     &/DIST(J1,J2)/DIST(J1,J2)

      C_term  = -0.5d0*u(J1)**(-0.5d0)*DREP(J1,J2,DIST)/DIST(J1,J2)
     &*rij(J1,J2,3-km)*rij(J1,J2,3-pn)
     &/DIST(J1,J2)/DIST(J1,J2)

      C1_term = -0.5d0*u(J2)**(-0.5d0)*DREP(J1,J2,DIST)/DIST(J1,J2)
     &*rij(J1,J2,3-km)*rij(J1,J2,3-pn)
     &/DIST(J1,J2)/DIST(J1,J2)

!      write(*,'(4i5,2f12.8)')J1,J2,2-km,2-pn,A_term,A1_term
        if(km.eq.pn)then
          C_term  =  C_term + 0.5d0*u(J1)**(-0.5d0)
     &*DREP(J1,J2,DIST)/DIST(J1,J2)      
          C1_term = C1_term + 0.5d0*u(J2)**(-0.5d0)
     &*DREP(J1,J2,DIST)/DIST(J1,J2)      
        endif

!      write(*,'(4i5,2f12.8)')J1,J2,2-km,2-pn,(B_term+C_term)
!     &,(B1_term+C1_term) 

      h(J3-km,J4-pn)= h(J3-km,J4-pn) + A_term + A1_term
     & + B_term + C_term + B1_term + C1_term

                enddo !pn
            enddo     !km
        enddo         !J2
      enddo           !J1

      ! S_term
      do J1=1,atom_num
        do J2=1,atom_num
        if(J1.eq.J2)cycle
        J3=3*J1 !index for J1 x,y,z
        J4=3*J2 !index for J2 x,y,z
      !=====================
            do km=2,0,-1
                do pn=2,0,-1
      !Define S_term
      do Js=1,atom_num 
      if(Js.eq.J1)cycle
      if(Js.eq.J2)cycle
      S(J3-km,J4-pn)=S(J3-km,J4-pn)+0.25d0*u(Js)**(-1.5d0)
     &*DREP(Js,J1,DIST)*DREP(Js,J2,DIST)
     & *rij(Js,J1,3-km)/DIST(J1,Js)
     & *rij(Js,J2,3-pn)/DIST(J2,Js)
      enddo
!      write(*,*)"S_term ",J1,J2,2-km,2-pn,S(J3-km,J4-pn)
               enddo
            enddo  
      !====================
        enddo
      enddo
!      stop
!=============================================
!           Combine hp, h, S term
!=============================================      
      do J1=1,atom_num
        do J2=1,atom_num
        if(J1.eq.J2)cycle
        J3=3*J1 !index for J1 x,y,z
        J4=3*J2 !index for J2 x,y,z
            do km=2,0,-1
                do pn=2,0,-1
          h(J3-km,J4-pn) = h(J3-km,J4-pn) + S(J3-km,J4-pn)
     & + hp(J3-km,J4-pn)
                enddo
            enddo
        enddo
      enddo
!==============================================            
!           for J1 = J2 , diagonal element ,sum-rule
!==============================================
      do J1=1,atom_num
        do J2=1,atom_num
             if(J2.eq.J1)cycle
             do I1=1,3
                do I2=1,3
                    indx=3*(J1-1)+I1     
                    indy=3*(J1-1)+I2     
                    h(indx,indy) = h(indx,indy) - h(indx,3*(J2-1)+I2)
                enddo
            enddo
        enddo
      enddo
!    Setup Unit
!    1 ev = 1.60218 x 10^-19 J
!    1 a.u. mass = 1.6609 x 10^-27 kg
      do J1=1,atom_num
        do J2=1,atom_num
             do I1=1,3
                do I2=1,3
                 indx=3*(J1-1)+I1     
                 indy=3*(J2-1)+I2     
                 call paramselec(J1,J2)
               h(indx,indy)=h(indx,indy)*(1.60218/1.6609)/(M_AB)**0.5d0
             enddo
             enddo
        enddo
      enddo
!      do J1=1,6
!       write(*,'(6f15.10)')(h(J1,J2),J2=1,6)
!       if(J1.eq.3)then
!!      write(*,*)'-------------------------------------------------'
!       endif
!      enddo
!      stop
      call tred2(h,3*atom_num,3*atom_num,d,e)
      call tqli(d,e,3*atom_num,3*atom_num,h)
      do J1=1,3*atom_num
        if(d(J1).lt.0.d0)d(J1)=-1.0*(-d(J1))**0.5d0
        if(d(J1).gt.0.d0)d(J1)=(d(J1))**0.5d0
        d(J1)=d(J1)*100.d0
         write(*,*)Ic,J1,d(J1)
      enddo
!      stop
      do I=1,nev
        eup=idev+(I+1)*dev
        elow=idev+I*dev
!        write(*,*)eup,elow
        do J1=1,3*atom_num
           if((d(J1).lt.eup).and.(d(J1).ge.elow))then 
!                   DOS(I) = DOS(I) + 1.d0/(3.0*atom_num)
                   DOS(I) = DOS(I) + 1.d0
           endif   
        enddo
      enddo

c==========================================      
1000  continue !Configuration iteration      
      do I=1,nev
!         if(DOS(I)/mm.gt.2.5)DOS(I)=0.0
         write(20,*)idev+(I+0.5d0)*dev,DOS(I)/mm
      enddo
!      RETURN
      END
      function DDREP(J1,J2,DIST)
      integer J1,J2
      real*8 Ust,DIST(14,14),DDREP
      real*8 EPSILON_AB,ZETA_AB,RZERO_AB,P_AB,Q_AB
      common /parabk/ EPSILON_AB,ZETA_AB,RZERO_AB,P_AB,Q_AB
      common /nbk/ atom_num_a

      call paramselec(J1,J2)
      DDREP=ZETA_AB*ZETA_AB*(2.0*Q_AB/RZERO_AB)**2.0*
     &dexp(2.0*Q_AB*(1.0 - DIST(J1,J2)/RZERO_AB))
      return
      end

      function DREP(J1,J2,DIST)
      integer J1,J2
      real*8 Ust,DIST(14,14),DREP
      real*8 EPSILON_AB,ZETA_AB,RZERO_AB,P_AB,Q_AB
      common /parabk/ EPSILON_AB,ZETA_AB,RZERO_AB,P_AB,Q_AB
      common /nbk/ atom_num_a

      call paramselec(J1,J2)
      DREP=(-2.0*ZETA_AB*ZETA_AB*Q_AB/RZERO_AB)*
     &dexp(2.0*Q_AB*(1.0 - DIST(J1,J2)/RZERO_AB))
      return
      end

      function REP(J1,J2,DIST)
      integer J1,J2
      real*8 Ust,DIST(14,14),REP
      real*8 EPSILON_AB,ZETA_AB,RZERO_AB,P_AB,Q_AB
      common /parabk/ EPSILON_AB,ZETA_AB,RZERO_AB,P_AB,Q_AB
      common /nbk/ atom_num_a

      call paramselec(J1,J2)
      REP=ZETA_AB*ZETA_AB*dexp(2.0*Q_AB*(1.0 - DIST(J1,J2)/RZERO_AB))
      return
      end

      function DATT(J1,J2,DIST)
      integer J1,J2
      real*8 Ust,DIST(14,14),DATT
      real*8 EPSILON_AB,ZETA_AB,RZERO_AB,P_AB,Q_AB
      common /parabk/ EPSILON_AB,ZETA_AB,RZERO_AB,P_AB,Q_AB
      common /nbk/ atom_num_a

      call paramselec(J1,J2)
      Ust=EPSILON_AB*dexp(P_AB*(1.0 - DIST(J1,J2)/RZERO_AB))
      DATT=-P_AB/RZERO_AB*Ust
!      write(*,'(2I5,4f10.5)')J1,J2,EPSILON_AB,P_AB,RZERO_AB,DATT
      return
      end

      function DDATT(J1,J2,DIST)
      integer J1,J2
      real*8 Ust,DIST(14,14),DDATT
      real*8 EPSILON_AB,ZETA_AB,RZERO_AB,P_AB,Q_AB
      common /parabk/ EPSILON_AB,ZETA_AB,RZERO_AB,P_AB,Q_AB
      common /nbk/ atom_num_a

      call paramselec(J1,J2)
      Ust=EPSILON_AB*dexp(P_AB*(1.0 - DIST(J1,J2)/RZERO_AB))
      DDATT=((P_AB/RZERO_AB)**2.0)*Ust
      return
      end

      subroutine paramselec(J1,J2)
      integer J1,J2,atom_num_a
      real*8 EPSILON_AB,ZETA_AB,RZERO_AB,P_AB,Q_AB
      real*8 M_Ag,M_Cu,M_AB
      real*8  P1,Q1,EPSILON1,ZETA1,RZERO1
      real*8  P2,Q2,EPSILON2,ZETA2,RZERO2
      real*8  P3,Q3,EPSILON3,ZETA3,RZERO3
      common /parabk/ EPSILON_AB,ZETA_AB,RZERO_AB,P_AB,Q_AB
      common /mbk/    M_Ag,M_Cu,M_AB
      common /nbk/ atom_num_a
      M_Ag = 107.8682
      M_Cu = 63.546

      EPSILON1= 0.1031
      ZETA1   = 1.1895
      P1      =10.85
      Q1      = 3.18
      RZERO1  = 2.89          !Ag-Ag

      EPSILON2= 0.0894
      ZETA2   = 1.2799
      P2      =10.550
      Q2      = 2.430
      RZERO2  = 2.560         !Cu-Cu
      
      EPSILON3= 0.0977
      ZETA3   = 1.2275
      P3      =10.70
      Q3      = 2.805
      RZERO3  = 2.725         !Ag-Cu
            IF ((J1.LE.atom_num_a).AND.(J2.LE.atom_num_a))THEN
               P_AB=P1                           !Ag-Ag
               Q_AB=Q1
               EPSILON_AB=EPSILON1
               ZETA_AB=ZETA1
               RZERO_AB=RZERO1
               M_AB=M_Ag*M_Ag
            ELSE IF((J1.GT.atom_num_a).AND.(J2.GT.atom_num_a))THEN
               P_AB=P2                           !Cu-Cu
               Q_AB=Q2
               EPSILON_AB=EPSILON2
               ZETA_AB=ZETA2
               RZERO_AB=RZERO2
               M_AB=M_Cu*M_Cu
            ELSE
               P_AB=P3                           !Cu-Ag
               Q_AB=Q3
               EPSILON_AB=EPSILON3
               ZETA_AB=ZETA3
               RZERO_AB=RZERO3
               M_AB=M_Cu*M_Ag
            ENDIF
      return
      end

      SUBROUTINE tqli(d,e,n,np,z)
      INTEGER n,np
      REAL*8 d(np),e(np),z(np,np)
CU    USES pythag
      INTEGER i,iter,k,l,m
      REAL*8 b,c,dd,f,g,p,r,s,pythag
      do 11 i=2,n
        e(i-1)=e(i)
11    continue
      e(n)=0.
      do 15 l=1,n
        iter=0
1       do 12 m=l,n-1
          dd=abs(d(m))+abs(d(m+1))
          if (abs(e(m))+dd.eq.dd) goto 2
12      continue
        m=n
2       if(m.ne.l)then
          if(iter.eq.30)pause 'too many iterations in tqli'
          iter=iter+1
          g=(d(l+1)-d(l))/(2.*e(l))
          r=pythag(g,1.d0)
          g=d(m)-d(l)+e(l)/(g+sign(r,g))
          s=1.
          c=1.
          p=0.
          do 14 i=m-1,l,-1
            f=s*e(i)
            b=c*e(i)
            r=pythag(f,g)
            e(i+1)=r
            if(r.eq.0.)then
              d(i+1)=d(i+1)-p
              e(m)=0.
              goto 1
            endif
            s=f/r
            c=g/r
            g=d(i+1)-p
            r=(d(i)-g)*s+2.*c*b
            p=s*r
            d(i+1)=g+p
            g=c*r-b
C     Omit lines from here ...
            do 13 k=1,n
              f=z(k,i+1)
              z(k,i+1)=s*z(k,i)+c*f
              z(k,i)=c*z(k,i)-s*f
13          continue
C     ... to here when finding only eigenvalues.
14        continue
          d(l)=d(l)-p
          e(l)=g
          e(m)=0.
          goto 1
        endif
15    continue
      return
      END

      SUBROUTINE tred2(a,n,np,d,e)
      INTEGER n,np
      REAL*8 a(np,np),d(np),e(np)
      INTEGER i,j,k,l
      REAL*8 f,g,h,hh,scale
      do 18 i=n,2,-1
        l=i-1
        h=0.
        scale=0.
        if(l.gt.1)then
          do 11 k=1,l
            scale=scale+abs(a(i,k))
11        continue
          if(scale.eq.0.)then
            e(i)=a(i,l)
          else
            do 12 k=1,l
              a(i,k)=a(i,k)/scale
              h=h+a(i,k)**2
12          continue
            f=a(i,l)
            g=-sign(sqrt(h),f)
            e(i)=scale*g
            h=h-f*g
            a(i,l)=f-g
            f=0.
            do 15 j=1,l
C     Omit following line if finding only eigenvalues
              a(j,i)=a(i,j)/h
              g=0.
              do 13 k=1,j
                g=g+a(j,k)*a(i,k)
13            continue
              do 14 k=j+1,l
                g=g+a(k,j)*a(i,k)
14            continue
              e(j)=g/h
              f=f+e(j)*a(i,j)
15          continue
            hh=f/(h+h)
            do 17 j=1,l
              f=a(i,j)
              g=e(j)-hh*f
              e(j)=g
              do 16 k=1,j
                a(j,k)=a(j,k)-f*e(k)-g*a(i,k)
16            continue
17          continue
          endif
        else
          e(i)=a(i,l)
        endif
        d(i)=h
18    continue
C     Omit following line if finding only eigenvalues.
      d(1)=0.
      e(1)=0.
      do 24 i=1,n
C     Delete lines from here ...
        l=i-1
        if(d(i).ne.0.)then
          do 22 j=1,l
            g=0.
            do 19 k=1,l
              g=g+a(i,k)*a(k,j)
19          continue
            do 21 k=1,l
              a(k,j)=a(k,j)-g*a(k,i)
21          continue
22        continue
        endif
C     ... to here when finding only eigenvalues.
        d(i)=a(i,i)
C     Also delete lines from here ...
        a(i,i)=1.
        do 23 j=1,l
          a(i,j)=0.
          a(j,i)=0.
23      continue
C     ... to here when finding only eigenvalues.
24    continue
      return
      END
      FUNCTION pythag(a,b)
      REAL*8 a,b,pythag
      REAL*8 absa,absb
      absa=abs(a)
      absb=abs(b)
      if(absa.gt.absb)then
        pythag=absa*dsqrt(1.+(absb/absa)**2)
      else
        if(absb.eq.0.)then
          pythag=0.
        else
          pythag=absb*sqrt(1.+(absa/absb)**2)
        endif
      endif
      return
      END
* ======================GNU General Public License=======================
* This program is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation; either version 2 of the License, or
* (at your option) any later version.
*  
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU General Public License for more details.
* 
* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
* =======================================================================
