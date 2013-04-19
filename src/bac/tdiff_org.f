      SUBROUTINE TDIFF(QP,QPDOT)
C=========================================================================
C THIS SUBROUTINE COMPUTES THE R.H. SIDE OF THE EQUATIONS OF MOTION.
C=========================================================================
      IMPLICIT NONE
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      include "../include/BIMD/BIMD.h"
C=======================LOCAL VARIABLES===============================
      integer N1,N2,N3,N4,N5,N6,kk,iii
      real*8 HX1,HX2,HX3,HY1,HY2,HY3,HZ1,
     &HZ2,HZ3,S,A1,A2,A3,A4_a,A4_b,NI
      real*8 PX1a,PX2a,PX3a,PX4a,PY1a,PY2a,
     &PY3a,PY4a,PZ1a,PZ2a,PZ3a,PZ4a
      real*8 PX1b,PX2b,PX3b,PX4b,PY1b,PY2b,
     &PY3b,PY4b,PZ1b,PZ2b,PZ3b,PZ4b
      real*8 QP(q_max),TQP(q_max),QPDOT(q_max),FXYZ(q_max)
      real*8 mem1,mem_force(q_max),multiply
      real*8 HX1_a,HX2_a,HX3_a,HY1_a,HY2_a,HY3_a,HZ1_a,HZ2_a,HZ3_a
C======================GLOBAL VARIABLES================================
      real*8 TMASS_a,TMASS_b,Q,P,EREP,EHOP,
     &R0,DP0_a
C=======================BINNING STAFF==================================
      real*8 EPOT,ETOT,EKIN,EROT,EVIB,RR,L2,BETA,GAMMA,INERT(3),
     &AX(3),AY(3),AZ(3),T,T1,fric
      Tmass_a=1.D0/mass_a
      Tmass_b=1.D0/mass_b
      N1=atom_num
      N2=atom_num*2
      N3=atom_num*3
      N4=atom_num*4
      N5=atom_num*5
      N6=atom_num*6
      NI = 1.0D0/DBLE(N1)
      S = -NI*DE0/DL0
      multiply=1D10
C      HX1 = S*QP(N6+1)**3*PSC1*multiply    
C      HX2 = S*QP(N6+2)*PSC2*multiply
C      HX3 = S*QP(N6+3)*PSC3*multiply
C      HY1 = S*QP(N6+4)**3*PSC1*multiply
C      HY2 = S*QP(N6+5)*PSC2*multiply
C      HY3 = S*QP(N6+6)*PSC3*multiply
C      HZ1 = S*QP(N6+7)**3*PSC1*multiply
C      HZ2 = S*QP(N6+8)*PSC2*multiply
C      HZ3 = S*QP(N6+9)*PSC3*multiply
      HX1 = S*QP(N6+1)**3*PSC1
      HX2 = S*QP(N6+2)*PSC2
      HX3 = S*QP(N6+3)*PSC3
      HY1 = S*QP(N6+4)**3*PSC1
      HY2 = S*QP(N6+5)*PSC2
      HY3 = S*QP(N6+6)*PSC3
      HZ1 = S*QP(N6+7)**3*PSC1
      HZ2 = S*QP(N6+8)*PSC2
      HZ3 = S*QP(N6+9)*PSC3     
      PX1a = 0.0D0     
      PX2a = 0.0D0     
      PX3a = 0.0D0     
      PX4a = 0.0D0     
      PY1a = 0.0D0     
      PY2a = 0.0D0     
      PY3a = 0.0D0     
      PY4a = 0.0D0     
      PZ1a = 0.0D0     
      PZ2a = 0.0D0     
      PZ3a = 0.0D0     
      PZ4a = 0.0D0     

      PX1b = 0.0D0     
      PX2b = 0.0D0     
      PX3b = 0.0D0     
      PX4b = 0.0D0     
      PY1b = 0.0D0     
      PY2b = 0.0D0     
      PY3b = 0.0D0     
      PY4b = 0.0D0     
      PZ1b = 0.0D0     
      PZ2b = 0.0D0     
      PZ3b = 0.0D0     
      PZ4b = 0.0D0     
     
      kk = 0
      do I1 = N3+1, N4
        kk=kk+1
        S = QP(I1)
        if (kk.le.atom_num_a) then
          PX1a = PX1a + S
          PX2a = PX2a + S**2
          PX3a = PX3a + S**3
          PX4a = PX4a + S**4
        else
          PX1b = PX1b + S
          PX2b = PX2b + S**2
          PX3b = PX3b + S**3
          PX4b = PX4b + S**4
        endif
      enddo
      kk = 0
      do I1 = N4+1, N5
        kk=kk+1
        S = QP(I1)
        if (kk.le.atom_num_a) then
          PY1a = PY1a + S
          PY2a = PY2a + S**2
          PY3a = PY3a + S**3
          PY4a = PY4a + S**4
        else
          PY1b = PY1b + S
          PY2b = PY2b + S**2
          PY3b = PY3b + S**3
          PY4b = PY4b + S**4
        endif
      enddo
      kk = 0
      do I1 = N5+1, N6
        kk=kk+1
        S = QP(I1)
        if (kk.le.atom_num_a) then
          PZ1a = PZ1a + S
          PZ2a = PZ2a + S**2
          PZ3a = PZ3a + S**3
          PZ4a = PZ4a + S**4
        else
          PZ1b = PZ1b + S
          PZ2b = PZ2b + S**2
          PZ3b = PZ3b + S**3
          PZ4b = PZ4b + S**4
        endif
      enddo
      do J1=1,atom_num
        J2=J1*3
        x(J2-2)=QP(J1)
        x(J2-1)=QP(J1+atom_num)
        x(J2)=QP(J1+atom_num*2)
C        write(*,"A6,3(1x,F12.7)") "AFTER ",x(J2-2),x(J2-1),x(J2)
        if (J1.le.atom_num_a) then
          Tqp(N3+J1)=Tmass_a*QP(N3+J1)
          Tqp(N3+J1+atom_num)=Tmass_a*QP(N3+J1+atom_num)
          Tqp(N3+J1+atom_num*2)=Tmass_a*QP(N3+J1+atom_num*2)
        else
          Tqp(N3+J1)=Tmass_b*QP(N3+J1)
          Tqp(N3+J1+atom_num)=Tmass_b*QP(N3+J1+atom_num)
          Tqp(N3+J1+atom_num*2)=Tmass_b*QP(N3+J1+atom_num*2)
        endif
      enddo 
      call pes 
C      call agrad   !already caculate grad in hamming, no need to do here!
C      write(*,*)"IN tdiff,pot=",pot
      do J1=1,atom_num
        J2=J1*3
        fxyz(J1)=-(grad(J2-2))
        fxyz(J1+atom_num)=-(grad(J2-1))
        fxyz(J1+atom_num*2)=-(grad(J2))
C        write(*,"A10,1x,F20.15,1x,F20.15,1x,F20.15")
C     &"gradintdiff",x(J2-2),x(J2-1),x(J2)
      enddo
C      pause"tdiff"
      do I1=1,N3                ! COORDINATES PART
        QPDOT(I1) = TQP(N3+I1)
      enddo
      mem1=1D3
C      mem1=multiply
      do I1=N3+1,N4              ! MOMENTA PART
        if ((I1-N3).gt.atom_num_a) then
          HX1_a=HX1*((mass_b/mass_a)**1.5)
          HX2_a=HX2*((mass_b/mass_a)**1.5)
          HX3_a=HX3*((mass_b/mass_a)**1.5)
          S=(QP(I1)/DP0)*((mass_b/mass_a)**(-0.5))
          QPDOT(I1)= FXYZ(I1-N3)*mem1+HX1_a*S + 
     &HX2_a*(S**2 - DA0)+HX3_a*S**3
C          QPDOT(I1)= QPDOT(I1)/multiply
        else    
          S = QP(I1)/DP0
          QPDOT(I1)= FXYZ(I1-N3)*mem1 + HX1*S + 
     &HX2*(S**2 - DA0)+HX3*S**3
C         QPDOT(I1)= QPDOT(I1)/multiply
        endif
      enddo
      do I1 = N4+1, N5
        if ((I1-N4).gt.atom_num_a) then
          HY1_a=hy1*((mass_b/mass_a)**1.5)
          HY2_a=hy2*((mass_b/mass_a)**1.5)
          HY3_a=hy3*((mass_b/mass_a)**1.5)
          S=(QP(I1)/DP0)*((mass_b/mass_a)**(-0.5))
          QPDOT(I1)= FXYZ(I1-N3)*mem1 +HY1_a*S+HY2_a*
     &(S**2-DA0)+HY3_a*S**3
C          QPDOT(I1)=QPDOT(I1)/multiply
        else    
          S=QP(I1)/DP0
          QPDOT(I1)=FXYZ(I1-N3)*mem1+ 
     &HY1*S+HY2*(S**2-DA0)+HY3*S**3
C          QPDOT(I1)=QPDOT(I1)/multiply
        endif
      enddo
      do I1=N5+1,N6
        if ((I1-N5).gt.atom_num_a)then
          HZ1_a=HZ1*((mass_b/mass_a)**1.5)
          HZ2_a=HZ2*((mass_b/mass_a)**1.5)
          HZ3_a=HZ3*((mass_b/mass_a)**1.5)
          S=(QP(I1)/DP0)*((mass_b/mass_a)**(-0.5))
          QPDOT(I1)=FXYZ(I1-N3)*mem1+HZ1_a*S+ 
     &HZ2_a*(S**2-DA0)+HZ3_a*S**3
C          QPDOT(I1)=QPDOT(I1)/multiply
        else    
          S=QP(I1)/DP0
          QPDOT(I1)=FXYZ(I1-N3)*mem1+HZ1*S+
     &HZ2*(S**2-DA0)+HZ3*S**3
C          QPDOT(I1)=QPDOT(I1)/multiply
        endif
      enddo
      S=kinetic_per_dim/(DP0*DL0)
      A1=PSC1*S
      A2=PSC2*S
      A3=PSC3*S
      A4_a=TMASS_a*NI/kinetic_per_dim
      A4_b=TMASS_b*NI/kinetic_per_dim
      DP0_a=DP0*((mass_b/mass_a)**0.5) 
      QPDOT(N6+1)=A1*(PX2a*A4_a+PX2b*A4_b-1.0D0)  ! 9 PESUDO-COEFIICIEN
      QPDOT(N6+2)=A2*((PX3a*A4_a/DP0)+(PX3b*A4_b/DP0_a)
     &-DA0*DP0*PX1a*A4_a-DA0*DP0_a*PX1b*A4_b  
     &-2.0D0*NI*((PX1a/dp0)+(PX1b/dp0_a)))
      QPDOT(N6+3)=A3*((PX4a*A4_a/(DP0**2))+
     &(PX4b*A4_b/(DP0_a**2))-3.0D0*NI*((PX2a/(dp0**2))+
     &(PX2b/(dp0_a**2))))
      QPDOT(N6+4)=A1*(PY2a*A4_a+PY2b*A4_b-1.D0)
      QPDOT(N6+5)=A2*((PY3a*A4_a/DP0)+(PY3b*A4_b/DP0_a) 
     *-DA0*DP0*PY1a*A4_a-DA0*DP0_a*PY1b*A4_b 
     *-2.0D0*NI*((PY1a/dp0)+(PY1b/dp0_a)))
      QPDOT(N6+6)=A3*((PY4a*A4_a/(DP0**2))+
     &(PY4b*A4_b/(DP0_a**2))-3.D0*NI*((PY2a/(dp0**2))+
     &(PY2b/(dp0_a**2))))
      QPDOT(N6+7)=A1*(PZ2a*A4_a+PZ2b*A4_b-1.0D0)
      QPDOT(N6+8)=A2*((PZ3a*A4_a/DP0)+(PZ3b*A4_b/DP0_a) 
     &-DA0*DP0*PZ1a*A4_a- DA0*DP0_A*PZ1b*A4_b 
     &-2.0D0*NI*((PZ1a/dp0)+(PZ1b/dp0_a)))
      QPDOT(N6+9)=A3*((PZ4a*A4_a/(DP0**2))+
     &(PZ4b*A4_b/(DP0_a**2)) -3.0D0*NI*
     &((PZ2a/(dp0**2))+(PZ2b/(dp0_a**2))))
      RETURN
      END
