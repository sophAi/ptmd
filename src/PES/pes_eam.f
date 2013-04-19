*========================================================================
* File Name : pes_eam.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時54分16秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
************************************************************
*  Energy and Gradient for EAM and alloy Potential.        *
************************************************************
      SUBROUTINE EAM(NDIM,POT,XINIB)
      IMPLICIT NONE
      INCLUDE "../../include/global_common.h"
      INCLUDE "../../include/common.h"
      INCLUDE "../../include/mbh_ndim.h"
      REAL*8 SUMP,SUMQ
     &,N1,M1
     &,C2,N2,M2,A3,C3,N3,M3
     &,PHI(NDIM),F(NDIM),PHI_DUMMY,F_DUMMY
     &,POT_PHI,POT_F
     &,POT_PHII,POT_FI
     &,DIST,DIST2,GRAD_PHI(NDIM,NDIM)
     &,GRAD_F(NDIM,NDIM),DISTP,DISTQ
     &,P_DUMMY,Q_DUMMY 
     &,DUMMYX,DUMMYY,DUMMYZ,XMUL2,XMUL3,F_DUMMYI
c      write(*,*) a1,a2,a3,c1,c2,c3,n1,n2,n3,m1,m2,m3,epsilon1,epsilon2
c     &           ,epsilon3
      EVAP=.FALSE.
      POT=0.0D0
      POT_PHII=0.0D0
      POT_FI=0.0D0
      RMS=0.D0
      DO J1=1,atom_num
        PHI(J1)=0.0D0
        F(J1)=0.0D0
      END DO
      DO J1=1,atom_num
        J3=3*J1
        bond_num(J1)=1.D0
        DIST2=(XINIB(J3-2)**2+XINIB(J3-1)**2+XINIB(J3)**2)
        IF (DIST2.GT.RADIUS2) THEN
          EVAP=.TRUE.
            POT=POT+restore_fac*(DIST2-RADIUS2)**2
        ENDIF
        POT_PHI=0.0D0
        POT_F=0.0D0
        PHI(J1)=0.0D0
        F(J1)=0.0D0
        GRAD_PHI(J1,J1)=0.0D0
        GRAD_F(J1,J1)=0.0D0
        DO J2=1,atom_num
          J4=3*J2
          IF(J2.NE.J1)THEN
            DIST=(XINIB(J3-2)-XINIB(J4-2))**2+
     &(XINIB(J3-1)-XINIB(J4-1))**2+(XINIB(J3)-XINIB(J4))**2
            DIST=DSQRT(DIST)
c            write(*,*) "DIST=",DIST
            PHI_DUMMY = ((eam_a1/DIST))**eam_n1
            F_DUMMY = ((eam_a1/DIST))**eam_m1  
            PHI(J1)= PHI(J1)+ PHI_DUMMY
            F(J1)= F(J1)+ F_DUMMY
            POT_PHI= POT_PHI+PHI_DUMMY
            POT_F= POT_F+F_DUMMY
            IF(DIST.LE.eam_a1*1.2D0)bond_num(J1)=bond_num(J1)+1.D0
          ENDIF
        ENDDO
        F_DUMMYI=DSQRT(POT_F)
        POT_PHII=POT_PHII+POT_PHI
        POT_FI=POT_FI+F_DUMMYI        
        VT(J1)=((1.D0/2.D0)*eam_epsilon1*(PHI(J1)))-
     &(eam_epsilon1*eam_C1*DSQRT(F(J1)))
      ENDDO
      POT=((1.D0/2.D0)*eam_epsilon1*POT_PHII)-(eam_epsilon1*
     &eam_C1*POT_FI)
c      write(*,*) "POT=",POT,POT_PHII*eam_epsilon1*0.5D0,POT_FI*epsilon*c
      DO J1=1,atom_num
        J3=3*J1
C        DIST2=X(J3-2)**2.D0+XINIB(J3-1)**2.D0+XINIB(J3)**2.D0
C        IF (DIST2.GT.RADIUS2) THEN
C          EVAP=.TRUE.
C            POT=POT+(DIST2-RADIUS2)**2
c        ENDIF
        DUMMYX=0.0D0
        DUMMYY=0.0D0
        DUMMYZ=0.0D0
        DO J2=1,atom_num
          J4=3*J2
          IF (J2.NE.J1)THEN
            DIST=(XINIB(J3-2)-XINIB(J4-2))**2+
     &(XINIB(J3-1)-XINIB(J4-1))**2+(XINIB(J3)-XINIB(J4))**2
            DIST=DSQRT(DIST)
C            write(*,*) "DIST=",DIST
            GRAD_PHI(J1,J2)=(-1.D0*eam_epsilon1)*eam_n1*
     &(eam_a1**eam_n1)*((DIST)**(-1.D0*eam_n1-2.D0))
            SUMP=0.0D0
            DO J5=1,atom_num
              J6=3*J5
              IF(J5.NE.J1)THEN
                DISTP=(XINIB(J3-2)-XINIB(J6-2))**2+
     &(XINIB(J3-1)-XINIB(J6-1))**2+(XINIB(J3)-XINIB(J6))**2
                DISTP=DSQRT(DISTP)
C                write(*,*) "DISTP=",DISTP
                P_DUMMY=(1.D0/DISTP)**eam_m1
                SUMP=SUMP+P_DUMMY
              ENDIF
            ENDDO
            SUMQ=0.0D0
            DO J5=1,atom_num
              J6=3*J5
              IF (J5.NE.J2)THEN
                DISTQ=(XINIB(J4-2)-XINIB(J6-2))**2+
     &(XINIB(J4-1)-XINIB(J6-1))**2+(XINIB(J4)-XINIB(J6))**2
                DISTQ=DSQRT(DISTQ)
C                write(*,*) "DISTQ=",DISTQ
                Q_DUMMY=(1.D0/DISTQ)**eam_m1
                SUMQ=SUMQ+Q_DUMMY
              ENDIF
            ENDDO
C            write(*,*) "SUMQ=",SUMQ,",SUMP=",SUMP
            GRAD_F(J1,J2)= (1.D0/2.D0)*eam_epsilon1*
     &eam_C1*(eam_a1**(eam_m1/2.D0))*eam_m1*
     &(DIST)**(-1.D0*eam_m1-2.D0)*((SUMP)**(-1.D0/2.D0)+(SUMQ)**
     &(-1.D0/2.D0))
            DUMMYX=
     &DUMMYX+(GRAD_PHI(J1,J2)+GRAD_F(J1,J2))*(XINIB(J3-2)-XINIB(J4-2))
            DUMMYY=
     &DUMMYY+(GRAD_PHI(J1,J2)+GRAD_F(J1,J2))*(XINIB(J3-1)-XINIB(J4-1))
            DUMMYZ=
     &DUMMYZ+(GRAD_PHI(J1,J2)+GRAD_F(J1,J2))*(XINIB(J3)-XINIB(J4))
          ENDIF
        ENDDO
        DIST2=(XINIB(J3-2)**2+XINIB(J3-1)**2+XINIB(J3)**2)
        IF (DIST2.GT.RADIUS2) THEN
          DUMMYX=DUMMYX+restore_fac*16.D0*(DIST2-RADIUS2)*XINIB(J3-2)
          DUMMYY=DUMMYY+restore_fac*16.D0*(DIST2-RADIUS2)*XINIB(J3-1)
          DUMMYZ=DUMMYZ+restore_fac*16.D0*(DIST2-RADIUS2)*XINIB(J3)
        ENDIF
        GRAD(J3-2)=DUMMYX
        GRAD(J3-1)=DUMMYY
        GRAD(J3)=DUMMYZ
C        write(*,*) "grad",grad(j3-2),grad(j3-1),grad(j3)
        RMS=RMS+DUMMYX**2+DUMMYY**2+DUMMYZ**2
      ENDDO
      RMS=DSQRT(RMS/DBLE(NDIM))
C      DO J1=1,NDIM
C        GRAD(J1)=GRAD(J1)/RMS
C      ENDDO
C      pause
      RETURN
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
