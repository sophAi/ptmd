*========================================================================
* File Name : pes_qscff.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時55分29秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
*******************************************************************************
*Energy and Gradient for Q-SC Many Body Force-field Potential By Po-Jen Hsu   *
*******************************************************************************
      SUBROUTINE QSCFF(NDIM,POT,XINIB)
      IMPLICIT NONE
      INCLUDE "../../include/global_common.h"
      INCLUDE "../../include/common.h"
      INCLUDE "../../include/mbh_ndim.h"
      REAL*8 Dij,Dij2,nij,mij,aij,cij
     &,ATT(1000),REP(1000),ATT_DUMMY,REP_DUMMY
     &,POT_REP,POT_ATT
     &,POT_REPI,POT_ATTI
     &,DIST,DIST2
     &,GRAD_ATTM(1000,1000),GRAD_REPM(1000,1000)
     &,DUMMYX,DUMMYY,DUMMYZ,XMUL2,XMUL3,
     &GRAD_ATT(1000)
      EVAP=.FALSE.
      POT=0.0D0
      POT_REPI=0.0D0
      POT_ATTI=0.0D0
      RMS=0.D0
C      IF (atom_num_a.EQ.ATOM_NUM)THEN
C        RADIUS=2.D0*aii
C      ELSE
C        RADIUS=2.D0*aij0
C      ENDIF
      DO J1=1,ATOM_NUM
        J3=3*J1
        bond_num(J1)=1.D0
        IF (J1.LE.atom_num_a)THEN
          Cij=Ci
          Dij2=Dii
        ELSE
          Cij=Cj
          Dij2=Djj
        ENDIF
        DIST2=XINIB(J3-2)**2+XINIB(J3-1)**2+XINIB(J3)**2
C        IF (DSQRT(DIST2).GT.DMAX) DMAX=DSQRT(DIST)
        IF (DIST2.GT.RADIUS2) THEN
          EVAP=.TRUE.
C          WRITE(*,*) "EVAP",DIST2,RADIUS2
          POT=POT+restore_fac*((DIST2-RADIUS2))**2
        ENDIF
        POT_REP=0.0D0
        POT_ATT=0.0D0
        REP(J1)=0.0D0
        ATT(J1)=0.0D0
        GRAD_ATT(J1)=0.D0
        GRAD_REPM(J1,J1)=0.D0
        GRAD_ATTM(J1,J1)=0.D0
        DO J2=1,ATOM_NUM
          J4=3*J2
          IF(J2.NE.J1)THEN
            DIST=(XINIB(J3-2)-XINIB(J4-2))**2+
     &(XINIB(J3-1)-XINIB(J4-1))**2+(XINIB(J3)-XINIB(J4))**2
            DIST=DSQRT(DIST)
            IF ((J1.LE.atom_num_a).AND.(J2.LE.atom_num_a))THEN
              Dij=Dii
              nij=ni
              mij=mi
              aij=aii
            ELSE IF((J1.GT.atom_num_a).AND.(J2.GT.atom_num_a))THEN
              Dij=Djj
              nij=nj
              mij=mj
              aij=ajj
            ELSE
              Dij=Dij0
              nij=nij0
              mij=mij0
              aij=aij0
            ENDIF
C            write(*,*) Dij,cij,mij,nij,aij,DIST
            ATT_DUMMY=(aij/DIST)**mij
            REP_DUMMY=(aij/DIST)**nij
            ATT(J1)=ATT(J1)+ATT_DUMMY
            REP(J1)=REP(J1)+Dij*REP_DUMMY/2.D0
            POT_ATT=POT_ATT+ATT_DUMMY
            POT_REP=POT_REP+Dij*REP_DUMMY/2.D0
            GRAD_REPM(J1,J2)=(-1.D0/2.D0)*nij*Dij*
     &REP_DUMMY/(DIST**2.D0)
            GRAD_ATTM(J1,J2)=(1.D0/2.D0)*mij*Cij*Dij2*
     &ATT_DUMMY/(DIST**2.D0)
            IF(DIST.LE.aij*1.2D0)bond_num(J1)=bond_num(J1)+1.D0
          ENDIF
        ENDDO
        GRAD_ATT(J1)=DSQRT(POT_ATT)
        POT_REPI=POT_REPI+POT_REP
        POT_ATTI=POT_ATTI+GRAD_ATT(J1)*Cij*Dij2
        VT(J1)=REP(J1)-Cij*Dij2*DSQRT(ATT(J1))
      ENDDO
      POT=(POT_REPI)-(POT_ATTI)
C      write(*,*) "POT=",POT,POT_REPI,POT_ATTI
C      pause
      DO J1=1,ATOM_NUM
        J3=J1*3
        DUMMYX=0.0D0
        DUMMYY=0.0D0
        DUMMYZ=0.0D0
        DO J2=1,ATOM_NUM
          J4=J2*3
          XMUL2=GRAD_REPM(J1,J2)+(GRAD_ATTM(J1,J2)/GRAD_ATT(J1))
          XMUL3=GRAD_REPM(J2,J1)+(GRAD_ATTM(J2,J1)/GRAD_ATT(J2))
          DUMMYX=DUMMYX+(XMUL2+XMUL3)*(XINIB(J3-2)-XINIB(J4-2))
          DUMMYY=DUMMYY+(XMUL2+XMUL3)*(XINIB(J3-1)-XINIB(J4-1))
          DUMMYZ=DUMMYZ+(XMUL2+XMUL3)*(XINIB(J3)  -XINIB(J4))
        ENDDO   
        DIST2=XINIB(J3-2)**2+XINIB(J3-1)**2+XINIB(J3)**2
        IF (DIST2.GT.RADIUS2) THEN
          DUMMYX=DUMMYX+restore_fac*16.D0*((DIST2-RADIUS2))*XINIB(J3-2)
          DUMMYY=DUMMYY+restore_fac*16.D0*((DIST2-RADIUS2))*XINIB(J3-1) 
          DUMMYZ=DUMMYZ+restore_fac*16.D0*((DIST2-RADIUS2))*XINIB(J3)
        ENDIF  
        GRAD(J3-2)=DUMMYX
        GRAD(J3-1)=DUMMYY
        GRAD(J3)=DUMMYZ
        RMS=RMS+DUMMYX**2+DUMMYY**2+DUMMYZ**2
      ENDDO
      RMS=DSQRT(RMS/DBLE(NDIM))
C      DO J1=1,NDIM
C        GRAD(J1)=GRAD(J1)/RMS
C      ENDDO
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
