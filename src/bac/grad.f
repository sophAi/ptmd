C
C  Energy and gradient for Many-Body Potential.
C
      SUBROUTINE agrad
      IMPLICIT NONE
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/PES/gp.h"
      DOUBLE PRECISION ZETA,ESILON,P,Q,RZERO,ATTIN,REPIN
      DOUBLE PRECISION ATT_DUMMY,REP_DUMMY
      DOUBLE PRECISION GRAD_ATT(atom_num_max),ELJ_ATT,XMUL3
      DOUBLE PRECISION dist1
      DOUBLE PRECISION GRAD_ATTM(atom_num_max,atom_num_max),
     &GRAD_REPM(atom_num_max,atom_num_max),rcut1
      DOUBLE PRECISION ELJ, DUMMYX, DUMMYY, DUMMYZ, XMUL2,DUMMY
      DO J1=1,atom_num
        J3=3*J1
        GRAD_REPM(J1,J1)=0.0D0
        GRAD_ATTM(J1,J1)=0.0D0
        ELJ_ATT=0.0D0
        DO J2=1,atom_num
          J4=3*J2
          IF(J1.NE.J2)THEN
            dist(J1,J2)=(X(J3-2)-X(J4-2))**2+(X(J3-1)-X(J4-1))**2
     & +(X(J3)-X(J4))**2
            dist(J1,J2)=DSQRT(dist(J1,J2))
            IF((J1.le.atom_num_a).and.(J2.le.atom_num_a)) THEN
              P=P1
              Q=Q1
              ESILON=EPSILON1
              ZETA=ZETA1
              RZERO=RZERO1
            ELSE IF((J1.gt.atom_num_a).and.(J2.gt.atom_num_a))THEN
              P=P2
              Q=Q2
              ESILON=EPSILON2
              ZETA=ZETA2
              RZERO=RZERO2
            ELSE
              P=P3
              Q=Q3
              ESILON=EPSILON3
              ZETA=ZETA3
              RZERO=RZERO3
            ENDIF
            dist1=dist(J1,J2)/rzero
            rcut1=3D0*rzero*dble(atom_num)**(1D0/3D0)
            GRAD_REPM(J1,J2)=ESILON*(-P/rzero)*DEXP(P*(1.D0-DIST1))
C            rcut=rcut_unit*rzero
cccccccccccccccccccccccc 
            if (dist(J1,J2).gt.rcut1 ) then 
              grad_repm(j1,j2)=grad_repm(j1,j2)
     &+esilon*(-p/rzero)*3D0*(rcut1-dist(J1,J2))
            end if
ccccccccccccccccccccccccccc
            grad_repm(j1,j2)=grad_repm(j1,j2)/DIST(J1,J2)
            GRAD_ATTM(J1,J2)=(ZETA**2)*(-2.D0*Q/rzero)*
     &DEXP(2.D0*Q*(1.D0-DIST1))/DIST(J1,J2)
            ELJ_ATT=ELJ_ATT+GRAD_ATTM(J1,J2)*((DIST(J1,J2))/
     &(-2.D0*Q/rzero))
          ENDIF
        ENDDO
        GRAD_ATT(J1)=DSQRT(ELJ_ATT)
      ENDDO
      DO J1=1,atom_num
        J3=3*J1
        DUMMYX=0.0D0
        DUMMYY=0.0D0
        DUMMYZ=0.0D0
        DO J2=1,atom_num
          J4=3*J2
          XMUL2=GRAD_REPM(J1,J2)-(GRAD_ATTM(J1,J2)/
     &(2.D0*GRAD_ATT(J1)))
          XMUL3=GRAD_REPM(J2,J1)-(GRAD_ATTM(J2,J1)/
     &(2.D0*GRAD_ATT(J2)))
          DUMMYX=DUMMYX+(XMUL2+XMUL3)*(X(J3-2)-X(J4-2))
          DUMMYY=DUMMYY+(XMUL2+XMUL3)*(X(J3-1)-X(J4-1))
          DUMMYZ=DUMMYZ+(XMUL2+XMUL3)*(X(J3)  -X(J4))
        ENDDO
        GRAD(J3-2)=DUMMYX
        GRAD(J3-1)=DUMMYY
        GRAD(J3)=DUMMYZ
      ENDDO
      RETURN
      END
