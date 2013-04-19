*========================================================================
* File Name : pes_gp_alloy_BIMD.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時54分36秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine read_pes_gupta_alloy
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/PES/gp.h"
C   read p1,q1,epsilon1,zeta1,rzero1
      integer total_num
      real*8 rzero_max
      character read_atom_name*4
      total_num=6
      ndim_fac=3
      file_name=file_path(:index(file_path," ")-1)//pes_file
      open(20,file=file_name,status="old")
      do I0=1,pes_line_num-1
        read(20,*)
      enddo
      do I1=1,4   !maximum 4 rows
        read(20,*) read_flag(1),read_int(1),read_flag(2),read_atom_name,
     &(read_real8(I0),I0=1,total_num)
        if(read_atom_name.eq.atom_name_a)then
          epsilon1=read_real8(1)
          zeta1=read_real8(2)
          p1=read_real8(3)
          q1=read_real8(4)
          rzero1=read_real8(5)
          mass_a=read_real8(6)
          mass_b=mass_a
        else if(read_atom_name.eq.atom_name_b)then
          epsilon2=read_real8(1)
          zeta2=read_real8(2)
          p2=read_real8(3)
          q2=read_real8(4)
          rzero2=read_real8(5)
          mass_b=read_real8(6)
        else
          epsilon3=read_real8(1)
          zeta3=read_real8(2)
          p3=read_real8(3)
          q3=read_real8(4)
          rzero3=read_real8(5)
        endif
      enddo
      do J1=1,atom_num
        do J2=1,atom_num
          if(J1.le.atom_num_a.and.J2.le.atom_num_a)then
            epsilon_ab(J1,J2)=epsilon1
            zeta_ab(J1,J2)=zeta1
            p_ab(J1,J2)=p1
            q_ab(J1,J2)=q1
            rzero_ab(J1,J2)=rzero1
          else if(J1.gt.atom_num_a.and.J2.gt.atom_num_a)then
            epsilon_ab(J1,J2)=epsilon2
            zeta_ab(J1,J2)=zeta2
            p_ab(J1,J2)=p2
            q_ab(J1,J2)=q2
            rzero_ab(J1,J2)=rzero2
          else
            epsilon_ab(J1,J2)=epsilon3
            zeta_ab(J1,J2)=zeta3
            p_ab(J1,J2)=p3
            q_ab(J1,J2)=q3
            rzero_ab(J1,J2)=rzero3
          endif
        enddo
      enddo
      rzero_max=rzero1
      if(rzero_max.lt.rzero2)rzero_max=rzero2
      if(rzero_max.lt.rzero3)rzero_max=rzero3
      rcut_unit=3.D0*dble(atom_num)**(1.D0/3.D0)
      rcut=rcut_unit*rzero_max
      call fix_reduced_unit(rzero3)
      close(20)
      return
      end

*************************************************************************
*Energy and Gradient for Many-Body Alloy Potential By Po-Jen Hsu        *
*************************************************************************
      function pes_gupta_alloy_function !The restortion force has been moved to pes_restore_function
      IMPLICIT NONE
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/PES/gp.h"
      real*8 pes_gupta_alloy_function
      real*8 ATT(ndim_max),REP(ndim_max),ATT_DUMMY,REP_DUMMY
      real*8 POT_REP,POT_ATT,DIST,DIST2,DIST_R
      real*8 POT_REPI,POT_ATTI
      real*8 GRAD_ATTM(ndim_max,ndim_max),GRAD_REPM(ndim_max,ndim_max)
      real*8 DUMMYX,DUMMYY,DUMMYZ,XMUL2,XMUL3,GRAD_ATT(ndim_max)
      pes_gupta_alloy_function=0.0D0
      POT_REPI=0.0D0
      POT_ATTI=0.0D0
      RMS=0.D0
      DO J1=1,atom_num
        J3=3*J1
        bond_num(J1)=1.D0
        POT_REP=0.0D0
        POT_ATT=0.0D0
        REP(J1)=0.0D0
        ATT(J1)=0.0D0
        GRAD_REPM(J1,J1)=0.D0
        GRAD_ATTM(J1,J1)=0.D0
        DO J2=1,atom_num
          J4=3*J2
          IF(J2.NE.J1)THEN
            DIST2=((x(J3-2)-x(J4-2))**2+(x(J3-1)-
     &x(J4-1))**2+(x(J3)-x(J4))**2)
            DIST=DSQRT(DIST2)
            if(DIST.ge.rcut)then
              evap_num=evap_num+1
              return
            endif
            ATT_DUMMY=DEXP(2.D0*Q_AB(J1,J2)*
     &(1.D0-(DIST/RZERO_AB(J1,J2))))
            REP_DUMMY=DEXP(P_AB(J1,J2)*
     &(1.D0-(DIST/RZERO_AB(J1,J2))))
            ATT(J1)=ATT(J1)+(ZETA_AB(J1,J2)**2)*ATT_DUMMY
            REP(J1)=REP(J1)+EPSILON_AB(J1,J2)*REP_DUMMY
            POT_ATT=POT_ATT+(ZETA_AB(J1,J2)**2)*ATT_DUMMY
            POT_REP=POT_REP+EPSILON_AB(J1,J2)*REP_DUMMY
            GRAD_REPM(J1,J2)=EPSILON_AB(J1,J2)*(-1.D0)*P_AB(J1,J2)
     &*REP_DUMMY/(DIST*RZERO_AB(J1,J2))
            GRAD_ATTM(J1,J2)=(ZETA_AB(J1,J2)**2)*(-1.D0)*
     &Q_AB(J1,J2)*ATT_DUMMY/(DIST*RZERO_AB(J1,J2))
            if(DIST.LE.RZERO_AB(J1,J2)*1.2D0)
     &bond_num(J1)=bond_num(J1)+1.D0
C===============Only for evaperation in BIMD===============
            if(DIST.gt.rcut)GRAD_REPM(J1,J2)=
     &GRAD_REPM(J1,J2)+EPSILON_AB(J1,J2)*
     &(-P_AB(J1,J2)/RZERO_AB(J1,J2))*3.D0*(rcut-dist)    !Better be masked in other job
C================End=======================================
          ENDIF
        ENDDO
        GRAD_ATT(J1)=DSQRT(POT_ATT)
        POT_REPI=POT_REPI+POT_REP
        POT_ATTI=POT_ATTI+GRAD_ATT(J1)
        pot_per_atom(j1)=rep(j1)-dsqrt(att(j1))
      ENDDO
      pes_gupta_alloy_function=(POT_REPI)-(POT_ATTI)
C      if(pes_gupta_alloy_function.ge.9999999.D0)then
C        evap_num=evap_num+1
C        return
C      endif
      DO J1=1,atom_num
        J3=J1*3
        DUMMYX=0.0D0
        DUMMYY=0.0D0
        DUMMYZ=0.0D0
        DO J2=1,atom_num
          J4=J2*3
          XMUL2=GRAD_REPM(J1,J2)-(GRAD_ATTM(J1,J2)/GRAD_ATT(J1))
          XMUL3=GRAD_REPM(J2,J1)-(GRAD_ATTM(J2,J1)/GRAD_ATT(J2))
          DUMMYX=DUMMYX+(XMUL2+XMUL3)*(x(J3-2)-x(J4-2))
          DUMMYY=DUMMYY+(XMUL2+XMUL3)*(x(J3-1)-x(J4-1))
          DUMMYZ=DUMMYZ+(XMUL2+XMUL3)*(x(J3)  -x(J4))
        ENDDO   
        grad(J3-2)=DUMMYX
        grad(J3-1)=DUMMYY
        grad(J3)=DUMMYZ
        RMS=RMS+DUMMYX**2+DUMMYY**2+DUMMYZ**2
      ENDDO
      RMS=DSQRT(RMS/DBLE(ndim))
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
