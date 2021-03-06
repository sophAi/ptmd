*========================================================================
* File Name : anl_pure.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時03分27秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine read_pes_gupta_pure
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/PES/gp.h"
C   read p1,q1,epsilon1,zeta1,rzero1
      integer total_num
      character read_atom_name*4
      total_num=6
      ndim_fac=3
      file_name=file_path(:index(file_path," ")-1)//pes_file
      open(20,file=file_name,status="old")
      do I0=1,pes_line_num-1
        read(20,*)
      enddo
      read(20,*) read_flag(1),read_int(1),read_flag(2),read_atom_name,
     &(read_real8(I0),I0=1,total_num)
      close(20)
      epsilon1=read_real8(1)
      zeta1=read_real8(2)
      p1=read_real8(3)
      q1=read_real8(4)
      rzero1=read_real8(5)
      mass_a=read_real8(6)
      epsilon2=0.D0
      zeta2=0.D0
      p2=0.D0
      q2=0.D0
      rzero2=0.D0
      epsilon3=0.D0
      zeta3=0.D0
      p3=0.D0
      q3=0.D0
      rzero3=0.D0
      atom_name_b=atom_name_a
      mass_b=mass_a
      do I1=1,atom_num
        mass(I1)=mass_a
        atom_name(I1)=atom_name_a
        do I2=1,atom_num
          bond_cutoff(I1,I2)=rzero1*bond_cutoff_ratio
        enddo
      enddo
      FAC_GRAD_ATT=ZETA1*(-2.D0)*Q1
      FAC_GRAD_REP=EPSILON1*(-1.D0)*P1
      rcut_unit=3.D0*dble(atom_num)**(1.D0/3.D0)
      call fix_reduced_unit(rzero1)
      call centre
      call pes
      call lbfgs_init
      call lbfgs_min
      global_pot=pot
      mean_global_pot=pot/dble(atom_num)
      return
      end

**************************************************
*  Energy and Gradient for Many-Body Potential.  *
**************************************************
      function pes_gupta_pure_function
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      include "../include/PES/gp.h"
      include "../include/tools/anl.h"
      real*8 pes_gupta_pure_function
      real*8 ATT(ndim_max),REP(ndim_max),ATT_DUMMY,REP_DUMMY
      real*8 POT_REP,POT_ATT,FAC_REP,FAC_ATT
      real*8 POT_REPI,POT_ATTI,GRAD_ATTM(ndim_max,ndim_max)
      real*8 DIST,RDIST,RDIST2,GRAD_REPM(ndim_max,ndim_max)
      real*8 DUMMYX,DUMMYY,DUMMYZ,XMUL2,XMUL3,GRAD_ATT(ndim_max)
      pes_gupta_pure_function=0.0D0
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
        GRAD_REPM(J1,J1)=0.0D0
        GRAD_ATTM(J1,J1)=0.0D0
        anl_num(J1)=0
        DO J2=1,atom_num
          J4=3*J2
          IF(J2.NE.J1)THEN
            DIST=DSQRT((x(J3-2)-x(J4-2))**2+
     &(x(J3-1)-x(J4-1))**2+(x(J3)-x(J4))**2)
            if(DIST.le.bond_cutoff(J1,J2))then
              anl_num(J1)=anl_num(J1)+1
              anl_neighbor(J1,anl_num(J1))=J2
C              write(*,*) J1,J2,bond_cutoff(J1,J2),DIST
            endif
            RDIST=DIST/RZERO1
            RDIST2=DIST*RZERO1
            if(RDIST.gt.rcut_unit)then
              evap_num=evap_num+1
C              evap=.true.
C              return
            endif
            ATT_DUMMY=DEXP(2.D0*Q1*(1.D0-RDIST))
            REP_DUMMY=DEXP(P1*(1.D0-RDIST))
            ATT(J1)=ATT(J1)+ATT_DUMMY
            REP(J1)=REP(J1)+REP_DUMMY
            POT_ATT=POT_ATT+ATT_DUMMY
            POT_REP=POT_REP+REP_DUMMY
            GRAD_REPM(J1,J2)=FAC_GRAD_REP*REP_DUMMY/RDIST2
            GRAD_ATTM(J1,J2)=FAC_GRAD_ATT*ATT_DUMMY/RDIST2
            if(RDIST.LE.1.2D0)bond_num(J1)=bond_num(J1)+1.D0
            if(RDIST.GT.rcut_unit)GRAD_REPM(J1,J2)=
     &GRAD_REPM(J1,J2)+EPSILON1*(-P1/RZERO1)*
     &3.D0*(rcut_unit/RDIST-1.D0)
          ENDIF
        ENDDO
        GRAD_ATT(J1)=DSQRT(POT_ATT)
        POT_REPI=POT_REPI+POT_REP
        POT_ATTI=POT_ATTI+GRAD_ATT(J1)
        pot_per_atom(J1)=(EPSILON1*REP(J1))-(ZETA1*DSQRT(ATT(J1)))
        pes_gupta_pure_function=pes_gupta_pure_function+pot_per_atom(J1)
      ENDDO
      DO J1=1,atom_num
        J3=J1*3
        DUMMYX=0.0D0
        DUMMYY=0.0D0
        DUMMYZ=0.0D0
        DO J2=1,atom_num
          J4=J2*3
          XMUL2=GRAD_REPM(J1,J2)-(GRAD_ATTM(J1,J2)/(2.D0*GRAD_ATT(J1)))
          XMUL3=GRAD_REPM(J2,J1)-(GRAD_ATTM(J2,J1)/(2.D0*GRAD_ATT(J2)))
          DUMMYX=DUMMYX+(XMUL2+XMUL3)*(X(J3-2)-X(J4-2))
          DUMMYY=DUMMYY+(XMUL2+XMUL3)*(X(J3-1)-X(J4-1))
          DUMMYZ=DUMMYZ+(XMUL2+XMUL3)*(X(J3)  -X(J4))
        ENDDO
        GRAD(J3-2)=DUMMYX
        GRAD(J3-1)=DUMMYY
        GRAD(J3)=DUMMYZ
C        write(*,*) J3,",xinib=",xinib(J3),",GRAD=",GRAD(J3)
        RMS=RMS+DUMMYX**2+DUMMYY**2+DUMMYZ**2
      ENDDO
      RMS=DSQRT(RMS/DBLE(NDIM))
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
