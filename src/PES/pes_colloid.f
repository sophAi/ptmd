*========================================================================
* File Name : pes_colloid.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時54分04秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine read_pes_colloid
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/PES/colloid.h"
      integer total_um
      real*8 rzero_max
      character read_atom_name*4
      total_num=6
      ndim_fac=2
      file_name=file_path(:index(file_path," ")-1)//pes_file
      open(20,file=file_name,status="old")
      do I0=1,pes
      return
      end

******************************************************************
*     Energy and Gradient force for 2-D interfacial colloid      *
******************************************************************
      real*8 function pes_colloid_function
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/PES/colloid.h"
      real*8 dist,rroot
      real*8 grad_vdw,grad_cap,grap_dip,grad_screen
      real*8 t_vt(atom_num_max,atom_num_max),t_dip,t_cap,t_vdw,
     &t_screen,t_res,grad_length,radixradi,d_radi_1,d_radi_2
      real*8 dummyx,dummyy,g(atom_num_max,atom_num_max)
      e_dip=0.D0
      e_cap=0.D0
      e_vdw=0.D0
      e_screen=0.D0
      rms=0.D0
      do J1=1,ndim,ndim_fac
        J3=(J1+1)/ndim_fac
        pot_per_atom(J3)=0.D0
        bond_num(J3)=1.D0
        do J2=J1+ndim_fac,ndim,ndim_fac
          J4=(J2+1)/ndim_fac
          dist=(x(J1)-x(J2))**2+(x(J1+1)-x(J2+1))**2
          rroot=dsqrt(dist)
          if(rroot.le.rzero1*1.2D0)bond_num(J3)=bond_num(J3)+1.D0
          t_cap=col_q(J3,J4)*dlog(2.D0/landa/rroot)    !cap
          e_cap=e_cap+t_cap
          grad_cap=col_q(J3,J4)*(-0.5D0)/DIST
          t_dip=col_f(J3,J4)*col_k(J3,J4)/rroot**3     !dip
          e_dip=e_dip+t_dip
          grad_dip=-1.5D0*t_dip/dist
          if(rroot.ge.rmax)then
            radixradi=radi(J3)*radi(J4)                !vdw
            d_radi_1=dist-(radi(J3)+radi(J4))**2
            d_radi_2=d_radi_1+4.D0*radixradi
            t_vdw=col_p*(2.D0*radixradi*(1.D0/d_radi_1+1.D0/d_radi_2)
     &+dlog(d_radi_1/d_radi_2)
            e_vdw=e_vdw+t_vdw
            grad_vdw=-32.D0*col_p*radixradi*
     &(radixradi/(d_radi_1*d_radi_2))**2
            t_screen=col_f(J3,J4)*col_h/rroot*
     &dexp(-1.D0*col_kapa(J3,J4)*rroot)
            e_screen=e_screen+t_screen
            grad_screen=
     &-0.5D0*t_screen*(1.D0/dist+col_kapa(J3,J4)/rroot)
            t_vt(J3,J4)=t_cap+t_dip+t_screen+t_vdw
            pot_per_atom(J3)=pot_per_atom(J3)+t_vt(J3,J4)
            g(J3,J4)=grad_cap+grad_dip+grad_screen+grad_vdw
          else
            t_vt(J3,J4)=t_cap+t_dip
            pot_per_atom(J3)=pot_per_atom(J3)+t_vt(J3,J4)
            g(J3,J4)=grad_cap+grad_dip
          endif
          dummyx=dummyx+g(J3,J4)*(x(J1)-x(J2))
          dummyy=dummyy+g(J3,J4)*(x(J1+1)-x(J2+1))
        enddo        
        do J5=1,J3-1
          J6=J5*ndim_fac
          dummyx=dummyx+g(J5,J3)*(x(J1)-x(J6-1))
          dummyy=dummyy+g(J5,J3)*(x(J1+1)-x(J6))
          pot_per_atom(J3)=pot_per_atom(J3)+t_vt(J5,J3)
        enddo
        grad(J1)=dummyx
        grad(J1+1)=dummyy
        pot_per_atom(J3)=pot_per_atom(J3)/ndim_fac
        rms=rms+dummyx**2+dummyy**2
      enddo
      rms=dsqrt(rms/dble(ndim))
      do J1=1,ndim
        grad(J1)=grad(J1)/rms
      enddo
      pot=e_cap+e_dip+e_vdw+e_screen
      return
      end
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
