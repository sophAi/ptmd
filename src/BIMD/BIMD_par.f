*========================================================================
* File Name : BIMD_par.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Wed 03 Nov 2010 04:47:48 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine BIMD_par
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/simulation.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/BIMD/BIMD_restore.h"
      real*8 wd,t0,ave_mass,vd
      a1 = simulation_delta_time*8.0D0/3.0D0
      a2 = 112.0D0/121.0D0
      a3 = 1.0D0/8.0D0
      a4 = 9.0D0
      a5 = simulation_delta_time*3.0D0/8.0D0
      a6 = -9.0D0/121.0D0
      N1=top_frz_num
      N2=N1*2
      N3=N1*3
      N4=N1*4
      N5=N1*5
      N6=N1*6
      N7=N1*7
      N8=N1*8
      N9=N1*9
      N10=N1*10
C============================
      wd=60.D0
      c=read_unit("Speed_of_light ","c ","mHz ")          !in A/ps 1D10/1D12   !must left one space in each word!!
      c=c/1.D2
C======original debug======
C      c=2.99792458D6 
C==========================
      kb=read_unit("Boltzmann_const ","kb ","evKelven^-1 ")   !in ev/K
      atomic_mass=read_unit("Atomic_mass ","uc^2 ","ev ")     !in ev
      pi=read_unit("Pi ","Pi ","1 ")
C========original debug=====
C      pi=3.14D0
C===========================
      mass_a=(mass_a*atomic_mass)/(c**2)     !in ev/(A/ps)^2
      mass_b=(mass_b*atomic_mass)/(c**2)
C========original debug=====
C      mass_a=(mass_a*931.4812D9)/(c**2)
C      mass_b=(mass_b*931.4812D9)/(c**2)
C===========================
      tmass_a=1.D0/mass_a
      tmass_b=1.D0/mass_b
      ave_mass=0.D0
      do J0=1,atom_num
        mass(J0)=(mass(J0)*atomic_mass)/(c**2)
        tmass(J0)=1.D0/mass(J0)
        ave_mass=ave_mass+mass(J0)
      enddo
      t0=(2.D0*pi)/wd
      DL0=1.D0
      vd=wd/(2.D0*pi)
      DA0=1.D0
      I_N=1.D0/dble(N1)
      I_N_over_2=2.D0*I_N
      I_N_over_3=3.D0*I_N
      I_NT=I_N/(temp*kb)
      d0_over_NT=DA0*I_NT
      pseudo_factor=1.D0/(t0*dsqrt(2.D0*dble(atom_num)))
      do I0=1,top_frz_num
        J0=top_frz_id(I0)
C========original debug======
C        mass(J0)=(mass(J0)*931.4812D9)/(c**2)
C        write(*,*) mass(J0),mass_a,mass_b
C============================        
        DE0(I0)=mass(J0)*DL0**2*vd**2
        DP0(I0)=dsqrt(2.D0*mass(J0)*temp*kb)
        I_DP0(I0)=1.D0/DP0(I0)
        I_DP02(I0)=1.D0/(DP0(I0)**2)
        DP0_over_mass(I0)=DP0(I0)/mass(J0)
        I_DP0_mass(I0)=1.D0/(DP0(I0)*mass(J0))
        I_DP02_mass(I0)=1.D0/((DP0(I0)**2)*mass(J0))
        alpha(I0)=dsqrt((mass(J0)*DL0**2)/
     &(dble(top_frz_num)*t0**2*temp*kb))   !check here atom_num or top_frz_num
        beta(I0)=alpha(I0)
        chi(I0)=alpha(I0)
        p_factor(I0)=-alpha(I0)*I_N*DE0(I0)/DL0
      enddo
      ave_mass=ave_mass/dble(atom_num)
c      multiply=1D0    !enlarge low value
c      mem1=1.D0*multiply
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
