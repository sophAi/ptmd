*========================================================================
* File Name : BIMD_thermal.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Wed 04 May 2011 10:52:46 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine BIMD_thermal_init
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/BIMD/BIMD_restore.h"
      BIMD_ave_energy=0.D0
      BIMD_ave_energy_square=0.D0
      BIMD_ave_pot=0.D0
      BIMD_ave_kinetic=0.D0
      BIMD_ave_temper=0.D0
      do I0=1,atom_num
        do I1=1,atom_num
          BIMD_ave_dist(I0,I1)=0.D0
          BIMD_ave_dist2(I0,I1)=0.D0
        enddo
      enddo
      return
      end


      subroutine BIMD_thermal
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/simulation.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/BIMD/BIMD_restore.h"
      include "../../include/pes.h"
      integer inverse_top_frz_id(atom_num_max)
      real*8 total_i_bond(atom_num_max),
     &total_bond,bond(atom_num_max,atom_num_max)
      total_loop=final_loop-start_loop+1.D0
      BIMD_ave_temper=BIMD_ave_temper/total_loop
      BIMD_ave_energy=BIMD_ave_energy/total_loop
      BIMD_ave_pot=BIMD_ave_pot/total_loop
      BIMD_ave_kinetic=BIMD_ave_kinetic/total_loop
      BIMD_ave_energy_square=BIMD_ave_energy_square/total_loop
      BIMD_Cv=(BIMD_ave_energy_square-BIMD_ave_energy**2)
     &/(dble(N1)*(temp*kb)**2)  !This Cv is in 1/K*atom unit,to use ev/K*atom,multiply by kb
      do J1=1,atom_num
        do J3=1,atom_num
          if(J3.ne.J1)then
            BIMD_ave_dist(J1,J3)=BIMD_ave_dist (J1,J3)/total_loop
            BIMD_ave_dist2(J1,J3)=BIMD_ave_dist2(J1,J3)/total_loop
          endif
        enddo
      enddo
      total_bond=0.D0
      do J1=1,atom_num
        total_i_bond(J1)=0.D0
        do J3=1,atom_num
          if (J3.ne.J1.and.(top_frz_io(J1).eq.1
     &.or.top_frz_io(J3).eq.1))then
            bond(J1,J3)=dsqrt(BIMD_ave_dist2(J1,J3)-
     &BIMD_ave_dist(J1,J3)**2)
            bond(J1,J3)= bond(J1,J3)/BIMD_ave_dist(J1,J3)
            total_i_bond(J1)=total_i_bond(J1)+bond(J1,J3)
          endif
        enddo
        total_bond=total_bond+total_i_bond(J1)
      enddo
      BIMD_ave_bond=total_bond*(1.D0/dble(atom_num*(atom_num-1)))
      if(wscreen)write(*,"(I5,1x,A20,1x,F13.1,1x,A6,1xF5.0,1x,A8,
     &1x,F15.7,1x,A5,1x,F15.7
     &,1x,A7,1x,F15.7,1x,A6,1x,F15.7)")
     &myid,"RESULTS: Total loop=",total_loop,",temp=",temp,
     &",Cv/Nkb=",BIMD_Cv,",eng=",BIMD_ave_energy,
     &",eng^2=",BIMD_ave_energy_square,",bond=",BIMD_ave_bond
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
