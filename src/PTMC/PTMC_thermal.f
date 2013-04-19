*========================================================================
* File Name : PTMC_thermal.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時56分52秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine PTMC_thermal_init
      implicit none 
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/PTMC/PTMC.h"
      kb=read_unit("Boltzmann_const ","kb ","evKelven^-1 ")   !in ev/K
      PTMC_ave_pot=0.D0
      PTMC_ave_pot_square=0.D0
      PTMC_ave_kinetic=(3.D0/2.D0)*dble(atom_num)*(kb*temp)
      PTMC_ave_energy=0.D0
      PTMC_ave_energy_square=0.D0
      PTMC_Cv=0.D0
      return
      end

      subroutine PTMC_sum_pot
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/PTMC/PTMC.h"
C     The results for doing total energy and potential are the same!
C     It is because the kinetic energy is always the same in MC
C     However, for MD it is better doing total energy instead of potential because the kinetic energy is fluctutating.
C     We can modified the zero point for the potential but it won't effect the energy fluctuation, say, heat capacity.
C     In MC, the potential fluctuation is exactly the same with the total energy fluctuation.
C     In MD, the total energy fluctuation is slightly different with the potential fluctuation due to the fluctuation of the kinetic energy. 
      PTMC_ave_pot=PTMC_ave_pot+pot
C      PTMC_ave_energy_square=PTMC_ave_energy_square+
C     &(pot)**2
C     &(pot+PTMC_ave_kinetic)**2
      PTMC_ave_pot_square=PTMC_ave_pot_square+pot**2
      return
      end
     

      subroutine PTMC_thermal
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/PTMC/PTMC.h"
      include "../../include/step_move/step_move.h"
      PTMC_ave_pot=PTMC_ave_pot/dble(global_accept_steps)
      PTMC_ave_pot_square=PTMC_ave_pot_square/dble(global_accept_steps)
      PTMC_Cv=(PTMC_ave_pot_square-PTMC_ave_pot**2)
     &/(((temp*kb)**2)*dble(atom_num))   ! The result is all the same for the total energy fluctuation
      if(wscreen)write(*,"(I5,1x,A20,1x,I13,1x,A6,1xF5.0,1x,A8,
     &1x,F15.7,1x,A5,1x,F15.7
     &,1x,A7,1x,F15.7)")
     &myid,"RESULTS: Total loop=",global_accept_steps,",temp=",temp,
     &",Cv/Nkb=",PTMC_Cv,",eng=",PTMC_ave_pot,
     &",eng^2=",PTMC_ave_pot_square
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
