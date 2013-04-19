*========================================================================
* File Name : PTMC.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Fri 25 Feb 2011 01:08:07 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine PTMC
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/PTMC/PTMC.h"
      include "../../include/step_move/step_move.h"
      include "../../include/pes.h"
      integer break_loop_int,interval_loop_int
      integer break_init,break_final
      real*8 x_initial(ndim_max),grad_initial(ndim_max),pot_initial
      real*8 acceptance_factor
      parameter(acceptance_factor=0.05D0)
      integer premc_break_steps
      parameter(premc_break_steps=100)
      init_loop_int=dint(init_loop)
      final_loop_int=dint(final_loop)
      break_loop_int=dint(break_loop)
      interval_loop_int=dint((final_loop-init_loop+1.D0)/break_loop)
      min_PES_method=5  !no minimum
      MC_acceptance_ratio=1.D0
      call centre
      call min_pes
C=========Preserve initial configuration===========
      pot_initial=pot
      do I1=1,ndim
        x_initial(I1)=x(I1)
        grad_initial(I1)=grad(I1)
      enddo
C=========Initial pre-MC to find out the suitable moving length=======
      if(wscreen)write(*,"(I5,1x,A36,1x,F13.8)")
     &myid,"Starting pre-PTMC,acceptance factor=",PTMC_acceptance_ratio*
     &acceptance_factor
      call PTMC_store_pes_prev
      do while(MC_acceptance_ratio.ge.
     &(PTMC_acceptance_ratio+PTMC_acceptance_ratio*acceptance_factor)
     &.or.MC_acceptance_ratio.le.
     &(PTMC_acceptance_ratio-PTMC_acceptance_ratio*acceptance_factor))
        do I2=1,premc_break_steps
          call step_move
          call min_pes
          call PTMC_check_transition
        enddo
        call PTMC_check_acceptance_ratio
      enddo
      if(wscreen)write(*,"(I5,1x,A20,F14.8)")
     &myid,"The moving length is",moving_length
C========Reset the initial configuration and steps===
      global_accept_steps=0
      global_reject_steps=0
      global_total_steps=0
      pot=pot_initial
      do I1=1,ndim
        x(I1)=x_initial(I1)
        grad(I1)=grad_initial(I1)
      enddo
      call PTMC_thermal_init
C==================================
      call PTMC_store_pes_prev
      if(wscreen) write(*,"(I5,1x,A18,1x,I13,1x,A2,1x,
     &I13)")
     &myid,"Starting PTMC from",init_loop_int,"to",final_loop_int

C======Start Monte Carlo Simulation=========================
      do I1=1,interval_loop_int
        break_init=(I1-1)*break_loop_int+init_loop_int
        break_final=I1*break_loop_int
        do I2=break_init,break_final
C=================PTMC random move==================================================
          call step_move
          call min_pes
          call PTMC_check_transition          
        enddo
C===========End of Restoring Points or recording xyz================================
C===========Check Acceptance Ratio============================
        call PTMC_check_acceptance_ratio
      enddo  
      return
      end


      subroutine PTMC_check_acceptance_ratio
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/step_move/step_move.h"
      include "../../include/PTMC/PTMC.h"
      MC_acceptance_ratio=dble(accept_steps)/dble(total_steps)
      if(PTMC_fix_method.eq.1)then
        if(MC_acceptance_ratio.gt.PTMC_acceptance_ratio)then
          moving_length=moving_length*PTMC_step_ratio
        else
          moving_length=moving_length/PTMC_step_ratio
        endif
      endif
      if(PTMC_fix_method.eq.2)then
        if(MC_acceptance_ratio.gt.PTMC_acceptance_ratio) then
          temp=temp*PTMC_temp_ratio
        else
          temp=temp/PTMC_temp_ratio
        endif
      endif
      global_accept_steps=global_accept_steps+accept_steps
      global_reject_steps=global_reject_steps+reject_steps
      global_total_steps=global_total_steps+total_steps
      accept_steps=0
      reject_steps=0
      total_steps=0
      if(wscreen) write(*,"(I5,1x,A9,1x,I13,1x,A5,1x,F13.8,1x,A21,
     &1x,F5.3,1x,A6,1x,F7.2,1x,A15,1x,F13.8)")
     &myid,"MC steps=",global_total_steps,",pot=",pot_prev,
     &",MC acceptance ratio=",MC_acceptance_ratio,
     &",temp=",temp,",moving length=",moving_length
      return
      end



      subroutine PTMC_store_pes_prev
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/PTMC/PTMC.h"
      pot_prev=pot
      rms_prev=rms
      do I0=1,ndim
        x_prev(I0)=x(I0)
        grad_prev(I0)=grad(I0)
      enddo
      do I0=1,atom_num
        pot_per_atom_prev(I0)=pot_per_atom(I0)
        bond_num_prev(I0)=bond_num(I0)
      enddo
      return
      end

      subroutine PTMC_restore_pes
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/PTMC/PTMC.h"
      do I0=1,ndim
        x(I0)=x_prev(I0)
      enddo
      return
      end


      subroutine PTMC_check_transition
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/step_move/step_move.h"
      include "../../include/PTMC/PTMC.h"
C      real*8 delta_pot
C      delta_pot=transition_func()
      real*8 tran_eng
      tran_eng=(pot_prev-pot)/(temp*kb)
C      write(*,*)"exp(tran_eng)=",dexp(tran_eng),pot_prev,pot,rnd()
      if(dexp(tran_eng).gt.rnd().or.tran_eng.gt.0.D0)then
        call PTMC_store_pes_prev
        call PTMC_sum_pot     
        accept_steps=accept_steps+1
      else
        reject_steps=reject_steps+1
      endif
      total_steps=total_steps+1
      return
      end

      function transition_func()
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/step_move/step_move.h"
      include "../../include/PTMC/PTMC.h"
      transition_func=(pot_prev-pot)/(temp*kb)
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
