*========================================================================
* File Name : simulation_init.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2011年03月24日 (週四) 09時45分05秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine simulation_init  !life_num,job_num have been known,then jump to the correspond parameters
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/life.h"
      include "../include/job.h"
      include "../include/ensemble.h"
      include "../include/pes.h"
      include "../include/step_move/step_move.h"
      include "../include/simulation.h"
      include "../include/BIMD/BIMD.h"
      include "../include/PTMC/PTMC.h"
      integer par_num1,par_num2,par_num3,par_num_all
      file_name=file_path(:index(file_path," ")-1)//job_file
C========locate parameters====================
      call wait_till_file_close(file_name)
      open(20,file=file_name,status="old")
      do I0=1,job_line_num-1
        read(20,*)
      enddo
      read(20,*) atom_num,par_num1,
     &(read_flag(I0),read_int(I0),I0=1,par_num1)
      read(20,*) par_num2,par_num3
     &,(read_flag(par_num1+I0),read_real8(par_num1+I0),I0=1,par_num2)
     &,(read_flag(par_num1+par_num2+I1),read_char(par_num1+par_num2+I1),
     &I1=1,par_num3)
      close(20)
      par_num_all=par_num1+par_num2+par_num3
      do I0=1,par_num_all
        if(read_flag(I0).eq."ensemble_num=")ensemble_num=read_int(I0)
        if(read_flag(I0).eq."loop=")loop_num=read_int(I0)
        if(read_flag(I0).eq."pes_id=")pes_id=read_int(I0)
        if(read_flag(I0).eq."ndim_fac=")ndim_fac=dint(read_real8(I0))
        if(read_flag(I0).eq."init_temp=")init_temp=read_real8(I0)
        if(read_flag(I0).eq."final_temp=")final_temp=read_real8(I0)
        if(read_flag(I0).eq."delta_temp=")delta_temp=read_real8(I0)
        if(read_flag(I0).eq."init_loop=")init_loop=read_real8(I0)
        if(read_flag(I0).eq."final_loop=")final_loop=read_real8(I0)
        if(read_flag(I0).eq."break_loop=")break_loop=read_real8(I0)
        if(read_flag(I0).eq."delta_time=")simulation_delta_time=
     &read_real8(I0)
        if(read_flag(I0).eq."reset_thermal=")
     &simulation_reset_thermal=dint(read_real8(I0))
        if(read_flag(I0).eq."min_method=")then
          simulation_min_method=dint(read_real8(I0))
          min_PES_method=simulation_min_method
        endif
        if(read_flag(I0).eq."PTMC_restore_method=")PTMC_restore_method=
     &dint(read_real8(I0))
        if(read_flag(I0).eq."PTMC_acceptance_ratio=")
     &PTMC_acceptance_ratio=read_real8(I0)
        if(read_flag(I0).eq."PTMC_fix_method=")PTMC_fix_method=
     &dint(read_real8(I0))
        if(read_flag(I0).eq."PTMC_step_ratio=")PTMC_step_ratio=
     &read_real8(I0)
        if(read_flag(I0).eq."mva=")mva=read_real8(I0)
        if(read_flag(I0).eq."mvb=")mvb=read_real8(I0)
        if(read_flag(I0).eq."mvc=")mvc=read_real8(I0)
        if(read_flag(I0).eq."mvd=")mvd=read_real8(I0)
        if(read_flag(I0).eq."mve=")mve=read_real8(I0)
        if(read_flag(I0).eq."mvf=")mvf=read_real8(I0)
        if(read_flag(I0).eq."mvg=")mvg=read_real8(I0)
        if(read_flag(I0).eq."mvh=")mvh=read_real8(I0)
        if(read_flag(I0).eq."mvi=")mvi=read_real8(I0)
        if(read_flag(I0).eq."simulation_rec_loop=")
     &simulation_rec_loop=read_real8(I0)
        if(read_flag(I0).eq."simulation_xyz_loop=")
     &simulation_xyz_loop=read_real8(I0)
        if(read_flag(I0).eq."simulation_ufe_loop=")
     &simulation_ufe_loop=read_real8(I0)
        if(read_flag(I0).eq."simulation_ufx_loop=")
     &simulation_ufx_loop=read_real8(I0)
        if(read_flag(I0).eq."simulation_ufv_loop=")
     &simulation_ufv_loop=read_real8(I0)
        if(read_flag(I0).eq."simulation_ufg_loop=")
     &simulation_ufg_loop=read_real8(I0)
        if(read_flag(I0).eq."simulation_vfx_loop=")
     &simulation_vfx_loop=read_real8(I0)
        if(read_flag(I0).eq."simulation_min_method=")
     &simulation_min_method=dint(read_real8(I0))
      enddo
      call read_xyz_top_file(file_name,job_line_num)
      call periodic_mass
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
