*========================================================================
* File Name : BIMD_init.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Wed 16 Feb 2011 05:10:21 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine BIMD_init  !life_num,job_num have been known,then jump to the correspond parameters
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/ensemble.h"
      include "../../include/pes.h"
      include "../../include/simulation.h"
      include "../../include/BIMD/BIMD.h"
C==========Output results============================
      if(wscreen)then
        write(*,"(I5,1x,A10,1x,F7.2,1x,A11,1x,F7.2,1x,A6,1x,
     &F8.2,1x,A10,1x,F11.0,1x,A11,1x,F11.0,1x,A6,1x,F10.7)")
     &myid,
     &"Init_temp=",init_temp,
     &"final_temp=",final_temp,
     &"dTemp=",delta_temp,
     &"init_loop=",init_loop,
     &"final_loop=",final_loop,
     &"dTime=",simulation_delta_time
        write(*,"(I5,7(1x,A9,1x,F13.1))")
     &myid,
     &"rec_loop=",simulation_rec_loop,
     &"xyz_loop=",simulation_xyz_loop,
     &"ufe_loop=",simulation_ufe_loop,
     &"ufx_loop=",simulation_ufx_loop,
     &"ufv_loop=",simulation_ufv_loop,
     &"ufg_loop=",simulation_ufg_loop,
     &"vfx_loop=",simulation_vfx_loop
      endif
      evap_num=0
      delta_time=simulation_delta_time
      q_dim=top_frz_num*6+9
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
