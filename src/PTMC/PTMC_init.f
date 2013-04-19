*========================================================================
* File Name : PTMC_init.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年10月24日 (週日) 15時44分39秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine PTMC_init  !life_num,job_num have been known,then jump to the correspond parameters
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/ensemble.h"
      include "../../include/pes.h"
      include "../../include/PTMC/PTMC.h"
      include "../../include/step_move/step_move.h"
C==========Output results============================
      if(wscreen)then
        write(*,"(I5,1x,A10,1x,F7.2,1x,A11,1x,F7.2,1x,A6,1x,
     &F6.2,1x,A10,1x,F11.0,1x,A11,1x,F11.0,1x,A11,1x,F11.0)")
     &myid,
     &"init_temp=",init_temp,
     &"final_temp=",final_temp,
     &"dTemp=",delta_temp,
     &"init_loop=",init_loop,
     &"final_loop=",final_loop,
     &"break_loop=",break_loop
      endif
      evap_num=0
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
