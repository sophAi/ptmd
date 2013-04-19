*========================================================================
* File Name : edit_PTMC.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Thu 18 Nov 2010 11:52:14 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_PTMC
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/file.h"
      include "../../include/job.h"
      include "../../include/ensemble.h"
      include "../../include/PTMC/PTMC.h"
      character yn*1,sel*2
      write(*,*) "<<<<PTMC MENU>>>>"
      call edit_temp
      sel="99"
      do while(sel.ne."0")
        call show_parameter(0,"quit ")
        call show_parameter(1,"init_loop ")
        call show_parameter(2,"final_loop ")
        call show_parameter(3,"break_loop ")
        call show_parameter(4,"PTMC_acceptance_ratio ")
        call show_parameter(5,"PTMC_step_ratio ")
        call show_parameter(6,"PTMC_temp_ratio ")
        call show_parameter(7,"PTMC_fix_method ")
        call show_parameter(8,"PTMC_restore_method ")
        write(*,*) " 9.Change the step moving ratio"
        write(*,*) "--------------------------------------------"
        read(*,*) sel
        if(sel.eq."1")call edit_parameter("init_loop ")
        if(sel.eq."2")call edit_parameter("final_loop ")
        if(sel.eq."3")call edit_parameter("break_loop ")
        if(sel.eq."4")call edit_parameter("PTMC_acceptance_ratio ")
        if(sel.eq."5")call edit_parameter("PTMC_step_ratio ")
        if(sel.eq."6")call edit_parameter("PTMC_temp_ratio ")
        if(sel.eq."7")then
          write(*,*) "Please select the fixing method:"
          write(*,*) "0.Fix all for each node"
          write(*,*) "1.Fix temperature for each node"
          write(*,*) "2.Fix moving length for each node"
          call edit_parameter("PTMC_fix_method ")
        endif
        if(sel.eq."8")then
          write(*,*) "0.Record nothing"
          write(*,*) "1.Record restoring point on break loops"
          call edit_parameter("PTMC_restore_method ")
        endif
        if(sel.eq."9")call edit_step_move
      enddo
      return
      end

      subroutine write_PTMC
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/file.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/ensemble.h"
      include "../../include/PTMC/PTMC.h"
      include "../../include/step_move/step_move.h"
      file_name=file_path(:index(file_path," ")-1)//"tmp."//job_file
      open(20,file=file_name,access="append")
      write(20,"(I3,1x,I3,
     &1x,A9,1x,I5,
     &1x,A10,1x,F7.2,
     &1x,A11,1x,F7.2,
     &1x,A11,1x,F6.2,
     &1x,A10,1x,F13.1,
     &1x,A11,1x,F13.1,
     &1x,A11,1x,F13.1,
     &1x,A20,1x,I2,
     &1x,A22,1x,F5.3,
     &1x,A16,1x,I2,
     &1x,A16,1x,F5.3,
     &1x,A16,1x,F5.3,
     &(9(1x,A4,1x,F3.1)))")
     &21,0,
     &"ndim_fac=",ndim_fac,
     &"init_temp=",init_temp,
     &"final_temp=",final_temp,
     &"delta_temp=",delta_temp,
     &"init_loop=",init_loop,
     &"final_loop=",final_loop,
     &"break_loop=",break_loop,
     &"PTMC_restore_method=",PTMC_restore_method,
     &"PTMC_acceptance_ratio=",PTMC_acceptance_ratio,
     &"PTMC_fix_method=",PTMC_fix_method,
     &"PTMC_temp_ratio=",PTMC_temp_ratio,
     &"PTMC_step_ratio=",PTMC_step_ratio,
     &"mva=",mva,
     &"mvb=",mvb,
     &"mvc=",mvc,
     &"mvd=",mvd,
     &"mve=",mve,
     &"mvf=",mvf,
     &"mvg=",mvg,
     &"mvh=",mvh,
     &"mvi=",mvi
      close(20)
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
