*========================================================================
* File Name : main_lite.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時12分54秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      program config
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/life.h"
      wscreen=.true.
      call check_file
      call update_version(version,update)
      write(*,*)
      write(*,*) "=========================="
      write(*,*) "  Sophai Configuration"
      write(*,*) "       Lite Version   "
      write(*,*) "     By Po-Jen Hsu  "
      write(*,*) "       Ver.",version  
      write(*,*) "  Last Update:",update 
      write(*,*) "=========================="
      write(*,*) 
C=================================================
      call initial_file
      call initial_life
      call initial_job
      call write_file_list
      call job_cycle_lite
      call rename_files
      call edit_wscreen
      call write_wscreen
      stop
      end

      subroutine job_cycle_lite
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/life.h"
      include "../include/file.h"
      include "../include/job.h"
      include "../include/pes.h"
      integer sel1
      loop_num=1
      life_num=1
      job_num=1
      job_id=400
      job_type="BIMD"
      job_content="Brownian-type_Molecular_Dynamic"
      write(*,*) "Gupta Potential:"
      write(*,*) "1.Pure Metal"
      write(*,*) "2.Alloy" 
      read(*,*) sel1
      if(sel1.eq.1)pes_type="gupta_pure"
      if(sel1.eq.2)pes_type="gupta_alloy"
      write(*,*) "Please indicate the material,ex:Cu, AgCu, or CuAg"
      read(*,*) pes_content
      call locate_current_pes_id
      write(*,*)
      call edit_job
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
