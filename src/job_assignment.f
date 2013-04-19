*========================================================================
* File Name : job_assignment.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年10月24日 (週日) 22時33分45秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine initial_job
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/life.h"
      include "../include/file.h"
      include "../include/ensemble.h"
      include "../include/job.h"
      include "../include/pes.h"
C=======Sleep=========================
      if(job_type.eq."SLEEP")then
!        call sleep
      endif
C========PTBH=====================
      if(job_type.eq."PTBH")then
!        call PTBH_init
      endif
C========PGA=====================
      if(job_type.eq."PGA")then
!        call PGA_init
      endif
C========MAXENT=====================
      if(job_type.eq."MAXENT")then
!        call MAXENT_init
      endif
C========BIMD=====================
      if(job_type.eq."BIMD")then
        call simulation_init
        call BIMD_init
        source_file_flag=1
        source_type="non"
      endif
C========PTMC=====================
      if(job_type.eq."PTMC")then
        call simulation_init
        call PTMC_init
        source_file_flag=1
        source_type="non"
      endif
C========CN=====================
      if(job_type.eq."CN")then
        call CN_init
      endif
C========TESTER===================
      if(job_type.eq."TESTER")then
        call tester_init
      endif
      if(job_type.eq."SKIP")then
        return
      endif
      return
      end


      subroutine job
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/life.h"
      include "../include/file.h"
      include "../include/ensemble.h"
      include "../include/job.h"
      include "../include/pes.h"
C=======Stop========================
C=======Sleep=========================
      if(job_type.eq."SLEEP")then
!        call sleep
      endif      
C========PTBH=====================
      if(job_type.eq."PTBH")then
!        call PTBH
      endif
C========PGA=====================
      if(job_type.eq."PGA")then
!        call PGA
      endif
C========MAXENT=====================
      if(job_type.eq."MAXENT")then
!        call MAXENT
      endif
C========BIMD=====================
      if(job_type.eq."BIMD")then
        call simulation_file
        call BIMD_main
      endif
C========PTMC=====================
      if(job_type.eq."PTMC")then
        call simulation_file
        call PTMC_main
      endif
C========CN=====================
      if(job_type.eq."CN")then
!        call CN
      endif
C========TESTER===================
      if(job_type.eq."TESTER")then
        call tester_main
      endif 
      if(job_type.eq."SKIP")then
        return
      endif
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
