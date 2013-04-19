*========================================================================
* File Name : edit_job.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年10月24日 (週日) 15時54分29秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_job
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/life.h"
      include "../include/job.h"
      include "../include/ensemble.h"
      character yn*1
      job_id=0
      call locate_current_job_table_id
C     call edit_loop  !if necessary
      if(job_type.eq."PGA")then
c        call edit_PGA
      endif      
      if(job_type.eq."MAXENT")then
c        call edit_MAXENT
      endif
      if(job_type.eq."BIMD")then
        call edit_BIMD
      endif
      if(job_type.eq."PTMC")then
        call edit_PTMC
      endif
      if(job_type.eq."CN")then
c        call edit_CN      
      endif
      if(job_type.eq."TESTER")then
        call edit_TESTER
      endif
      if(job_type.eq."SLEEP")then
c        call edit_SLEEP
      endif
      if(job_type.eq."SKIP")then
      endif
      return
      end

      subroutine write_job
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/life.h"
      include "../include/ensemble.h"
      include "../include/job.h"
      include "../include/pes.h"
      include "../include/file.h"
      character yn*1
C=========First line of job file=====================
      file_name=file_path(:index(file_path," ")-1)//"tmp."//job_file
      open(20,file=file_name,access="append")
      write(*,*) "Writing ",file_name
      write(20,"(I5,1x,I3,1x,A5,1x,I5,1x,A4,1x,I5,1x,A7,1x,I5,
     &1x,A13,1x,I5,1x,A9,1x,I5,1x,A7,1x,I5)")
     &atom_num,6,
     &"life=",life_num,
     &"job=",job_num,
     &"job_id=",job_id,
     &"ensemble_num=",ensemble_num,
     &"loop_num=",loop_num,
     &"pes_id=",pes_id
      close(20)
C===================================================
C      if(job_type.eq."PTBH ")then
C         call write_PTBH
C      endif
      if(job_type.eq."PGA")then
c        call write_PGA
      else if(job_type.eq."MAXENT")then
c        call write_MAXENT
      else if(job_type.eq."BIMD")then
        call write_BIMD
      else if(job_type.eq."PTMC")then
        call write_PTMC
      else if(job_type.eq."CN")then
c          call write_CN
      else if(job_type.eq."TESTER")then
        call write_tester
      else if(job_type.eq."SLEEP")then
c        call write_SLEEP
      else if(job_type.eq."SKIP")then
        call write_SKIP   !write an empty job
      endif
      return
      end

      subroutine write_SKIP
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/job.h"
      include "../include/file.h"
      file_name=file_path(:index(file_path," ")-1)//"tmp."//job_file
      open(20,file=file_name,access="append")
      write(20,*) "No job assigned, skip this job"
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
