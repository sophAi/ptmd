*========================================================================
* File Name : edit_lmin.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時09分23秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_lmin
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      character yn*1
      
      return
      end

      subroutine write_lmin
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      character yn*1
C=========First line of job file=====================
      file_name=file_path(:index(file_path," ")-1)//"tmp."//job_file
      open(20,file=file_name,access="append")
      write(*,*) "writing ",file_name
      write(20,"I5,1x,A11,1x,I5,1x,A4,1x,I5,1x,A7,1x,I5,
     &1x,A13,1x,I5,1x,A5,1x,I5,1x,A7,1x,I5")
     &atom_num,"atoms life=",
     &life_num,
     &"job=",job_num,
     &"job_id=",job_id,
     &"job_ensemble=",ensemble_num,
     &"loop=",loop_num,
     &"pes_id=",pes_id
      close(20)
C===================================================
      if(job_type.eq."PTBH ")then
         call write_PTBH
      endif
      if(job_type.eq."PGA")then
c        call write_PGA
      endif
      if(job_type.eq."MAXENT")then
c        call write_MAXENT
      endif
      if(job_type.eq."BIMD")then
        call write_BIMD
      endif
      if(job_type.eq."PTMC")then
c        call write_PTMC
      endif
      if(job_type.eq."CN")then
c          call write_CN
      endif
      if(job_type.eq."VACF")then
        call write_VACF
      endif
      if(job_type.eq."TESTER")then
C        call write_TESTER
      endif
      if(job_type.eq."SLEEP")then
c        call write_SLEEP
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
