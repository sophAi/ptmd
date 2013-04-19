*========================================================================
* File Name : locate_current_job_table.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時12分05秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine locate_current_job_table_id
C     Priority job_id>job_type
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/life.h"
      include "../include/ensemble.h"
      include "../include/file.h"
      include "../include/job.h"
      integer read_job_id
      character read_job_flag*10,read_job_type*10,read_job_content*50
      logical locate_job_table
      locate_job_table=.false.
      job_id=0 
      job_content="Skipping this job"
      file_name=file_path(:index(file_path," ")-1)//job_table_file
      call count_file_line(file_name)
      open(20,file=file_name,status="old")
      do I0=1,file_line_num
        job_line_num=job_line_num+1
        read(20,*) read_job_flag,read_job_id,read_job_type,
     &read_job_content
        if(job_type.eq.read_job_type)then
          job_id=read_job_id
          job_content=read_job_content
          locate_job_table=.true.
          job_table_line_num=I0
          if(wscreen)then
            write(*,"(I5,1x,A12,1x,A50)")
     &myid,"Current job=",read_job_content
          endif
          close(20)
          return
        endif
      enddo
      close(20)
      if(.not.locate_job_table)then
        if(wscreen)then
          write(*,"(I5,1x,A45)")
     &myid,"The job you indicated is not in the database!"
          write(*,"(I5,1x,A31)")
     &myid,"The program will skip this job!"
        endif
        ensemble_num=1
        loop_num=1
      endif
      return
      end

      subroutine locate_current_job_table_type
C     Priority job_id>job_type
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/life.h"
      include "../include/ensemble.h"
      include "../include/file.h"
      include "../include/job.h"
      integer read_job_id
      character read_job_flag*10,read_job_type*10,read_job_content*50
      logical locate_job_table
      job_type="SKIP"
      job_content="Skipping this job"
      locate_job_table=.false.
      file_name=file_path(:index(file_path," ")-1)//job_table_file
      call count_file_line(file_name)
      open(20,file=file_name,status="old")
      do I0=1,file_line_num
        job_line_num=job_line_num+1
        read(20,*) read_job_flag,read_job_id,read_job_type,
     &read_job_content
        if(job_id.eq.read_job_id)then
          job_type=read_job_type
          job_content=read_job_content
          locate_job_table=.true.
          job_table_line_num=I0
          if(wscreen)then
            write(*,"(I5,1x,A12,1x,A50)")
     &myid,"Current job=",read_job_content
          endif
          close(20)
          return
        endif
      enddo
      close(20)
      if(.not.locate_job_table)then  !usually assign job_id=0 for skipping the job
        if(wscreen)then
          write(*,"(I5,1x,A45)")
     &myid,"The job you indicated is not in the database!"
          write(*,"(I5,1x,A31)")
     &myid,"The program will skip this job!"
        endif
        ensemble_num=1
        loop_num=1
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
