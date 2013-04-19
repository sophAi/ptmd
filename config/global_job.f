*========================================================================
* File Name : global_job.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 西元2010年06月22日 (週二) 11時57分03秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine initial_job
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/job.h"
      file_name=file_path(:index(file_path," ")-1)//
     &"tmp."//job_file
      job_type="none "
      call clean_file(file_name)
      return
      end

      subroutine show_job
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/job.h"
      integer read_job_id
      character read_job_flag*10,read_job_type*10,read_job_content*50
      character job_type_matrix(1000)*10
      common /job_type_matrix0/job_type_matrix
      file_name=file_path(:index(file_path," ")-1)//job_table_file
      call count_file_line(file_name)
      open(20,file=file_name,status="old")
      write(*,*) "Reading ",file_path(:index(file_path," ")-1)//
     &job_table_file
      write(*,*) "Last job:",job_type
      write(*,*) "Job List:"
      write(*,*) "====================================================="
      do I0=1,file_line_num
        read(20,*) read_job_flag,read_job_id,read_job_type,
     &read_job_content
        job_type_matrix(I0)=read_job_type
        write(*,"(I5,2x,A10,1x,A50)") I0,read_job_type,read_job_content
      enddo
      write(*,*) "====================================================="
      close(20)
      return
      end

      subroutine job_cycle
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/life.h"
      include "../include/file.h"
      include "../include/job.h"
      include "../include/pes.h"
      integer sel
      character yn1*1,yn2*1,job_type_matrix(1000)*10
      common/job_type_matrix0/job_type_matrix
      yn1="y"
      job_num=0
      do while(yn1.eq."y")
        job_num=job_num+1
        job_id=0
101     write(*,*) "The Job Cycle: ",job_num 
        call pes_cycle
102     write(*,*)
        call show_job
        write(*,*) "Please select a job"
        read(*,*) sel
        job_type=job_type_matrix(sel)
        call edit_job
C========================Write all files for this job=======================
        write(*,*) "Write the setting into the file?(y/n)"
        read(*,*)yn2
        if(yn2.eq."y")then
          call write_job
          call write_pes
          call write_life
          call write_parameter
        else 
          job_num=job_num-1
        endif
        write(*,*) "================================================"
        write(*,*) "Edit more jobs in this life?(y/n)"
        read(*,*) yn1
      enddo
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
