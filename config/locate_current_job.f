*========================================================================
* File Name : locate_current_job.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時11分48秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine locate_current_job
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/life.h"
      include "../include/job.h"
      include "../include/ensemble.h"
      include "../include/pes.h"
      integer total_num,read_life_num,read_job_num
      character dim_dummy1*10
      logical locate_job
      locate_job=.false.
      total_num=6
      job_line_num=0
      file_name=file_path(:index(file_path," ")-1)//job_file
      open(20,file=file_name,status="old")
15    read(20,*)atom_num,dim_dummy1
C     &read_flag(1),read_int(1),read_flag(2),read_int(2),
C     &read_flag(3),read_int(3),read_flag(4),read_int(4),
C     &read_flag(5),read_int(5),read_flag(6),read_int(6)
C    &,((read_flag(I0),read_int(I0)),I0=1,total_num)
     &,(read_flag(I0),read_int(I0),I0=1,total_num)
      job_line_num=job_line_num+1
      do I0=1,total_num
        if(read_flag(I0).eq."life=")read_life_num=read_int(I0)
        if(read_flag(I0).eq."job=")read_job_num=read_int(I0)
      enddo
      if(read_life_num.eq.life_num.and.read_job_num.eq.job_num)then
        locate_job=.true.
        close(20)
        return
      endif
      do I0=1,atom_num+1
        read(20,*)
      enddo
      job_line_num=job_line_num+atom_num+1
      goto 15
      if(.not.locate_job.and.wscreen)then
        write(*,"(I5,1x,A24)") myid,"Can not find job!!Skip!!"
      endif
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
