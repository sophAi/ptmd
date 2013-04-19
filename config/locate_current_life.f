*========================================================================
* File Name : locate_current_life.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Tue 26 Oct 2010 05:02:13 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine locate_current_life
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/life.h"
      include "../include/job.h"
      include "../include/ensemble.h"
      include "../include/pes.h"
      integer read_life_num(15),par_num1,life_num_temp
      character*15 read_life_flag(15)
      logical locate_life
      locate_life=.false.
      file_name=file_path(:index(file_path," ")-1)//life_file
      total_job_num=0
      open(20,file=file_name,status="old")
12    read(20,*,end=11)par_num1,
     &(read_life_flag(I0),read_life_num(I0),I0=1,par_num1)
      do I0=1,par_num1
        if(read_life_flag(I0).eq."life=")then
          life_num_temp=read_life_num(I0)
        endif
      enddo
      if(life_num_temp.eq.life_num)then
        do I0=1,par_num1
          if(read_life_flag(I0).eq."job=")job_num=read_life_num(I0)
          if(read_life_flag(I0).eq."job_id=")job_id=read_life_num(I0)
          if(read_life_flag(I0).eq."ensemble_num=")then
            if(job_id.eq.999)then
              ensemble_num=1      !job_id=999 ->skip the job,reset ensemble to 1
            else
              ensemble_num=read_life_num(I0)
            endif
          endif
          if(read_life_flag(I0).eq."loop_num=")loop_num=
     &read_life_num(I0)
          if(read_life_flag(I0).eq."pes_id=")pes_id=read_life_num(I0)
        enddo
C=======================Store the data for each life==>life_job and life_pes=======
        do I1=1,ensemble_num
          total_job_num=total_job_num+1
          life_id(total_job_num)=I1
          life_job(total_job_num)=job_num
          life_job_id(total_job_num)=job_id
          life_ensemble_id(total_job_num)=ensemble_num
          life_loop_id(total_job_num)=loop_num
          life_pes_id(total_job_num)=pes_id
        enddo
C        if(wscreen)then
C          write(*,"A5,1x,I5,1x,A10,1x,I5,1x,A4,1x,I5,1x,A7,1x,
C     &I5,1x,A13,1x,I5,1x,A9,1x,I5,1x,A7,1x,I5")
C     &"life=",life_num,
C     &"total_job=",total_job_num,
C     &"job=",job_num,
C     &"job_id=",job_id,
C     &"ensemble_num=",ensemble_num,
C     &"loop_num=",loop_num,
C     &"pes_id=",pes_id
C        endif
        locate_life=.true.
      endif
      goto 12
11    close(20)
      if(.not.locate_life)then
        write(*,"(I5,1x,A26)") myid,"Can not locate life! Quit!!" 
        stop
      endif
      return
      end

      subroutine count_life_num
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/life.h"
      include "../include/file.h"
      integer par_num1
      character dummy*20
      file_name=file_path(:index(file_path," ")-1)//life_file
      call count_file_line(file_name)
      open(20,file=file_name,status="old")
      do I0=1,file_line_num
        read(20,*) par_num1,dummy,total_life_num
      enddo
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
