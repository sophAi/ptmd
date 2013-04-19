*========================================================================
* File Name : usr_score2_main.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 西元2010年07月16日 (週五) 14時59分43秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description : Advanced USR score function
* ========================================================================
      subroutine usr_score_main
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/usr/usr.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/usr_score.h"
      integer source_init,source_final,source_delta
      real*8 total_usr_score_num,diff_moment
      character target_file_name*80
      integer target_frame(tdusr_lmin_max),total_target_num
      integer moment_init,moment_final,total_source_num,
     &moment_length
      real*8 usr_target_moment(tdusr_lmin_max,total_usr_moment)
      integer dummy_frame,dummy_cst,dummy_fct,dummy_ftf
      real*8 dummy_pot
      integer end_1st,end_2nd,end_3rd,end_4th
      integer read_source_num,frame_num,read_ndim,dummy_int1
      character dummy_char3*3
      file_name=usr_score_file_name(:index(usr_score_file_name," ")-1)
      call check_file_exist(file_name)
      if(.not.file_exist)then
        write(*,"(I5,1x,A31)")myid,"Source file doesn't exist!Skip!"
        return
      endif
      target_file_name=
     &usr_score_target_file_name
     &(:index(usr_score_target_file_name," ")-1)
      call check_file_exist(target_file_name)
      if(.not.file_exist)then
        write(*,"(I5,1x,A30)")myid,"target.usr doesn't exist!Stop!"
        stop
      endif
C=======Calculate moment length=======================
      moment_length=0.D0
      if(usr_score_1st_moment.eq.1)moment_length=moment_length+4.D0
      if(usr_score_2nd_moment.eq.1)moment_length=moment_length+4.D0
      if(usr_score_3rd_moment.eq.1)moment_length=moment_length+4.D0
      if(usr_score_4th_moment.eq.1)moment_length=moment_length+4.D0
      end_1st=usr_score_1st_moment*4
      end_2nd=usr_score_2nd_moment*8
      end_3rd=usr_score_3rd_moment*12
      end_4th=usr_score_4th_moment*16
C=====================================================
      target_init=dint(usr_score_target_init)
      target_final=dint(usr_score_target_final)
      total_target_num=target_final-target_init+1
      open(21,file=target_file_name,status="old")
      do I0=1,target_init-1
        read(21,*)
      enddo
      do I0=target_init,target_final
        read(21,*)target_frame(I0),dummy_pot,dummy_cst,dummy_fct,
     &dummy_ftf,(usr_target_moment(I0,J2),J2=1,total_usr_moment)
        TDUSR(I0)=0.D0
      enddo
      close(21)
C============Start to calculate USR SCORE===================
      open(22,file=file_name,form="unformatted",status="old")
      read(22)
     &dummy_char3,read_source_num,frame_num,dummy_pot,read_ndim
      rewind(22)
      if(read_source_num.lt.source_final)then
        source_final=read_source_num
        final_loop=dble(source_final)
        if(wscreen)write(*,"(I5,1x,A27,1x,I14)") 
     &myid,"Total usr num exceed Fix to",source_final
      endif
      source_init=dint(init_loop)
      source_final=dint(final_loop)
      source_delta=dint(delta_loop)
      total_usr_score_num=
     &(final_loop-init_loop+1.D0)
     &/delta_loop
      total_source_num=dint(total_usr_score_num)
      do I0=1,source_init-1
        read(22)
      enddo
      do I0=source_init,source_final        !ensemble average
        if(source_type.eq."usr")then
          read(22,*) dummy_frame,dummy_pot,dummy_cst,dummy_fct,
     &dummy_ftf,(usr_moment(J2),J2=1,total_usr_moment)
        else if(source_type.eq."ufx")then
          read(22)    
     &dummy_char3,read_source_num,frame_num,dummy_pot,dummy_int1,
     &(x(I1),I1=1,read_ndim)
          call usr
          if(usr_score_write_moment.eq.1)then 
            call write_usr(usr_moment_result_file_name,
     &frame_num,"app","org")
          endif
        endif
        do I1=1,source_delta-1   !skip interval
          read(22)
        enddo
        do I1=target_init,target_final      !calculate local minima
          diff_moment=0.D0
          do I2=1,end_1st    !if usr_score_1st_moment=0(off),it will skip 1st moment
            diff_moment=diff_moment+
     &dabs(usr_moment(I2)-usr_target_moment(I1,I2))
          enddo
          do I2=5,end_2nd    !if usr_score_2nd_moment=0(off),it will skip 2nd moment
            diff_moment=diff_moment+
     &dabs(usr_moment(I2)-usr_target_moment(I1,I2))
          enddo
          do I2=9,end_3rd   !if usr_score_3rd_moment=0(off),it will skip 3rd moment
            diff_moment=diff_moment+
     &dabs(usr_moment(I2)-usr_target_moment(I1,I2))
          enddo
          do I2=13,end_4th  !if usr_score_4th_moment=0(off),it will skip 4th moment
            diff_moment=diff_moment+
     &dabs(usr_moment(I2)-usr_target_moment(I1,I2))
          enddo 
          TDUSR(I1)=TDUSR(I1)+1.D0/(1.D0+(diff_moment/moment_length))
        enddo
      enddo 
      do I1=target_init,target_final
        TDUSR(I1)=TDUSR(I1)/total_usr_score_num
      enddo
      TDUSR_temp=dint(temp) 
      close(22)
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
