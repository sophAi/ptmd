*========================================================================
* File Name : mom_score_main.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Thu 21 Apr 2011 11:18:10 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description : Advanced MOM score function
* ========================================================================
      subroutine mom_score_main
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/header.h"
      include "../../include/pes.h"
      include "../../include/job.h"
      include "../../include/tools/moment.h"
      include "../../include/tester/score.h"
      include "../../include/tester/tester.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/PTMC/PTMC.h"
      real*8 score_time,diff_moment,
     &begin_score_time,end_score_time
      integer end_1st,end_2nd,end_3rd,end_4th
      real*8 target_moment(moment_y_max,moment_z_max)
     &,source_moment(moment_y_max,moment_z_max)
      real*8 target_moment_length(moment_y_max),source_moment_length
      real*8 dummy_x,dummy_y,temp_matrix(1000,100)
      character char4*4,char4_id*4,octave_score_header_file*80,
     &octave_mom_time_header_file*80
      call check_file_exist(source_file_name)
      if(.not.file_exist)then
        write(*,"(I5,1x,A30,1x,A80)")
     &myid,"Skip!Can not find source file=",source_file_name
        job_skip=.true.
        return
      endif
      call check_file_exist(target_file_name)
      if(.not.file_exist)then
        write(*,"(I5,1x,A30,1x,A80)")
     &myid,"Skip!Can not find target file=",target_file_name
        job_skip=.true.
        return
      endif
C===================================================================================
      if(wscreen)write(*,"(I5,1x,A14,1x,A80)")myid,"Start to read ",
     &target_file_name
      target_init=dint(score_target_init)
      target_final=dint(score_target_final)
      call read_formatted_header(target_file_name,
     &"total_moment_y_num ")
      call read_formatted_header(target_file_name,
     &"total_moment_z_num ")
      call read_formatted_header(target_file_name,
     &"total_moment_x_num ")
      score_moment_sub_dim=total_moment_x_num
      if(target_final.gt.total_moment_y_num)then
        if(wscreen)write(*,"(I5,1x,A41,1x,I13,1x,A4,1x,I13)")
     &myid,"Warning! Target final exceed, reset from ",target_final,
     &" to ",total_moment_y_num
        target_final=total_moment_y_num
      endif
      call count_file_header_line(target_file_name)
      open(21,file=target_file_name,status="old")
      do I0=1,target_init-1+header_line_num  ! include the header lines
        read(21,*)
      enddo
      do I0=target_init,target_final
        read(21,*)dummy_x,dummy_y,
     &(temp_matrix(I0,I1),I1=1,total_moment_z_num)
        do I1=1,total_moment_z_num
          target_moment(I0,I1)=temp_matrix(I0,I1)
        enddo
        target_moment_length(I0)=0.D0    
        do I3=1,total_moment_z_num
          target_moment_length(I0)=target_moment_length(I0)+
     &target_moment(I0,I3)**2
        enddo
        target_moment_length(I0)=dsqrt(target_moment_length(I0))
        score_y(I0)=0.D0
        score_mean(I0)=0.D0
C        write(*,*) 
C     &I0,"target=",(target_moment(I0,I3),I3=1,total_moment_z_num)
      enddo
      close(21)
C=======Calculate moment length=======================
      score_moment_dim=0.D0
      if(score_1st_io.eq.1)score_moment_dim=score_moment_dim+
     &score_moment_sub_dim
      if(score_2nd_io.eq.1)score_moment_dim=score_moment_dim+
     &score_moment_sub_dim
      if(score_3rd_io.eq.1)score_moment_dim=score_moment_dim+
     &score_moment_sub_dim
      if(score_4th_io.eq.1)score_moment_dim=score_moment_dim+
     &score_moment_sub_dim
      end_1st=score_1st_io*1*score_moment_sub_dim
      end_2nd=score_2nd_io*2*score_moment_sub_dim
      end_3rd=score_3rd_io*3*score_moment_sub_dim
      end_4th=score_4th_io*4*score_moment_sub_dim
C====================score vs time====================================================
      if(score_output_flag.ge.1)then
        call mom_score_header_file
        open(24,file=score_time_file_name,access="append")
        if(wscreen)write(*,"(I5,1x,A24,1x,A80)")
     &myid,"Output score vs time to ",score_time_file_name
        write(24,"(A36)")"# The first column is the score time"
        write(24,"(A15)")"# name: score_y"
        write(24,"(A14)")"# type: matrix"
        write(24,"(A8,1x,I13)")"# rows: ",total_loop_int
        write(24,"(A11,1x,I4)")"# columns: ",
     &(1+target_final-target_init+1)
      endif
C===============Calculate MOM score========================
      call read_formatted_header(header_file_name,"file_x_dim ")
      if(total_loop_int.gt.file_x_dim)then
        write(*,"(I5,1x,A34,1x,I13)") 
     &myid,"Total source num exceed. Reset to ",file_x_dim
        total_loop_int=file_x_dim
        total_loop=dble(total_loop_int)
      endif
      call count_file_header_line(source_file_name)
      open(22,file=source_file_name,status="old")
      do I0=1,init_loop_int-1+header_line_num !include the header lines
        read(22,*)
      enddo
      if(score_function.eq.1)then
        do I0=1,total_loop_int        !ensemble average
          do I1=target_init,target_final      !calculate local minima
            read(22,*) score_time,dummy_y
     &,(source_moment(I1,J2),J2=1,total_moment_z_num)
C==============Conventional score function=======================
            diff_moment=0.D0
            do I2=1,end_1st    !if mom_score_1st_moment=0(off),it will skip 1st moment
              diff_moment=diff_moment+
     &dabs(source_moment(I1,I2)-target_moment(I1,I2))
            enddo
            do I2=score_moment_sub_dim+1,end_2nd    !if mom_score_2nd_moment=0(off),it will skip
              diff_moment=diff_moment+
     &dabs(source_moment(I1,I2)-target_moment(I1,I2))
            enddo
            do I2=score_moment_sub_dim*2+1,end_3rd   !if mom_score_3rd_moment=0(off),it will skip 3rd moment
              diff_moment=diff_moment+
     &dabs(source_moment(I1,I2)-target_moment(I1,I2))
            enddo
            do I2=score_moment_sub_dim*3+1,end_4th  !if mom_score_4th_moment=0(off),it will skip 4th moment
              diff_moment=diff_moment+
     &dabs(source_moment(I1,I2)-target_moment(I1,I2))
            enddo 
            score_y(I1)=1.D0/(1.D0+(diff_moment/score_moment_dim))
            score_mean(I1)=score_mean(I1)+score_y(I1)
          enddo
C===============output score vs time=====================
       
          if(score_output_flag.ge.1)then
            write(24,"(F15.6,100(1x,F9.7))")score_time,
     &(score_y(I2),I2=target_init,target_final)
          endif
          do I1=1,delta_loop_int-1   !skip interval
            read(22,*)
          enddo
C======================================================
        enddo 
      else if(score_function.eq.2)then
        do I0=1,total_loop_int        !ensemble average
          do I1=target_init,target_final      !calculate local minima
            read(22,*) score_time,dummy_y,
     &(source_moment(I1,J2),J2=1,total_moment_z_num)
C=============cosine score function
            source_moment_length=0.D0
            score_y(I1)=0.D0
            do I2=1,end_1st
              source_moment_length=source_moment_length+
     &source_moment(I1,I2)**2
              score_y(I1)=score_y(I1)+
     &source_moment(I1,I2)*target_moment(I1,I2)
            enddo
            do I2=score_moment_sub_dim+1,end_2nd
              source_moment_length=source_moment_length+
     &source_moment(I1,I2)**2
              score_y(I1)=score_y(I1)+
     &source_moment(I1,I2)*target_moment(I1,I2)
            enddo
            do I2=score_moment_sub_dim*2+1,end_3rd
              source_moment_length=source_moment_length+
     &source_moment(I1,I2)**2
              score_y(I1)=score_y(I1)+
     &source_moment(I1,I2)*target_moment(I1,I2)
            enddo
            do I2=score_moment_sub_dim*3+1,end_4th
              source_moment_length=source_moment_length+
     &source_moment(I1,I2)**2
              score_y(I1)=score_y(I1)+
     &source_moment(I1,I2)*target_moment(I1,I2)
            enddo
            score_y(I1)=score_y(I1)/
     &(dsqrt(source_moment_length)*target_moment_length(I1))
            score_mean(I1)=score_mean(I1)+score_y(I1)
          enddo
C===============output score vs time=====================
          if(score_output_flag.ge.1)then
            write(24,"(F15.6,100(1x,F9.7))")score_time,
     &(score_y(I2),I2=target_init,target_final)
          endif
          do I1=1,delta_loop_int-1   !skip interval
            read(22,*)
          enddo
C======================================================
        enddo 
      else if(score_function.eq.3)then
C===========Ouotput a moment directly========================
        do I0=1,total_loop_int        !ensemble average
          do I1=target_init,target_final      !Only one moment is allowed, be sure to turn others off
            read(22,*) score_time,dummy_y,
     &(source_moment(I1,J2),J2=1,total_moment_z_num)
            do I2=1,end_1st
              score_y(I1)=source_moment(I1,I2)  !use end_1st to turn this on/off
            enddo
            do I2=score_moment_sub_dim+1,end_2nd  !use end_2nd to turn this on/off
              score_y(I1)=source_moment(I1,I2)
            enddo
            do I2=score_moment_sub_dim*2+1,end_3rd  !use end_3rd to turn this on/off
              score_y(I1)=source_moment(I1,I2)
            enddo
            do I2=score_moment_sub_dim*3+1,end_4th  !use end_4th to turn this on/off
              score_y(I1)=source_moment(I1,I2)
            enddo
            score_mean(I1)=score_mean(I1)+score_y(I1)  !Do the average for only one moment
          enddo
C===============output score vs time=====================
          if(score_output_flag.ge.1)then
            write(24,"(F15.6,100(1x,F9.7))")score_time,
     &(score_y(I2),I2=target_init,target_final)
          endif
          do I1=1,delta_loop_int-1   !skip interval
            read(22,*)
          enddo
C======================================================
        enddo
      endif
      score_temp=dint(temp)
      do I1=target_init,target_final
        score_mean(I1)=score_mean(I1)/total_loop
C        write(*,*)I1,score_temp,score_mean(I1),score_y(I1),
C     &total_loop
      enddo
      close(22)
C=============output mom vs time=======================
      if(score_output_flag.ge.1)close(24)
C======================================================
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
