*========================================================================
* File Name : usr_score2_main.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Thu 21 Apr 2011 11:20:36 AM CST
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
      include "../../include/header.h"
      include "../../include/pes.h"
      include "../../include/job.h"
      include "../../include/simulation.h"
      include "../../include/tools/moment.h"
      include "../../include/tools/usr.h"
      include "../../include/tester/score.h"
      include "../../include/tester/tester.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/PTMC/PTMC.h"
      real*8 diff_moment,score_time
      integer read_top_usr_num,read_source_num
      integer top_usr_num_target(moment_y_max),
     &top_atom_num(moment_y_max),
     &top_usr_io_target(moment_y_max,atom_num_max),
     &top_usr_id_target(moment_y_max,atom_num_max)
      real*8 target_moment(moment_y_max,moment_z_max)
      real*8 target_moment_length(moment_y_max),source_moment_length
      real*8 temp_matrix(1000,100),dummy_x,dummy_y
      integer end_1st,end_2nd,end_3rd,end_4th
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
      if(wscreen)write(*,"(I5,1x,A14,1x,A80)")
     &myid,"Start to read ",target_file_name
      target_init=dint(score_target_init)
      target_final=dint(score_target_final)
      call read_formatted_header(target_file_name,
     &"total_moment_y_num ")
      call read_formatted_header(target_file_name,
     &"total_moment_z_num ")
      call read_formatted_header(target_file_name,
     &"atom_num ")
      call read_formatted_header(target_file_name,
     &"file_z_dim ")
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
        top_atom_num(I0)=atom_num
        read(21,*)dummy_x,dummy_y,
     &(temp_matrix(I0,I1),I1=1,file_z_dim-2)
        do I1=1,total_moment_z_num
          target_moment(I0,I1)=temp_matrix(I0,I1)
        enddo
        top_usr_num_target(I0)=
     &dint(temp_matrix(I0,total_moment_z_num+1))
        do I1=1,top_atom_num(I0)
          top_usr_io_target(I0,I1)=
     &dint(temp_matrix(I0,total_moment_z_num+1+I1))
        enddo
        target_moment_length(I0)=0.D0    
        do I3=1,total_moment_z_num
          target_moment_length(I0)=target_moment_length(I0)+
     &target_moment(I0,I3)**2
        enddo
        target_moment_length(I0)=dsqrt(target_moment_length(I0))
        score_y(I0)=0.D0
        score_mean(I0)=0.D0
        read_top_usr_num=0
        do I1=1,top_atom_num(I0)
          if(top_usr_io_target(I0,I1).eq.1)then
            read_top_usr_num=read_top_usr_num+1
            top_usr_id_target(I0,read_top_usr_num)=I1
          endif
        enddo
        if(read_top_usr_num.ne.top_usr_num_target(I0).and.wscreen)
     &write(*,"(I5,1x,A33)")myid,"Warning!! top_usr_num inconsist!!"
      enddo
      close(21)
C============USR SCORE INITIAL PARAMETERS===================
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
      call read_unformatted_header(header_file_name,"file_y_dim ")
      ndim=file_y_dim
      call read_unformatted_header(header_file_name,"file_x_dim ")
      read_source_num=file_x_dim
      call read_unformatted_header(header_file_name,
     &"simulation_delta_time ")
      if(read_source_num.lt.total_loop_int)then
        total_loop_int=read_source_num
        if(wscreen)write(*,"(I5,1x,A27,1x,I14)") 
     &myid,"Total usr num exceed Fix to",total_loop_int
      endif
C=====Wrinting header to usr file==============
      if(score_output_flag.eq.1.or.score_output_flag.eq.3)then   !only sutiable for 1 target tdusr ,and selected topology in dat/job file.
        if(tester_output_flag.eq.1)then
          octave_mom_time_header_file=score_mom_time_file_name(
     &:index(score_mom_time_file_name," ")-1)//"o"
          if(wscreen)write(*,"(I5,1x,A44,1x,A80)")
     &myid,"Output octave headers of moment function to ",
     &octave_mom_time_header_file
          file_x_dim=total_loop_int
          call write_usr_octave_header(octave_mom_time_header_file)
        endif
        call usr_score_mom_time_header_file
        if(wscreen)write(*,"(I5,1x,A37,1x,A80)")
     &myid,"Output moment descriptors vs time to ",
     &score_mom_time_file_name
        open(25,file=score_mom_time_file_name,access="append")
        write(25,"(A14)")"# name: moment"
        write(25,"(A14)")"# type: matrix"
        write(25,"(A8,1x,I13)")"# rows: ",total_loop_int
        write(25,"(A11,1x,I4)")"# columns: ",6+total_moment_z_num
        close(25)
      endif
C====================score vs time====================================================
      if(score_output_flag.eq.2.or.score_output_flag.eq.3)then
        call usr_score_time_header_file
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
C===============Calculate USR score========================
      if(wscreen)write(*,"(I5,1x,A14,1x,A80)")
     &myid,"Start to read ",source_file_name
C      call count_file_header_line_unformatted(source_file_name)
      header_line_num=1
      open(22,file=source_file_name,form="unformatted",status="old")
      do I0=1,init_loop_int-1+header_line_num !include the header lines
        read(22)
      enddo
      score_time=init_loop*simulation_delta_time  !in ps
      if(score_function.eq.1)then
        do I0=1,total_loop_int        !ensemble average
          read(22) (x(I1),I1=1,ndim)
          do I1=1,delta_loop_int-1   !skip interval
            read(22)
          enddo
          do I1=target_init,target_final      !calculate local minima
            top_usr_num=top_usr_num_target(I1)
            do I2=1,top_usr_num
              top_usr_id(I2)=top_usr_id_target(I1,I2)
            enddo
            call usr
C==============Conventional score function=======================
            diff_moment=0.D0
            do I2=1,end_1st    !if usr_score_1st_moment=0(off),it will skip 1st moment
              diff_moment=diff_moment+
     &dabs(mom(I2)-target_moment(I1,I2))
            enddo
            do I2=score_moment_sub_dim+1,end_2nd    !if usr_score_2nd_moment=0(off),it will skip
              diff_moment=diff_moment+
     &dabs(mom(I2)-target_moment(I1,I2))
            enddo
            do I2=score_moment_sub_dim*2+1,end_3rd   !if usr_score_3rd_moment=0(off),it will skip 3rd moment
              diff_moment=diff_moment+
     &dabs(mom(I2)-target_moment(I1,I2))
            enddo
            do I2=score_moment_sub_dim*3+1,end_4th  !if usr_score_4th_moment=0(off),it will skip 4th moment
              diff_moment=diff_moment+
     &dabs(mom(I2)-target_moment(I1,I2))
            enddo 
            score_y(I1)=1.D0/(1.D0+(diff_moment/score_moment_dim))
            score_mean(I1)=score_mean(I1)+score_y(I1)
            if(score_output_flag.eq.1.or.score_output_flag.eq.3)then
              call write_usr(score_mom_time_file_name,I0,I1,"app","org")
            endif
          enddo
C===============output score vs time=====================
          if(score_output_flag.eq.2.or.score_output_flag.eq.3)then
            write(24,"(F15.6,100(1x,F9.7))")score_time,
     &(score_y(I2),I2=target_init,target_final)
            score_time=score_time+simulation_delta_time
          endif
C======================================================
        enddo 
      else if(score_function.eq.2)then
        do I0=1,total_loop_int        !ensemble average
          read(22) (x(I1),I1=1,ndim)
          do I1=1,delta_loop_int-1   !skip interval
            read(22)
          enddo
          do I1=target_init,target_final      !calculate local minima
            top_usr_num=top_usr_num_target(I1)
            do I2=1,top_usr_num
              top_usr_id(I2)=top_usr_id_target(I1,I2)
            enddo
            call usr
C=============cosine score function
            source_moment_length=0.D0
            score_y(I1)=0.D0
            do I2=1,end_1st
              source_moment_length=source_moment_length+mom(I2)**2
              score_y(I1)=score_y(I1)+mom(I2)*target_moment(I1,I2)
            enddo
            do I2=score_moment_sub_dim+1,end_2nd
              source_moment_length=source_moment_length+mom(I2)**2
              score_y(I1)=score_y(I1)+mom(I2)*target_moment(I1,I2)
            enddo
            do I2=score_moment_sub_dim*2+1,end_3rd
              source_moment_length=source_moment_length+mom(I2)**2
              score_y(I1)=score_y(I1)+mom(I2)*target_moment(I1,I2)
            enddo
            do I2=score_moment_sub_dim*3+1,end_4th
              source_moment_length=source_moment_length+mom(I2)**2
              score_y(I1)=score_y(I1)+mom(I2)*target_moment(I1,I2)
            enddo
            score_y(I1)=score_y(I1)/
     &(dsqrt(source_moment_length)*target_moment_length(I1))
            score_mean(I1)=score_mean(I1)+score_y(I1)
            if(score_output_flag.eq.1.or.score_output_flag.eq.3)then
              call write_usr(score_mom_time_file_name,I0,I1,"app","org")
            endif
          enddo
C===============output score vs time=====================
          if(score_output_flag.eq.2.or.score_output_flag.eq.3)then
            write(24,"(F15.6,100(1x,F9.7))")score_time,
     &(score_y(I2),I2=target_init,target_final)
            score_time=score_time+simulation_delta_time
          endif
C======================================================
        enddo 
      endif
      do I1=target_init,target_final
        score_mean(I1)=score_mean(I1)/total_loop
      enddo
      score_temp=dint(temp) 
      close(22)
C=============output score vs time=======================
      if(score_output_flag.eq.2.or.score_output_flag.eq.3)close(24)
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
