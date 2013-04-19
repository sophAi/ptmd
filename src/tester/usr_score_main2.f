*========================================================================
* File Name : usr_score2_main.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2011年01月10日 (週一) 18時15分12秒
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
      integer init_loop_int,final_loop_int,delta_loop_int
      real*8 total_loop,diff_moment,score_time
      integer total_loop_int,read_top_usr_num,read_source_num
      integer top_usr_num_target(moment_y_max),
     &top_atom_num(moment_y_max),
     &top_usr_io_target(moment_y_max,atom_num_max),
     &top_usr_id_target(moment_y_max,atom_num_max)
      real*8 target_moment(moment_y_max,moment_z_max)
      real*8 target_moment_length(moment_y_max),source_moment_length
      real*8 temp_matrix(1000,100),dummy_x,dummy_y
      integer end_1st,end_2nd,end_3rd,end_4th
      call check_file_exist(source_file_name)
      if(.not.file_exist)then
        write(*,"(I5,1x,A30,1x,A80)")
     &myid,"Skip!Can not find source file=",source_file_name
        job_skip=.true.
        return
      endif
      call check_file_exist(score_target_file_name)
      if(.not.file_exist)then
        write(*,"(I5,1x,A30,1x,A80)")
     &myid,"Skip!Can not find target file=",score_target_file_name
        job_skip=.true.
        return
      endif
C=======Calculate moment length=======================
      score_moment_dim=0.D0
      score_moment_sub_dim=4.D0
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
C===================================================================================
      if(wscreen)write(*,"(I5,1x,A14,1x,A80)")
     &myid,"Start to read ",score_target_file_name
      target_init=dint(score_target_init)
      target_final=dint(score_target_final)
      call read_formatted_header(score_target_file_name,
     &"total_moment_y_num ")
      call read_formatted_header(score_target_file_name,
     &"total_moment_z_num ")
      call read_formatted_header(score_target_file_name,
     &"atom_num ")
      call read_formatted_header(score_target_file_name,
     &"file_z_dim ")
      if(target_final.gt.total_moment_y_num)then
        if(wscreen)write(*,"(I5,1x,A41,1x,I13,1x,A4,1x,I13)")
     &myid,"Warning! Target final exceed, reset from ",target_final,
     &" to ",total_moment_y_num
        target_final=total_moment_y_num
      endif
      open(21,file=score_target_file_name,status="old")
      read(21,*)
      do I0=1,target_init-1
        read(21,*)
      enddo
      do I0=target_init,target_final
        top_atom_num(I0)=atom_num
        read(21,*)dummy_x,dummy_y,(
     &temp_matrix(I0,I1),I1=1,file_z_dim-2)
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
      init_loop_int=dint(init_loop)
      final_loop_int=dint(final_loop)
      delta_loop_int=dint(delta_loop)
      total_loop=(final_loop-init_loop+1.D0)/delta_loop
      total_loop_int=dint(total_loop)
      call read_unformatted_header(source_file_name,"file_y_dim ")
      ndim=file_y_dim
      call read_unformatted_header(source_file_name,"file_x_dim ")
      read_source_num=file_x_dim
      call read_unformatted_header(source_file_name,"init_loop ")
      call read_unformatted_header(source_file_name,
     &"simulation_delta_time ")
      if(read_source_num.lt.total_loop_int)then
        total_loop_int=read_source_num
        if(wscreen)write(*,"(I5,1x,A27,1x,I14)") 
     &myid,"Total usr num exceed Fix to",total_loop_int
      endif
      if(wscreen)write(*,"(I5,1x,A14,1x,A80)")
     &myid,"Start to read ",source_file_name
      open(22,file=source_file_name,form="unformatted",status="old")
      read(22)  !skip the header line
C=====Wrinting header to usr file==============
      if(score_output_flag.eq.1)then   !only sutiable for 1 target tdusr ,and selected topology in dat/job file.
        header_source_type="mom"
        header_par_num=9
        header_par_name(1)="pes_id"
        header_par_real(1)=dble(pes_id)
        header_par_name(2)="time_label"
        header_par_real(2)=time_label
        header_par_name(3)="ndim_fac"
        header_par_real(3)=1.D0
        header_par_name(4)="file_x_dim"
        header_par_real(4)=
     &dble(dint(final_loop-init_loop+1.D0)/dint(delta_loop))
        header_par_name(5)="file_y_dim"
        header_par_real(5)=dble(target_final-target_init)+1.D0
        header_par_name(6)="file_z_dim"
        header_par_real(6)=dble(6+total_moment_z_num)
        header_par_name(7)="atom_num"
        header_par_real(7)=dble(atom_num)
        header_par_name(8)="total_moment_y_num"
        header_par_real(8)=dble(target_final-target_init)+1.D0
        header_par_name(9)="total_moment_z_num"
        header_par_real(9)=dble(total_moment_z_num)
        header_annotation=
     &"'IDx IDy moment_1~moment_n pot cst_id fct_id ftf_id'"
        call creat_formatted_header(score_mom_time_file_name)
        if(wscreen)write(*,"(I5,1x,A37,1x,A80)")
     &myid,"Output moment descriptors vs time to ",
     &score_mom_time_file_name
      endif
C====================usr vs time====================================================
      if(score_output_flag.eq.2)then
        if(tester_output_flag.eq.1)then
          call creat_basic_octave_header(score_time_file_name,"new")
          open(23,file=score_time_file_name,access="append")
          write(23,"(A18)")"# name: total_loop"
          write(23,"(A14)")"# type: scalar"
          write(23,"(I13)")
     &(dint(final_loop-init_loop+1.D0)/dint(delta_loop))
          write(23,"(A30)")"# Total number of atomic index"
          write(23,"(A19)")"# name: score_y_num"
          write(23,"(A14)")"# type: scalar"
          write(23,"(I13)")(1+target_final-target_init)
          write(23,"(A36)")"# The first column is the score time"
          write(23,"(A15)")"# name: score_y"
          write(23,"(A14)")"# type: matrix"
          write(23,"(A8,1x,I13)")"# rows: ",
     &(dint(final_loop-init_loop+1.D0)/dint(delta_loop))
          write(23,"(A11,1x,I13)")"# columns: ",
     &(1+target_final-target_init+1)
        endif
        if(tester_output_flag.eq.2)then
          header_source_type="scr"
          header_par_num=6
          header_par_name(1)="pes_id"
          header_par_real(1)=dble(pes_id)
          header_par_name(2)="time_label"
          header_par_real(2)=time_label
          header_par_name(3)="ndim_fac"
          header_par_real(3)=1.D0
          header_par_name(4)="file_x_dim"
          header_par_real(4)=
     &dble(dint(final_loop-init_loop+1.D0)/dint(delta_loop))
          header_par_name(5)="file_y_dim"
          header_par_real(5)=dble(1+target_final-target_init+1)
          header_par_name(6)="file_z_dim"
          header_par_real(6)=0.D0
          header_annotation="'time_ps score_1~score_n'"
          call creat_formatted_header(score_time_file_name)
          open(23,file=score_time_file_name,access="append")
        endif
        if(wscreen)write(*,"(I5,1x,A24,1x,A80)")
     &myid,"Output score vs time to ",score_time_file_name
      endif
C===============Calculate USR score========================
      do I0=1,init_loop_int-1
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
            if(score_output_flag.eq.1)then
              call write_usr(score_mom_time_file_name,I0,I1,"app","org")
            endif
          enddo
C===============output usr vs time=====================
          if(score_output_flag.eq.2)then
            write(23,"(F15.6,100(1x,F9.7))")score_time,
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
            if(score_output_flag.eq.1)then
              call write_usr(score_mom_time_file_name,I0,I1,"app","org")
            endif
          enddo
C===============output usr vs time=====================
          if(score_output_flag.eq.2)then
            write(23,"(F15.6,100(1x,F9.7))")score_time,
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
C=============output usr vs time=======================
      if(score_output_flag.eq.2)close(23)
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
