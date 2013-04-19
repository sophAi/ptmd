*========================================================================
* File Name : mom_score_output.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Thu 21 Apr 2011 11:18:52 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine mom_score_output
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/job.h"
      include "../../include/header.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/score.h"
      integer total_temp_int
      real*8 total_temp
      character octave_score_mean_header_file*80
      if(job_skip)return
      total_temp=(final_temp-init_temp)/delta_temp+1.D0
      total_temp_int=dint(total_temp)
      file_name=score_mean_file_name
     &(:index(score_mean_file_name," ")-1)
      call check_file_exist(file_name)
      if(.not.file_exist)then
        if(tester_output_flag.eq.1)then
          octave_score_mean_header_file=score_mean_file_name
     &(:index(score_mean_file_name," ")-1)//"o"
          write(*,"(I5,1x,A47,1x,A80)")
     &myid,"Write octave headers of mean score function to ",
     &octave_score_mean_header_file
          call creat_basic_octave_header(
     &octave_score_mean_header_file,"new")
          open(21,file=octave_score_mean_header_file,access="append")
          write(21,"(A18)")"# name: total_loop"
          write(21,"(A14)")"# type: scalar"
          write(21,*)total_temp_int
          write(21,"(A30)")"# Total number of atomic index"
          write(21,"(A19)")"# name: score_y_num"
          write(21,"(A14)")"# type: scalar"
          write(21,*)(1+target_final-target_init)
C==============-=score_mean id table===================
          write(21,"(A29)")"# name: score_mean_matrix_dim"
          write(21,"(A14)")"# type: scalar"
          write(21,"(I1)")2
          write(21,"(A26)")"# name: score_mean_temp_id"
          write(21,"(A14)")"# type: scalar"
          write(21,"(I1)")1
          write(21,"(A28)")"# name: score_mean_member_id"
          write(21,"(A14)")"# type: matrix"
          write(21,"(A9)")"# rows: 1"
          write(21,"(A11,1x,I4)")"# columns: ",
     &target_final-target_init+1
          write(21,*)(J0,J0=target_init,target_final)
          close(21)
        endif
        header_source_type="scr"
        header_par_num=6
        header_par_name(1)="pes_id"
        header_par_real(1)=dble(pes_id)
        header_par_name(2)="time_label"
        header_par_real(2)=time_label
        header_par_name(3)="ndim_fac"
        header_par_real(3)=1.D0
        header_par_name(4)="file_x_dim"
        header_par_real(4)=total_temp
        header_par_name(5)="file_y_dim"
        header_par_real(5)=dble(1+target_final-target_init+1)
        header_par_name(6)="file_z_dim"
        header_par_real(6)=0.D0
        header_annotation="'temp score_1~score_n'"
        call creat_formatted_header(file_name)
        open(20,file=file_name,access="append")
        write(20,"(A40)")"# The first column is the parallel index"
        write(20,"(A18)")"# name: score_mean"
        write(20,"(A14)")"# type: matrix"
        write(20,"(A8,1x,I6)")"# rows: ",total_temp_int
        write(20,"(A11,1x,I4)")"# columns: ",
     &(1+target_final-target_init+1)
        close(20)
      endif
      call wait_till_file_close(file_name)
      if(wscreen)write(*,"(I5,1x,A27,1x,A80)")
     &myid,"Output ensemble average to ",file_name
      open(22,file=file_name,access="append")
      write(22,"(F15.7,100(1x,F15.7))") score_temp,
     &(score_mean(I0),I0=target_init,target_final)
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
