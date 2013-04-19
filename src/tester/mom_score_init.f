*========================================================================
* File Name : mom_score_init.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Thu 21 Apr 2011 11:17:05 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine mom_score_init  !life_num,job_num have been known,then jump to the correspond parameters
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/ensemble.h"
      include "../../include/pes.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/score.h"
      if(wscreen)then
        write(*,"(I5,1x,A31)")myid,"Calculate moment score function"
        write(*,"(I5,1x,4(A4,1x,I2,1x))")
     &myid,
     &"1st=",score_1st_io,
     &"2nd=",score_2nd_io,
     &"3rd=",score_3rd_io,
     &"4th=",score_4th_io
      endif
      return
      end

      subroutine mom_score_header_file
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
      character char4*4,char4_id*4,octave_score_header_file*80
      if(tester_output_flag.eq.1)then
        octave_score_header_file=score_time_file_name
     &(:index(score_time_file_name," ")-1)//"o"
        if(wscreen)write(*,"(I5,1x,A43,1x,A80)")
     &myid,"Output octave headers of score function to ",
     &octave_score_header_file
        call creat_basic_octave_header(octave_score_header_file,"new")
        open(23,file=octave_score_header_file,access="append")
        write(23,"(A18)")"# name: total_loop"
        write(23,"(A14)")"# type: scalar"
        write(23,*)total_loop_int
        write(23,"(A30)")"# Total number of atomic index"
        write(23,"(A24)")"# name: score_member_num"
        write(23,"(A14)")"# type: scalar"
        write(23,*)(1+target_final-target_init)
        write(23,"(A17)")"# name: def_score"
        write(23,"(A14)")"# type: string"
        write(23,"(A12,1x,I5)")"# elements: ",
     &(target_final-target_init+1)
        write(23,"(A12)")"# length: 14"
        write(23,"(A14)")"0001.real_time"
        J0=1
        do I0=target_init,target_final
          J0=J0+1
          call int2char4(I0,char4_id)
          call int2char4(J0,char4)
          write(23,"(A12)")"# length: 15"
          write(23,"(A4,A7,A4)")char4,".member",char4_id
        enddo
C================score id table=======================
        write(23,"(A24)")"# name: score_matrix_dim"
        write(23,"(A14)")"# type: scalar"
        write(23,"(I1)")2
        write(23,"(A16)")"# The id of time"
        write(23,"(A21)")"# name: score_time_id"
        write(23,"(A14)")"# type: scalar"
        write(23,"(I1)")1
        write(23,"(A19)")"# The id of members"
        write(23,"(A23)")"# name: score_member_id"
        write(23,"(A14)")"# type: matrix"
        write(23,"(A9)")"# rows: 1"
        write(23,"(A11,1x,I5)")"# columns: ",
     &target_final-target_init+1
        write(23,*)(J0,J0=1+target_init,1+target_final)
        close(23)
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
      header_par_real(4)=total_loop
      header_par_name(5)="file_y_dim"
      header_par_real(5)=dble(1+target_final-target_init+1)
      header_par_name(6)="file_z_dim"
      header_par_real(6)=0.D0
      header_annotation="'time_ps score_1~score_n'"
      call creat_formatted_header(score_time_file_name)
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
