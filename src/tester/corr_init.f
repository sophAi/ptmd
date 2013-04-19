*========================================================================
* File Name : corr_init.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Thu 21 Apr 2011 11:12:37 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine corr_init
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/ensemble.h"
      include "../../include/pes.h"
      include "../../include/simulation.h"
      include "../../include/tester/corr.h"
      include "../../include/tools/correlation.h"
      real*8 corr_read_simulation_loop
      if(job_skip)return
      call read_unformatted_header(header_file_name,"ndim_fac ")
      call read_unformatted_header(header_file_name,"file_x_dim ")
      call read_unformatted_header(header_file_name,"file_y_dim ")
      corr_member_num=(file_y_dim)/ndim_fac
C      call read_unformatted_header(header_file_name,
C     &"simulation_delta_time ")
C      if(source_type.eq."ufv")then
C        call read_unformatted_header
C     &(header_file_name,"simulation_ufv_loop ")
C        corr_read_simulation_loop=simulation_ufv_loop
C      else if(source_type.eq."vfx")then
C        call read_unformatted_header
C     &(header_file_name,"simulation_vfx_loop ")
C        corr_read_simulation_loop=simulation_vfx_loop
C      endif
C      if(corr_delta_time.ne.(corr_read_simulation_loop*
C     &simulation_delta_time))then
C        if(wscreen)then
C          write(*,"(I5,1x,A26,1x,F15.7,1x,A22,1x,F15.7)")
C     &myid,"Warning!Source delta time=",
C     &corr_read_simulation_loop*simulation_delta_time,
C     &"=/=setting delta time=",corr_delta_time
C          write(*,"(I5,1x,A9,1x,F15.7)")
C     &myid,"Reset to ",corr_read_simulation_loop*simulation_delta_time
C        endif
C        corr_delta_time=corr_read_simulation_loop*simulation_delta_time
C        delta_time=corr_delta_time
C      endif
C==========Check if corr_total_time_step is OK===========
      corr_total_time_step=total_loop_int
      if(file_x_dim.le.corr_total_time_step)then
        final_loop=init_loop-1+dble(corr_total_time_step)*delta_loop
        if(wscreen)write(*,"(I5,1x,A36,1x,F13.1)")
     &myid,"Warning! final loop exceeded! Fix to",final_loop
      endif
      if(corr_total_time_step.gt.time_step_max)then
        if(wscreen)then
          write(*,"(I5,1x,A46,1x,I13,A1,I13)")
     &myid,"Warning!Insufficient array for storing vectors",
     &corr_total_time_step,">",time_step_max
          write(*,"(I5,1x,A29,1x,I13)")
     &myid,"Reset corr_total_time_step to",time_step_max
        endif
        corr_total_time_step=time_step_max
      endif
      if(wscreen)then
        write(*,"(I5,1x,A12,1x,I4)")myid,"ndim factor=",ndim_fac
        write(*,"(I5,1x,A16,1x,I13)")
     &myid,"Total time step=",corr_total_time_step
        write(*,"(I5,1x,A31,1x,I5)")
     &myid,"Total elements for correlation= ",corr_member_num+1
        write(*,"(I5,1x,A49,1x,F7.2)")
     &myid,"Start calculate correlation with parallel label= ",temp
        write(*,"(I5,1x,A10,1x,F6.1,1x,A11,1x,F6.1,1x,A6,1x,F7.2,
     &1x,1x,A18,1x,I8,1x,A11,1x,F15.7)")
     &myid,"init_temp=",init_temp,"final_temp=",final_temp,
     &"dTemp=",delta_temp,
     &"observe_time_step=",corr_observe_time_step,
     &"delta_time=",corr_delta_time
      endif
      corr_dim=corr_member_num+1   !plus one mean corr
      delta_time=corr_delta_time  !for general form in FT
      ndim=corr_member_num*ndim_fac
      return
      end

      subroutine corr_header_file   !For any dimension
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/job.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/header.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/corr.h"
      include "../../include/tools/correlation.h"
      character octave_header_file*80,char4*4,char4_id*4
      if(tester_output_flag.eq.1)then
        octave_header_file=
     &corr_cor_file_name(:index(corr_cor_file_name," ")-1)//"o"
        if(wscreen)write(*,"(I5,1x,A24,1x,A80)")
     &myid,"Write octave headers to ",octave_header_file
        call creat_basic_octave_header(octave_header_file,"new")
        open(22,file=octave_header_file,access="append")
        write(22,"(A16)")"# name: ndim_fac"
        write(22,"(A14)")"# type: scalar"
        write(22,"(I4)")1
        write(22,"(A12)")"# name: temp"
        write(22,"(A14)")"# type: scalar"
        write(22,"(F15.6)")temp
        write(22,"(A28)")"# name: corr_total_time_step"
        write(22,"(A14)")"# type: scalar"
        write(22,"(I13)")corr_total_time_step
        write(22,"(A30,1x,I13,1x,A19)")"# The real number of rows are ",
     &corr_observe_time_step+1,"(including corr(0))"
        write(22,"(A30)")"# name: corr_observe_time_step"
        write(22,"(A14)")"# type: scalar"
        write(22,"(I13)")corr_observe_time_step
        write(22,"(A23)")"# name: corr_member_num"
        write(22,"(A14)")"# type: scalar"
        write(22,"(I5)")corr_member_num
        write(22,"(A26)")"# corr_dim include mean_vv"
        write(22,"(A16)")"# name: corr_dim"
        write(22,"(A14)")"# type: scalar"
        write(22,"(I5)")corr_dim
        write(22,"(A23)")"# name: corr_delta_time"
        write(22,"(A14)")"# type: scalar"
        write(22,"(F15.6)") corr_delta_time
        write(22,"(A19)")"# name: corr_method"
        write(22,"(A14)")"# type: scalar"
        write(22,"(I3)")corr_method
C================corr id table============================
        write(22,"(A55)")
     &"# Below is the definition of corr and its mapping index"
        write(22,"(A23)")"# name: corr_matrix_dim"
        write(22,"(A14)")"# type: scalar"
        write(22,"(I1)")2
        write(22,"(A17)")"# name: corr_x_id"
        write(22,"(A14)")"# type: scalar"
        write(22,"(I1)") 1
        write(22,"(A22)")"# name: corr_y_mean_id"
        write(22,"(A14)")"# type: scalar"
        write(22,"(I1)") 2
        write(22,"(A17)")"# name: corr_y_id"
        write(22,"(A14)")"# type: matrix"
        write(22,"(A9)")"# rows: 1"
        write(22,"(A11,1x,I4)")"# columns: ",corr_member_num
        write(22,"(3000(I4,1x))")(I0,I0=3,2+corr_member_num)
        write(22,"(A16)")"# name: def_corr"
        write(22,"(A14)")"# type: string"
        write(22,"(A12,1x,I5)")"# elements: ",corr_dim+2
        write(22,"(A11)")"# length: 9 "
        write(22,"(A9)")"0001.time"
        write(22,"(A11)")"# length: 9"
        write(22,"(A9)")"0002.mean"
        do I0=1,corr_member_num
          call int2char4(I0+2,char4)
          call int2char4(I0,char4_id)
          write(22,"(A12)")"# length: 11"
          write(22,"(A4,A3,A4)")char4,".id",char4_id
        enddo
        close(22)
      endif
C==============Write sophAi headers============================
      header_source_type="cor"
      header_par_num=11
      header_par_name(1)="pes_id"
      header_par_real(1)=dble(pes_id)
      header_par_name(2)="time_label"
      header_par_real(2)=time_label
      header_par_name(3)="ndim_fac"
      header_par_real(3)=ndim_fac
      header_par_name(4)="file_x_dim"
      header_par_real(4)=dble(corr_observe_time_step+1)
      header_par_name(5)="file_y_dim"
      header_par_real(5)=dble(2+corr_member_num)
      header_par_name(6)="corr_dim"
      header_par_real(6)=dble(corr_dim)
      header_par_name(7)="corr_observe_time_step"
      header_par_real(7)=dble(corr_observe_time_step)
      header_par_name(8)="corr_delta_time"
      header_par_real(8)=corr_delta_time
      header_par_name(9)="init_loop"
      header_par_real(9)=0.D0
      header_par_name(10)="final_loop"
      header_par_real(10)=dble(corr_observe_time_step)
      header_par_name(11)="corr_method"
      header_par_real(11)=dble(corr_method)
      header_annotation=
     &"'real_time org_mean_corr org_corr_y(1~corr_member_num)'"
      call creat_formatted_header(corr_cor_file_name)
C===========create dtf header file==========================
      header_source_type="dtf"     !Only modified these parameters, others are the same!
      header_par_real(3)=1.D0
      header_par_real(4)=dble(corr_member_num)
      header_par_real(5)=2.D0
      header_annotation="mean_CT CT_atom(1~corr_member_num)"
      call creat_formatted_header(corr_dtf_file_name)
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
