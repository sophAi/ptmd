*========================================================================
* File Name : tester_init.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Thu 21 Apr 2011 11:04:15 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine tester_init
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/ensemble.h"
      include "../../include/pes.h"
      include "../../include/tools/moment.h"
      include "../../include/tools/histogram.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/sliding.h"
      include "../../include/tester/score.h"
      include "../../include/tester/bond_vector.h"
      include "../../include/tester/hist.h"
      include "../../include/tester/corr.h"
      include "../../include/tester/fourier.h"
      integer par_num1,par_num2,par_num3,par_num_all
      character read_atom_name*4,dim_dummy1*10,dummy2*10,dummy3*17
      character dummy4*12,tester_id_flag*10
      init_loop=0.D0
      final_loop=0.D0
      delta_loop=1.D0
      file_name=file_path(:index(file_path," ")-1)//job_file
      par_num1=6
C=================locate parameters======================
      call wait_till_file_close(file_name)
      open(20,file=file_name,status="old")
      do I0=1,job_line_num-1
        read(20,*)
      enddo
      read(20,*) atom_num,par_num1,
     &(read_flag(I0),read_int(I0),I0=1,par_num1)
      read(20,*)par_num2,par_num3,
     &(read_flag(par_num1+I0),read_real8(par_num1+I0),I0=1,par_num2),
     &(read_flag(par_num1+par_num2+I1),read_char(par_num1+par_num2+I1),
     &I1=1,par_num3)
      close(20)
      par_num_all=par_num1+par_num2+par_num3
      do I0=1,par_num_all
        if(read_flag(I0).eq."job_ensemble=")ensemble_num=read_int(I0)
        if(read_flag(I0).eq."loop=")loop_num=read_int(I0)
        if(read_flag(I0).eq."pes_id=")pes_id=read_int(I0)
        if(read_flag(I0).eq."tester_id=")tester_id=dint(read_real8(I0))
        if(read_flag(I0).eq."ndim_fac=")ndim_fac=dint(read_real8(I0))
        if(read_flag(I0).eq."init_temp=")init_temp=read_real8(I0)
        if(read_flag(I0).eq."final_temp=")final_temp=read_real8(I0)
        if(read_flag(I0).eq."delta_temp=")delta_temp=read_real8(I0)
        if(read_flag(I0).eq."init_loop=")init_loop=read_real8(I0)
        if(read_flag(I0).eq."final_loop=")final_loop=read_real8(I0)
        if(read_flag(I0).eq."delta_loop=")delta_loop=read_real8(I0)
        if(read_flag(I0).eq."output_flag=")tester_output_flag=
     &dint(read_real8(I0))
C=========Correlation sliding windows=================
        if(read_flag(I0).eq."window_width=")
     &window_width=dint(read_real8(I0))
        if(read_flag(I0).eq."sliding_width=")
     &sliding_width=dint(read_real8(I0))
        if(read_flag(I0).eq."source_file_flag=")
     &source_file_flag=dint(read_real8(I0))
        if(read_flag(I0).eq."target_file_flag=")
     &target_file_flag=dint(read_real8(I0))
        if(read_flag(I0).eq."header_file_flag=")
     &header_file_flag=dint(read_real8(I0))
C===========Score function of moment==================
        if(read_flag(I0).eq."score_1st_io=")
     &score_1st_io=dint(read_real8(I0))
        if(read_flag(I0).eq."score_2nd_io=")
     &score_2nd_io=dint(read_real8(I0))
        if(read_flag(I0).eq."score_3rd_io=")
     &score_3rd_io=dint(read_real8(I0))
        if(read_flag(I0).eq."score_4th_io=")
     &score_4th_io=dint(read_real8(I0))
        if(read_flag(I0).eq."score_function=")
     &score_function=dint(read_real8(I0))
        if(read_flag(I0).eq."score_target_init=")
     &score_target_init=read_real8(I0)
        if(read_flag(I0).eq."score_target_final=")
     &score_target_final=read_real8(I0)
        if(read_flag(I0).eq."score_output_method=")
     &score_output_flag=dint(read_real8(I0))
C===========Histogram===============================
        if(read_flag(I0).eq."hist_method=")
     &hist_method=dint(read_real8(I0))
        if(read_flag(I0).eq."hist_min_lower=")
     &hist_min_lower=read_real8(I0)
        if(read_flag(I0).eq."hist_min_upper=")
     &hist_min_upper=read_real8(I0)
        if(read_flag(I0).eq."hist_min_delta=")
     &hist_min_delta=read_real8(I0)
        if(read_flag(I0).eq."hist_max_lower=")
     &hist_max_lower=read_real8(I0)
        if(read_flag(I0).eq."hist_max_upper=")
     &hist_max_upper=read_real8(I0)
        if(read_flag(I0).eq."hist_max_delta=")
     &hist_max_delta=read_real8(I0)
        if(read_flag(I0).eq."hist_interval_lower=")
     &hist_interval_lower=read_real8(I0)
        if(read_flag(I0).eq."hist_interval_upper=")
     &hist_interval_upper=read_real8(I0)
        if(read_flag(I0).eq."hist_interval_delta=")
     &hist_interval_delta=read_real8(I0)
C============Correlation=================================
        if(read_flag(I0).eq."corr_observe_time_step=")
     &corr_observe_time_step=dint(read_real8(I0))
        if(read_flag(I0).eq."corr_delta_time=")
     &corr_delta_time=read_real8(I0)
        if(read_flag(I0).eq."corr_method=")
     &corr_method=dint(read_real8(I0))
C============FFT=========================================
        if(read_flag(I0).eq."fourier_window_time_step=")
     &fourier_window_time_step=dint(read_real8(I0))
        if(read_flag(I0).eq."fourier_delta_time=")
     &fourier_delta_time=read_real8(I0)
        if(read_flag(I0).eq."fourier_method=")
     &fourier_method=dint(read_real8(I0))
C============Source File=================================
        if(read_flag(I0).eq."tester_flag=")tester_flag=
     &read_char(I0)(:index(read_char(I0)," ")-1)
        if(read_flag(I0).eq."source_type=")source_type=
     &read_char(I0)(:index(read_char(I0)," ")-1)
        if(read_flag(I0).eq."source_file_name=")source_file_name=
     &read_char(I0)(:index(read_char(I0)," ")-1)
        if(read_flag(I0).eq."source_file_type=")source_file_type=
     &read_char(I0)(:index(read_char(I0)," ")-1)
        if(read_flag(I0).eq."target_type=")target_type=
     &read_char(I0)(:index(read_char(I0)," ")-1)
        if(read_flag(I0).eq."target_file_name=")target_file_name=
     &read_char(I0)(:index(read_char(I0)," ")-1)
        if(read_flag(I0).eq."target_file_type=")target_file_type=
     &read_char(I0)(:index(read_char(I0)," ")-1)
        if(read_flag(I0).eq."header_file_source_type=")
     &header_file_source_type=read_char(I0)(:index(read_char(I0)," ")-1)
        if(read_flag(I0).eq."header_file_type=")header_file_type=
     &read_char(I0)(:index(read_char(I0)," ")-1)
        if(read_flag(I0).eq."header_file_name=")header_file_name=
     &read_char(I0)(:index(read_char(I0)," ")-1)
      enddo
      call read_xyz_top_file(file_name,job_line_num)
      call periodic_mass
      call loop_assignment
!Generating the output information
      call tester_file
      if(wscreen)then
        write(*,"(I5,1x,A14,1x,A20)")
     &myid,"Tester job is ",tester_flag
        write(*,"(I5,1x,A25,1x,A80)")
     &myid,"Read source file name is ",source_file_name
      endif
      if(tester_flag.eq."sliding_correlation")call sliding_init
      if(tester_flag.eq."usr_score")call usr_score_init
      if(tester_flag.eq."mom_score")call mom_score_init
      if(tester_flag.eq."bond_vector")call bond_vector_init
      if(tester_flag.eq."corr")call corr_init
      if(tester_flag.eq."fourier")call fourier_init
      if(tester_flag.eq."histogram")call hist_init
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
