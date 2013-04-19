*========================================================================
* File Name : tester_file.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Fri 25 Mar 2011 11:56:01 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine tester_file
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/job.h"
      include "../../include/pes.h"
      include "../../include/tools/moment.h"
      include "../../include/tools/histogram.h"
      include "../../include/tester/corr.h"
      include "../../include/tester/fourier.h"
      include "../../include/tester/sliding.h"
      include "../../include/tester/score.h"
      include "../../include/tester/bond_vector.h"
      include "../../include/tester/hist.h"
      include "../../include/tester/tester.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/PTMC/PTMC.h"
      integer source_id_int
      character source_id_char4*4
      job_skip=.false.
      if(source_file_flag.eq.0)then   !default input file name
        call temp_assignment
        source_id_int=dint(temp)
        call int2char4(source_id_int,source_id_char4)
        score_mean_file_name=
     &source_file_name(:index(source_file_name,".")-1)//"_"//
     &source_id_char4//"_mean.scr"
        score_time_file_name=
     &source_file_name(:index(source_file_name,".")-1)//"_"//
     &source_id_char4//".scr"
        score_mom_time_file_name=
     &source_file_name(:index(source_file_name,".")-1)//"_"//
     &source_id_char4//".mom"
        corr_cor_file_name=
     &source_file_name(:index(source_file_name,".")-1)//"_"//
     &source_id_char4//".cor"
        corr_dtf_file_name=
     &source_file_name(:index(source_file_name,".")-1)//"_"//
     &source_id_char4//".dtf"
        fourier_fft_file_name=
     &source_file_name(:index(source_file_name,".")-1)//"_"//
     &source_id_char4//".fft"
        fourier_dft_file_name=
     &source_file_name(:index(source_file_name,".")-1)//"_"//
     &source_id_char4//".dft"
        hist_file_name=
     &source_file_name(:index(source_file_name,".")-1)//"_"//
     &source_id_char4//".his"
        hist_mom_file_name=
     &source_file_name(:index(source_file_name,".")-1)//"_"//
     &source_id_char4//".mom"
        hist_min_xyz_file_name=
     &source_file_name(:index(source_file_name,".")-1)//"_"//
     &source_id_char4//".xyz"
        bond_vector_file_name=
     &source_file_name(:index(source_file_name,".")-1)//"_"//
     &source_id_char4//".ufv"
      else
        call source_assignment
C=======Source file name====================

        source_file_name=
     &source_path(:index(source_path," ")-1)//
     &simulation_type//"_"//source_type//"_"//
     &pes_file_name(:index(pes_file_name," ")-1)//
     &"."//source_file_type

C=======mom_score output file name==========
        score_mean_file_name=
     &result_root_path(:index(result_root_path," ")-1)//
     &scr_path(:index(scr_path," ")-1)//simulation_type//"_"//
     &source_type//"_"//pes_type(:index(pes_type," ")-1)//"_"//
     &config_name(:index(config_name," ")-1)//
     ""_mean.scr"

        score_time_file_name=
     &result_root_path(:index(result_root_path," ")-1)//
     &scr_path(:index(scr_path," ")-1)//simulation_type//"_"//
     &source_type//"_"//pes_file_name(:index(pes_file_name," ")-1)//
     &".scr"

        score_mom_time_file_name=
     &result_root_path(:index(result_root_path," ")-1)//
     &mom_path(:index(mom_path," ")-1)//simulation_type//"_"//
     &source_type//"_"//pes_file_name(:index(pes_file_name," ")-1)//
     &".mom"

C=========correlation output file_name============

        corr_cor_file_name=
     &result_root_path(:index(result_root_path," ")-1)//
     &cor_path(:index(cor_path," ")-1)//simulation_type//"_"//
     &source_type//"_"//pes_file_name(:index(pes_file_name," ")-1)//
     &".cor"

        corr_dtf_file_name=
     &result_root_path(:index(result_root_path," ")-1)//
     &dtf_path(:index(dtf_path," ")-1)//simulation_type//"_"//
     &source_type//"_"//pes_file_name(:index(pes_file_name," ")-1)//
     &".dtf"

C========Fourier transformation ouptut fine name=============
        fourier_fft_file_name=
     &result_root_path(:index(result_root_path," ")-1)//
     &fft_path(:index(fft_path," ")-1)//simulation_type//"_"//
     &source_type//"_"//pes_file_name(:index(pes_file_name," ")-1)//
     &".fft"

        fourier_dft_file_name=
     &result_root_path(:index(result_root_path," ")-1)//
     &fft_path(:index(fft_path," ")-1)//simulation_type//"_"//
     &source_type//"_"//pes_file_name(:index(pes_file_name," ")-1)//
     &".dft"

C========hist output file name==========

        if(hist_method.eq.1)then
          hist_source_type="ufe"
        else if(hist_method.eq.2)then
          hist_source_type="min"
        else if(hist_method.eq.6)then
          hist_source_type="scr"
        else 
          hist_source_type=source_type
        endif
        hist_file_name=
     &result_root_path(:index(result_root_path," ")-1)//
     &his_path(:index(his_path," ")-1)//simulation_type//"_"//
     &hist_source_type//"_"//
     &pes_file_name(:index(pes_file_name," ")-1)//
     &".his"
        hist_mom_file_name=
     &result_root_path(:index(result_root_path," ")-1)//
     &mom_path(:index(mom_path," ")-1)//simulation_type//"_"//
     &hist_source_type//"_"//
     &pes_file_name(:index(pes_file_name," ")-1)//
     &".mom"

        hist_min_xyz_file_name=
     &result_root_path(:index(result_root_path," ")-1)//
     &xyz_path(:index(xyz_path," ")-1)//simulation_type//"_"//
     &hist_source_type//"_"//
     &pes_file_name(:index(pes_file_name," ")-1)//
     &".xyz"

C========bond vector outpue file name======

        bond_vector_file_name=
     &result_root_path(:index(result_root_path," ")-1)//
     &xyz_path(:index(xyz_path," ")-1)//simulation_type//"_"//
     &source_type//"_"//pes_file_name(:index(pes_file_name," ")-1)//
     &".ufv"

C==========================================
        call check_file_exist(source_file_name)
        if(file_exist)then
          if(wscreen)
     &write(*,"(I5,1x,A10,1x,A80)") myid,"Read file=",source_file_name
        else
          write(*,"(I5,1x,A23,A80)")
     &myid,"Skip job, can not find ",source_file_name
          job_skip=.true.
          return
        endif
      endif
C==========Target file=========================================
      if(target_file_flag.ne.0)then
        call target_assignment
        target_file_name=
     &target_path(:index(target_path," ")-1)//
     &target_simulation_type//"_"//target_type//"_"//
     &target_pes_file_name(:index(target_pes_file_name," ")-1)//
     &"."//target_file_type
      endif
C============Header file=======================================
      if(header_file_flag.eq.3)then
        header_file_name=
     &source_file_name(:index(source_file_name," ")-1)
      else if(header_file_flag.eq.1.or.header_file_flag.eq.2)then
        call header_assignment
        header_file_name=
     &header_path(:index(header_path," ")-1)//
     &header_simulation_type//"_"//header_file_source_type//"_"//
     &header_pes_file_name(:index(header_pes_file_name," ")-1)//
     &"."//header_file_type
      endif
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
