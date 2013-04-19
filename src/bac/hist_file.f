*========================================================================
* File Name : hist_file.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年10月22日 (週五) 15時46分05秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine hist_file
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/job.h"
      include "../../include/file.h"
      include "../../include/tester/hist.h"
      include "../../include/tester/tester.h"
      job_skip=.false.
C=====Specify the source name for output file when they are different!
      if(hist_method.eq.1)then
        hist_source_type="pot"
      else if(hist_method.eq.2)then
        hist_source_type="min"
      else
        hist_source_type=source_type
      endif
C==============================================
      if(source_file_flag.eq.0)then   !default input file name
        hist_source_file_name=
     &source_file(:index(source_file," ")-1)
        hist_output_original_file_name=
     &source_file(:index(source_file,".")
     &-1)//"_org.his"
        hist_output_normalized_file_name=
     &source_file(:index(source_file,".")
     &-1)//"_nor.his"
      else
        call file_assignment
        hist_source_file_name=
     &source_path(:index(source_path," ")-1)//simulation_type//"_"//
     &source_type//"_"//pes_file_name(:index(pes_file_name," ")-1)//
     &"."//source_type
        hist_output_original_file_name=
     &result_root_path(:index(result_root_path," ")-1)//
     &his_path(:index(his_path," ")-1)//
     &simulation_type//"_"//hist_source_type//"_"//
     &pes_file_name(:index(pes_file_name," ")-1)//
     &"_org.his"
        hist_output_normalized_file_name=
     &result_root_path(:index(result_root_path," ")-1)//
     &his_path(:index(his_path," ")-1)//
     &simulation_type//"_"//hist_source_type//"_"//
     &pes_file_name(:index(pes_file_name," ")-1)//
     &"_nor.his"
        hist_output_moment_file_name=
     &result_root_path(:index(result_root_path," ")-1)//
     &mom_path(:index(mom_path," ")-1)//
     &simulation_type//"_"//hist_source_type//"_"//
     &pes_file_name(:index(pes_file_name," ")-1)//
     &".mom"
      endif
      call check_file_exist(hist_source_file_name)
      if(.not.file_exist)then
        if(wscreen) write(*,"(I5,1x,A30,1x,A80)")myid,
     &"Skip!Can not find source file=",hist_source_file_name
        job_skip=.true.
        return
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
