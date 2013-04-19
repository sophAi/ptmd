*========================================================================
* File Name : usr_score_file.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年10月21日 (週四) 09時25分08秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine usr_score_file
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/job.h"
      include "../../include/tools/score.h"
      include "../../include/tester/tester.h"
      job_skip=.false.
      if(source_file_flag.eq.0)then   !default input file name
        score_target_file_name="target.mom"
        score_result_file_name="result_mean.scr"
        moment_result_file_name="result.mom"
        score_time_file_name="result.scr"
        score_source_file_name=source_file(:index(source_file," ")-1)
      else
        call file_assignment
              
        score_source_file_name=
     &source_path(:index(source_path," ")-1)//simulation_type//"_"//
     &source_type//"_"//pes_file_name(:index(pes_file_name," ")-1)//
     &"."//source_type
        moment_result_file_name=
     &result_root_path(:index(result_root_path," ")-1)//
     &mom_path(:index(mom_path," ")-1)//simulation_type//"_"//
     &source_type//"_"//pes_file_name(:index(pes_file_name," ")-1)//
     &".mom"
        write(*,*) moment_result_file_name
        score_target_file_name=
     &result_root_path(:index(result_root_path," ")-1)//
     &mom_path(:index(mom_path," ")-1)//
     &"target.mom"
        moment_result_file_name=
     &result_root_path(:index(result_root_path," ")-1)//
     &mom_path(:index(usr_path," ")-1)//
     &simulation_type//"_"//source_type//"_"//
     &pes_file_name(:index(pes_file_name," ")-1)//"."//source_type
        score_result_file_name= 
     &result_root_path(:index(result_root_path," ")-1)//
     &scr_path(:index(scr_path," ")-1)//
     &"result_mean.scr"
        score_time_file_name=
     &result_root_path(:index(result_root_path," ")-1)//
     &scr_path(:index(scr_path," ")-1)//simulation_type//"_"//
     &source_type//"_"//pes_file_name(:index(pes_file_name," ")-1)//
     &".scr"
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
