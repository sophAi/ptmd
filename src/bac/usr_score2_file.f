*========================================================================
* File Name : usr_score_file.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 西元2010年07月16日 (週五) 12時22分24秒
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
      include "../../include/tester/tester.h"
      include "../../include/tester/usr_score.h"
      integer temp_int
      if(source_file_flag.eq.0)then   !default input file name
        usr_score_target_file_name="target.usr"
        usr_score_result_file_name="result.tdusr"
        usr_moment_result_file_name="result.usr"
      else
        call temp_assignment
        temp_int=dint(temp)
        call int2char4(temp_int,int2char)
        source_file=
     &source_file(:index(source_file," ")-1)
     &//int2char//"."//source_type
        usr_moment_result_file_name= 
     &source_file(:index(source_file," ")-1)
     &//int2char//"."//"usr"
        if(source_file_flag.eq.1)then
          usr_score_file_name=
     &output_path(:index(output_path," ")-1)//
     &bimd_path(:index(bimd_path," ")-1)//
     &usr_path(:index(usr_path," ")-1)//
     &source_file(:index(source_file," ")-1)
          usr_score_target_file_name=
     &output_path(:index(output_path," ")-1)//
     &bimd_path(:index(bimd_path," ")-1)//
     &usr_path(:index(usr_path," ")-1)//
     &"target.usr"
          usr_moment_result_file_name=
     &output_path(:index(output_path," ")-1)//
     &bimd_path(:index(bimd_path," ")-1)//
     &usr_path(:index(usr_path," ")-1)//
     &usr_moment_result_file_name
     &(:index(usr_moment_result_file_name," ")-1)
          usr_score_result_file_name= 
     &output_path(:index(output_path," ")-1)// 
     &bimd_path(:index(bimd_path," ")-1)// 
     &usr_path(:index(usr_path," ")-1)//
     &"result.tdusr"
        else if(source_file_flag.eq.2)then
          usr_score_file_name=
     &output_path(:index(output_path," ")-1)//
     &ptmc_path(:index(ptmc_path," ")-1)//
     &usr_path(:index(usr_path," ")-1)//
     &source_file(:index(source_file," ")-1)
          usr_moment_result_file_name=
     &output_path(:index(output_path," ")-1)//
     &ptmc_path(:index(ptmc_path," ")-1)//
     &usr_path(:index(usr_path," ")-1)//
     &usr_moment_result_file_name
     &(:index(usr_moment_result_file_name," ")-1)
          usr_score_target_file_name=
     &output_path(:index(output_path," ")-1)//
     &ptmc_path(:index(ptmc_path," ")-1)//
     &usr_path(:index(usr_path," ")-1)//
     &"target.usr"
          usr_score_result_file_name=
     &output_path(:index(output_path," ")-1)//
     &ptmc_path(:index(ptmc_path," ")-1)//
     &usr_path(:index(usr_path," ")-1)//
     &"result.tdusr"
        endif
        call check_file_exist(usr_score_file_name)
        if(.not.file_exist)then
          usr_score_file_name=
     &output_path(:index(output_path," ")-1)//
     &source_file(:index(source_file," ")-1)
          usr_moment_result_file_name=
     &output_path(:index(output_path," ")-1)//
     &usr_moment_result_file_name
     &(:index(usr_moment_result_file_name," ")-1)
          usr_score_target_file_name=
     &output_path(:index(output_path," ")-1)//
     &"target.usr"
          usr_score_result_file_name=
     &output_path(:index(output_path," ")-1)//
     &"result.tdusr" 
          if(wscreen)write(*,"(I5,1x,A13,1x,A80)")
     &myid,"Try searching",usr_score_file_name
          call check_file_exist(usr_score_file_name)
          if(file_exist)then
            if(wscreen)write(*,"(I5,1x,A13)")myid,"Located file!"
          else
            usr_score_file_name=
     &source_file(:index(source_file," ")-1)
            usr_moment_result_file_name=
     &usr_moment_result_file_name
     &(:index(usr_moment_result_file_name," ")-1)
            usr_score_target_file_name="target.usr"
            usr_score_result_file_name="result.tdusr"
            call check_file_exist(usr_score_file_name)
            if(file_exist)then
              if(wscreen)write(*,"(I5,1x,A13)")myid,"Located file!"
            else
              if(wscreen)write(*,"(I5,1x,A24)")
     &myid,"Can't locate source file"
            endif
          endif
        endif
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
