*========================================================================
* File Name : hist_energy_file.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 西元2010年07月26日 (週一) 17時05分12秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine hist_energy_file
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/job.h"
      include "../../include/file.h"
      include "../../include/histogram.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/hist_energy.h"
      integer temp_int
      job_skip=.false.
      if(source_file_flag.eq.0)then   !default input file name
        hist_input_file_name=
     &source_file(:index(source_file," ")-1)
        hist_output_file_name=
     &source_file(:index(source_file,".")
     &-1)//".his"
      else
        call temp_assignment
        temp_int=dint(temp)
        call int2char4(temp_int,int2char)
        if(source_file_flag.eq.1)then
          hist_input_file_name=
     &output_path(:index(output_path," ")-1)//
     &bimd_path(:index(bimd_path," ")-1)//
     &xyz_path(:index(cnl_path," ")-1)//
     &source_file(:index(source_file," ")-1)
     &//int2char//"."//source_type

          hist_output_file_name=
     &output_path(:index(output_path," ")-1)//
     &bimd_path(:index(bimd_path," ")-1)//
     &hist_path(:index(cnl_path," ")-1)//
     &source_file(:index(source_file," ")-1)
     &//int2char//".his"
        else if(source_file_flag.eq.2)then
         hist_input_file_name=
     &output_path(:index(output_path," ")-1)//
     &ptmc_path(:index(bimd_path," ")-1)//
     &xyz_path(:index(cnl_path," ")-1)//
     &source_file(:index(source_file," ")-1)
     &//int2char//"."//source_type

          hist_output_file_name=
     &output_path(:index(output_path," ")-1)//
     &ptmc_path(:index(bimd_path," ")-1)//
     &hist_path(:index(cnl_path," ")-1)//
     &source_file(:index(source_file," ")-1)
     &//int2char//".his"
        endif
        call check_file_exist(hist_input_file_name)
        if(.not.file_exist)then
          if(wscreen) write(*,"(I5,1x,A40)")myid,
     &"Source file doesn't exist,skip this job!"
          job_skip=.true.
          return
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
