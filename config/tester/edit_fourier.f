*========================================================================
* File Name : edit_fourier.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2011年04月13日 (週三) 12時57分39秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_fourier
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/ensemble.h"
      include "../../include/job.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/tester/fourier.h"
      include "../../include/tester/tester.h"
      character yn*1,sel*2
      call edit_source
      write(*,*) "<<<<Fourier MENU>>>>"
      sel="99"
      do while(sel.ne."0")
        call show_parameter(0,"quit ")
        call show_parameter(1,"fourier_delta_time ")
        call show_parameter(2,"fourier_window_time_step ")
        call show_parameter(3,"fourier_method ")
        call show_parameter(4,"tester_output_flag ")
        write(*,*)"----------------------------------------"
        read(*,*) sel
        if(sel.eq."1")call edit_parameter("fourier_delta_time ")
        if(sel.eq."2")call edit_parameter("fourier_window_time_step ")
        if(sel.eq."3")then
          write(*,*) "Please select the Fourier Transformation method"
          write(*,*) 
     &"1.Calculate Fast Fourier Transformation(DFT)"
          write(*,*)
     &"2.Calculate Discrete Fourier Transformation(FFT)"
          call edit_parameter("fourier_method ")
        endif
        if(sel.eq."4")then
          write(*,*) "Please select the output method"
          write(*,*) " 1. Output additional octave headers"
          write(*,*) " 2. Output sophai file only"
          call edit_parameter("tester_output_flag ")
        endif
      enddo
      if(fourier_delta_time.ne.0.001D0)then
        write(*,*) 
     &"PJ: To get smooth transformation, I suggest you use 0.001D0"
        write(*,*)"However,you should make sure your xyz files are "
        write(*,*)"truely in 0.001 ps."
        write(*,*)
     &"You could check the settings of the recording interval" 
      endif
      return
      end

      subroutine write_fourier
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/ensemble.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/fourier.h"
      integer temp_int
      file_name=file_path(:index(file_path," ")-1)//"tmp."//job_file
      open(20,file=file_name,access="append")
      write(20,"(I3,1x,I3,1x
     &A10,1x,I3,1x,
     &A9,1x,I5,1x,
     &A10,1x,F6.1,1x,
     &A11,1x,F6.1,1x,
     &A11,1x,F7.2,1x,
     &A25,1x,I13,1x,
     &A19,1x,F11.7,1x,
     &A15,1x,I2,1x,
     &A12,1x,I2,1x,
     &A17,1x,I2,1x,
     &A17,1x,I2,1x,
     &A12,1x,A20,1x,
     &A12,1x,A3,1x,
     &A17,1x,A3,1x,
     &A19,A80,A1,1x,
     &A24,1x,A3,1x,
     &A17,1x,A3,1x,
     &A19,A80,A1)")


     &11,7,
     &"tester_id=",tester_id,
     &"ndim_fac=",ndim_fac,
     &"init_temp=",init_temp,
     &"final_temp=",final_temp,
     &"delta_temp=",delta_temp,
     &"fourier_window_time_step=",fourier_window_time_step,
     &"fourier_delta_time=",fourier_delta_time,
     &"fourier_method=",fourier_method,
     &"output_flag=",tester_output_flag,
     &"source_file_flag=",source_file_flag,
     &"header_file_flag=",header_file_flag,
     &"tester_flag=",tester_flag,
     &"source_type=",source_type,
     &"source_file_type=",source_file_type,
     &"source_file_name= '",source_file_name,"'",
     &"header_file_source_type=",header_file_source_type,
     &"header_file_type=",header_file_type,
     &"header_file_name= '",header_file_name,"'"
      close(20)
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
