*========================================================================
* File Name : edit_corr.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2011年04月13日 (週三) 12時57分30秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_corr
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/ensemble.h"
      include "../../include/job.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/tester/corr.h"
      include "../../include/tester/tester.h"
      character yn*1,sel*2
      call edit_source
      write(*,*) "<<<<Correlation MECU>>>>"
      sel="99"
      do while(sel.ne."0")
        call show_parameter(0,"quit ")
        call show_parameter(1,"init_loop ")
        call show_parameter(2,"final_loop ")
        call show_parameter(3,"delta_loop ")
        call show_parameter(4,"corr_delta_time ")
        call show_parameter(5,"corr_observe_time_step ")
        call show_parameter(6,"corr_method ")
        call show_parameter(7,"tester_output_flag ")
        write(*,*)"----------------------------------------"
        read(*,*) sel
        if(sel.eq."1")call edit_parameter("init_loop ")
        if(sel.eq."2")call edit_parameter("final_loop ")
        if(sel.eq."3")call edit_parameter("delta_loop ")
        if(sel.eq."4")call edit_parameter("corr_delta_time ")
        if(sel.eq."5")call edit_parameter("corr_observe_time_step ")
        if(sel.eq."6")then
          write(*,*) "Please select the correlation method"
C          write(*,*) 
C     &"1.Veclocity ACF and perform central correction(ufv+ufx)"
          write(*,*) 
     &"1.Use standard TACF (read ufv)"
          write(*,*) 
     &"2.Use FFT (read_ufv)"
          call edit_parameter("corr_method ")
        endif 
        if(sel.eq."7")then
          write(*,*) "Please select the output method"
          write(*,*) " 1. Output additional octave headers"
          write(*,*) " 2. Output sophai file only"
          call edit_parameter("tester_output_flag ")
        endif
      enddo
      if(corr_delta_time.ne.0.001D0)then
        write(*,*) 
     &"PJ: To get smooth power spectrum, I suggest you use 0.001D0"
        write(*,*)"However,you should make sure your xyz files are "
        write(*,*)"truely in 0.001 ps."
        write(*,*)
     &"You could check the settings of the recording interval" 
      endif
      return
      end

      subroutine write_corr
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
      include "../../include/tester/corr.h"
      integer temp_int
      file_name=file_path(:index(file_path," ")-1)//"tmp."//job_file
      open(20,file=file_name,access="append")
      write(20,"(I3,1x,I3,1x
     &A10,1x,I3,1x,
     &A9,1x,I5,1x,
     &A10,1x,F6.1,1x,
     &A11,1x,F6.1,1x,
     &A11,1x,F7.2,1x,
     &A10,1x,F13.1,1x,
     &A11,1x,F13.1,1x,
     &A11,1x,F13.1,1x,
     &A23,1x,I13,1x,
     &A16,1x,F11.7,1x,
     &A12,1x,I2,1x,
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


     &14,7,
     &"tester_id=",tester_id,
     &"ndim_fac=",ndim_fac,
     &"init_temp=",init_temp,
     &"final_temp=",final_temp,
     &"delta_temp=",delta_temp,
     &"init_loop=",init_loop,
     &"final_loop=",final_loop,
     &"delta_loop=",delta_loop,
     &"corr_observe_time_step=",corr_observe_time_step,
     &"corr_delta_time=",corr_delta_time,
     &"corr_method=",corr_method,
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
