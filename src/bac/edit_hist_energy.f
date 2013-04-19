*========================================================================
* File Name : edit__hist_energy.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年10月22日 (週五) 15時29分58秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_hist_energy
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/ensemble.h"
      include "../../include/tools/hist.h"
      include "../../include/job.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/tester/gvacf.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/hist_energy.h"
      character*2 sel,sel2
!Default input file name
      call edit_source
      source_type="ufx"
      sel="99"
      do while(sel.ne."0")
        call show_parameter(0,"quit ")
        call show_parameter(1,"init_loop ")
        call show_parameter(2,"final_loop ")
        call show_parameter(3,"delta_loop ")
        call show_parameter(4,"hist_method ")
        call show_parameter(5,"hist_interval  ")
        write(*,*) "---------------------------------------------"
        read(*,*) sel
        if(sel.eq."1")call edit_parameter("init_loop ")
        if(sel.eq."2")call edit_parameter("final_loop ")
        if(sel.eq."3")call edit_parameter("delta_loop ")
        if(sel.eq."4")then
          sel2="99"
          write(*,*)
     &"0. Calculate histogram from original potential and local minima"
          write(*,*)
     &"1. Calculate histogram from potential only"
          write(*,*)
     &"2. Calculate histogram from local minima only"
          write(*,*) "-----------------------------------------"
          call edit_parameter("hist_method ")
        endif
        if(sel.eq."5")call edit_parameter("hist_interval ")
      enddo
      return
      end

      subroutine write_hist_energy
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/ensemble.h"
      include "../../include/tools/hist.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/tester/gvacf.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/hist_energy.h"
      file_name=file_path(:index(file_path," ")-1)//"tmp."//job_file
      open(20,file=file_name,access="append")
      write(20,"(A10,1x,I3,1x,
     &A10,1x,F6.1,1x,
     &A11,1x,F6.1,1x,
     &A6,1x,F7.2,1x,
     &A10,1x,F12.1,1x,
     &A11,1x,F15.1,1x,
     &A6,1x,F10.1,1x,
     &A19,1x,I1,1x,
     &A14,1x,F17.6,1x,
     &A17,1x,I2,1x,A7,1x,A20,1x,A80)")

     &"tester_id=",tester_id,
     &"init_temp=",init_temp,
     &"final_temp=",final_temp,
     &"dTemp=",delta_temp,
     &"init_loop=",init_loop,
     &"final_loop=",final_loop,
     &"dloop=",delta_loop,
     &"hist_method=",hist_method,
     &"hist_interval=",hist_interval,
     &"read_file_method=",source_file_flag,
     &source_type,tester_flag,source_file
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
