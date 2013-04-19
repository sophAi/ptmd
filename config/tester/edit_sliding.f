*========================================================================
* File Name : edit_sliding.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Thu 18 Nov 2010 11:58:18 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_sliding
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/ensemble.h"
      include "../../include/job.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/sliding.h"
      character yn*7
      call edit_source
      write(*,*) "Please select the data type" 
      write(*,*) "1.Common neighbor analysis(*.cnl files)"
      write(*,*) "2.Atomic nearest neighbor list(*.anl files)"
      read(*,*) yn
      if(yn.eq."1".or.yn.eq."cnl")tester_data_type="cnl"
      if(yn.eq."2".or.yn.eq."anl")tester_data_type="anl"
      write(*,*) "Please input the window width(default=2000)"
      read(*,*) window_width
      write(*,*) "Please input the sliding width(default=1000)"
      read(*,*) sliding_width
      return
      end

      subroutine write_sliding
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
      include "../../include/tester/sliding.h"
      file_name=file_path(:index(file_path," ")-1)//"tmp."//job_file
      open(20,file=file_name,access="append")
      write(20,"(I3,1x,I3,1x,A10,1x,I3,1x,A9,1x,I5,1x,A10,1x,F6.1,1x,
     &A11,1x,F6.1,1x,
     &A11,1x,F7.2,1x,A13,1x,I9,1x,A14,1x,I9,1x,A17,1x,I2,1x,
     &A12,1x,A7,1x,
     &A12,1x,A20,1x,
     &A17,1x,A80,1x,
     &A17,1x,A3)")

     &8,4,
     &"tester_id=",tester_id,
     &"ndim_fac=",ndim_fac,
     &"init_temp=",init_temp,
     &"final_temp=",final_temp,
     &"delta_temp=",delta_temp,
     &"window_width=",window_width,
     &"sliding_width=",sliding_width,
     &"source_file_flag=",source_file_flag,
     &"source_type=",source_type,
     &"tester_flag=",tester_flag,
     &"source_file_name=",source_file_name,
     &"source_file_type=",source_file_type
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
