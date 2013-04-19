*========================================================================
* File Name : corr.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Thu 09 Dec 2010 04:53:31 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine corr_import(file_name)
!Import the variaus physical quantities for GVACF calculation and store it to the vector:vx,vy,vz
!1 or 2 dimension is possible
!Current limitation: <4000000 data points
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/tools/correlation.h"
       include "../include/file.h"
       
       return
       end
       
       subroutine corr_export(file_name)
!Export the imported data from the corr_import subroutine and transfer it to a readable format for GVACF
!The export format:
!VACF_total_time_step
!frame_num
!dummy_char(n) vx,vy,vz
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/tools/correlation.h"
       include "../include/file.h"

       return
       end
!
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
