*========================================================================
* File Name : BIMD_history.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Mon 01 Nov 2010 04:10:17 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine BIMD_history(file_name,frame_num,access_method,
     &min_method)  !access_method="new" >replace file ;access_method="app">append old file; min_method="min" cn with local minimum; min_method="org" CN with original pot
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/cn/cn.h"
      include "../../include/tools/anl.h"
      integer frame_num
      character access_method*3,min_method*3
      if(min_method.eq."min")then
        call backup_pes("backup")
        call min_pes
      endif
      call cn
      call cn2name
      if(access_method.eq."new")then
        open(25,file=file_name,status="replace")
      else if(access_method.eq."app")then
        open(25,file=file_name,access="append")
      endif
C      write(21,*)"# frame_num lmin pot cn_name_of_lmin"
      write(25,"(I13,1x,F17.8,1x,A200)")frame_num,pot,
     &cn_name
      close(25)
      cn_prev_name=cn_name
C=======Back=============
      if(min_method.eq."min")then
        call backup_pes("resume")
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
