*========================================================================
* File Name : BIMD_sorting.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年10月22日 (週五) 14時58分42秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine BIMD_sorting(file_name,sort_row)
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       include "../../include/file.h"
       include "../../include/BIMD/BIMD.h"
       integer sort_row   !the program will sort at sort_row ,sort_row must >1
       integer start_sort_line,finish_sort_line
       real*8 read_time_label
C please input time_label or use common
       call count_file_row(file_name)
       call count_file_line(file_name)
       open(20,file=file_name,status="old")
       do I1=1,file_line_num
         read(20,*) read_time_label
         if(read_time_label.eq.time_label)then
           start_sort_line=I1
           goto 14
         endif
       enddo
14     close(20)
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
