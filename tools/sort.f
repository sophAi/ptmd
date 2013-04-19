*========================================================================
* File Name : sort.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時07分17秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine sort_real(sort_num,sort_data_real,sort_matrix)
      implicit none   !sort_matrix(1)=greatest,sort_matrix(2)=2nd greatest..and so on
      include "../include/global_common.h"
      include "../include/common.h"
      integer sort_num,sort_temp
      real*8 sort_data_real(sort_num)
      integer sort_matrix(sort_num)
      do I0=1,sort_num
        sort_matrix(I0)=I0
      enddo
      do I0=1,sort_num
        do I1=I0+1,sort_num
          if(sort_data_real(sort_matrix(I0)).gt.
     &sort_data_real(sort_matrix(I1))) then
            sort_temp=sort_matrix(I0)
            sort_matrix(I0)=sort_matrix(I1)
            sort_matrix(I1)=sort_temp 
          endif
        enddo
      enddo
      return
      end

      subroutine sort_integer(sort_num,sort_data_integer,sort_matrix)
      implicit none   !sort_matrix(1)=greatest,sort_matrix(2)=2nd greatest..and so on
      include "../include/global_common.h"
      include "../include/common.h"
      integer sort_num,sort_temp
      integer sort_data_integer(sort_num)
      integer sort_matrix(sort_num)
      do I0=1,sort_num
        sort_matrix(I0)=I0
      enddo
      do I0=1,sort_num
        do I1=I0+1,sort_num
          if(sort_data_integer(sort_matrix(I0)).gt.
     &sort_data_integer(sort_matrix(I1))) then
            sort_temp=sort_matrix(I0)
            sort_matrix(I0)=sort_matrix(I1)
            sort_matrix(I1)=sort_temp
          endif
        enddo
      enddo
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
