*========================================================================
* File Name : matrix_pool.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時05分24秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine matrix_pool
     &(int_data,x_dim,y_dim,file_name,access_method)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      integer x_dim,y_dim
      integer int_data(x_dim,y_dim)
      character access_method*3
      if(access_method.eq."app")then
        open(10,file=file_name,access="append")
      else
        open(10,file=file_name,status="replace")
      endif
      do I0=1,y_dim
        write(10,*) (int_data(I0,I1),I1=1,x_dim)
      enddo
      close(10)
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
