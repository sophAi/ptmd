*========================================================================
* File Name : sliding_main.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時58分43秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine sliding_main
C     If we have a=window_width,b=sliding_width,with N=total_step
C     ,we have (N-a)/b sliding_windows
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/sliding.h"
      call p_matrix(file_name)
      call s_matrix
      call c_matrix
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
