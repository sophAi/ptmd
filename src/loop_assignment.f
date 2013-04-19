*========================================================================
* File Name : loop_assignment.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 2011-04-21
* Last Modified : Thu 21 Apr 2011 10:59:57 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : 
* Description : 
*========================================================================

      subroutine loop_assignment
      implicit none  !input real*8 final_loop,init_loop, and delta_loop gives real*8 total_loop
      include "../include/global_common.h"
      include "../include/common.h"
      total_loop_int=dint((final_loop-init_loop)/delta_loop)+1
      total_loop=dble(total_loop_int)
      init_loop_int=dint(init_loop)
      final_loop_int=dint(final_loop)
      delta_loop_int=dint(delta_loop)
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
