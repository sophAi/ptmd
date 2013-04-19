*========================================================================
* File Name : top_usr.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Mon 04 Oct 2010 04:07:26 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine top_usr_init
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       include "../../include/pes.h"
       top_usr_num=atom_num
       do I0=1,atom_num
         top_usr_io(I0)=1
         top_usr_id(I0)=I0
       enddo
       return
       end
 
       subroutine top_usr
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       include "../../include/pes.h"
       top_usr_num=0
       do I0=1,atom_num
         if(top_usr_io(I0).eq.1)then
           top_usr_num=top_usr_num+1
           top_usr_id(top_usr_num)=I0
         endif
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
