*========================================================================
* File Name : top_assignment.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時44分49秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine top_init
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/pes.h"
       top_use_usr=0
       top_use_frz=0
       top_use_acc=0
       top_use_tcp=0
       use_top=0
       top_num=0
       return
       end

       subroutine top
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/pes.h"
C       write(*,*) "top_num=",top_num,top_name(1)
       do I0=1,top_num
         if(top_name(I0).eq."usr")then
           do I1=1,atom_num
             top_usr_io(I1)=dint(top_value(I0,I1))
           enddo
           top_type(I0)="int"
           top_use_usr=1
           call top_usr
         endif
         if(top_name(I0).eq."frz")then
           do I1=1,atom_num
             top_frz_io(I1)=dint(top_value(I0,I1))
           enddo
           top_use_frz=1
           top_type(I0)="int"
           call top_frz
         endif
       enddo
       if(top_use_usr.ne.1)call top_usr_init
       if(top_use_frz.ne.1)call top_frz_init
       if(top_use_acc.ne.1)call top_acc_init
       if(top_use_tcp.ne.1)call top_tcp_init
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
