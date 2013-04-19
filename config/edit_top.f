*========================================================================
* File Name : edit_top.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年10月22日 (週五) 15時25分50秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_top
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      include "../include/tools/top_sort.h"
      character sel*2
      if(pes_content.eq."none ")return
      sel="99"
      top_usr_locate=1  !temporary initial position
      top_frz_locate=2  
      top_acc_locate=3
      top_tcp_locate=4
      top_use_usr=0
      top_use_frz=0
      top_use_acc=0
      top_use_tcp=0
      do I0=1,top_num_max
        top_id_sort(I0)=0
      enddo
      do while(sel.ne."0")
        write(*,*) "======================================="
        write(*,*) "Current topology status:"
        call sort_top
        call show_top
        write(*,*) "======================================="
        write(*,*) "0,Quit"
        write(*,*) "1.USR topology"
        write(*,*) "2.Freezing topology"
C        write(*,*) "3.Temperature coupling topology"
C        write(*,*) "4.Accelerate topology" 
        read(*,*) sel
        if(sel.eq."1")call edit_top_usr
        if(sel.eq."2")call edit_top_frz
C        if(sel.eq."3")call edit_top_tcp
C        if(sel.eq."4")call edit_top_acc
      enddo
      return
      end
      subroutine show_top
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      include "../include/tools/top_sort.h"
      if(use_top.ne.0)then
        do I0=1,top_num
          write(*,"(1x,A6,1x,I3,1x,A5,1x,A3,1x,A5,1x,A3)")
     &" Top ID",I0,"name=",top_name(I0),"type=",top_type(I0)
        enddo
      else
        write(*,*) "<<No topology information>>"
      endif
      end

      subroutine sort_top
      implicit none
!     To save the memory, I prefer this method, though it seems redundent.
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      include "../include/tools/top_sort.h"
      use_top=0
      top_num=0
      do I0=1,top_num_max
        top_id(I0)=0
        if(top_id_sort(I0).eq.1)then
          use_top=1
          top_num=top_num+1
          top_id(top_num)=1
          top_type(top_num)=top_type_sort(I0)
          top_name(top_num)=top_name_sort(I0)
          do I1=1,atom_num
            top_value(top_num,I1)=top_value_sort(I0,I1) 
          enddo
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
