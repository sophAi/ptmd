*========================================================================
* File Name : edit_top_frz.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Thu 24 Mar 2011 04:57:09 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_top_frz
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/tools/top_sort.h"
      integer atom_id,range_init,range_final
      character sel*2
      sel="99"
      if(top_use_frz.eq.0)then
        call top_frz_init
      endif
      do while(sel.ne."0")
        write(*,*) "Total atom num=",atom_num,"(1=free/0=freeze)"
        do I0=1,atom_num
          I1=I0*3
          write(*,"(I3,1x,A2,1x,A9,1x,I1)") 
     &I0,atom_name(I0),",use frz=",top_frz_io(I0)
        enddo
        write(*,*)"========================================"
        write(*,*)"Beware to use new BIMD method"
        write(*,*)"The backup function should be take care!"
        write(*,*)"========================================"
        write(*,*)"0.Quit"
        write(*,*)"1.Freeze a particle"
        write(*,*)"2.Free a particle"
        write(*,*)"3.Freeze a range of particles"
        write(*,*)"4.Free a range of particles"
        read(*,*) sel
        if(sel.eq."1")then
          write(*,*) "Please choose a particle index"
          read(*,*) atom_id
          top_frz_io(atom_id)=0
        endif
        if(sel.eq."2")then
          write(*,*) "Please choose a particle index"
          read(*,*) atom_id
          top_frz_io(atom_id)=1
        endif
        if(sel.eq."3")then
          write(*,*) "Please input the inital index"
          read(*,*) range_init
          write(*,*) "Please input the final index"
          read(*,*) range_final
          do J0=range_init,range_final
            top_frz_io(J0)=0
          enddo
        endif
        if(sel.eq."4")then
          write(*,*) "Please input the inital index"
          read(*,*) range_init
          write(*,*) "Please input the final index"
          read(*,*) range_final
          do J0=range_init,range_final
            top_frz_io(J0)=1
          enddo
        endif
      enddo
      top_use_frz=0
      do I0=1,atom_num
        if(top_frz_io(I0).eq.0)top_use_frz=1
        top_value_sort(top_frz_locate,I0)=dble(top_frz_io(I0))
      enddo
      if(top_use_frz.eq.1)then
        top_id_sort(top_frz_locate)=1
        top_name_sort(top_frz_locate)="frz"
        top_type_sort(top_frz_locate)="int"
      else
        top_id_sort(top_frz_locate)=0
      endif
      top_frz_num=0
      do I0=1,atom_num
        if(top_frz_io(I0).eq.1)then
          top_frz_num=top_frz_num+1
          top_frz_id(top_frz_num)=I0
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
