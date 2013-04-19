*========================================================================
* File Name : edit_top_usr.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Thu 24 Mar 2011 04:54:54 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_top_usr
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/tools/top_sort.h"
      integer atom_id,range_init,range_final
      character sel*2
      sel="99"
      if(top_use_usr.eq.0)then
        call top_usr_init
      endif
      do while(sel.ne."0")
        write(*,*) "Total atom num=",atom_num,"(1=use/0=do not use)"
        do I0=1,atom_num
          I1=I0*3
          write(*,"(I3,1x,A2,1x,A9,1x,I1)") 
     &I0,atom_name(I0),",use usr=",top_usr_io(I0)
        enddo
        write(*,*)"=========================="
        write(*,*)"0.Quit"
        write(*,*)"1.Do not calculate USR for a particle"
        write(*,*)"2.Calculate USR for a particle"
        write(*,*)"3.DO not calculate USR for a range of particles"
        write(*,*)"4.Calculate USR for a range of particles"
        read(*,*) sel
        if(sel.eq."1")then
          write(*,*) "Please choose a particle index"
          read(*,*) atom_id
          top_usr_io(atom_id)=0
        endif
        if(sel.eq."2")then
          write(*,*) "Please choose a particle index"
          read(*,*) atom_id
          top_usr_io(atom_id)=1
        endif
        if(sel.eq."3")then
          write(*,*) "Please input the inital index"
          read(*,*) range_init
          write(*,*) "Please input the final index"
          read(*,*) range_final
          do J0=range_init,range_final
            top_usr_io(J0)=0
          enddo
        endif
        if(sel.eq."4")then
          write(*,*) "Please input the inital index"
          read(*,*) range_init
          write(*,*) "Please input the final index"
          read(*,*) range_final
          do J0=range_init,range_final
            top_usr_io(J0)=1
          enddo
        endif
      enddo
      top_use_usr=0
      do I0=1,atom_num
        if(top_usr_io(I0).eq.0)top_use_usr=1
        top_value_sort(top_usr_locate,I0)=dble(top_usr_io(I0))
      enddo
      if(top_use_usr.eq.1)then
        top_id_sort(top_usr_locate)=1
        top_name_sort(top_usr_locate)="usr"
        top_type_sort(top_usr_locate)="int"
      else
        top_id_sort(top_usr_locate)=0
      endif
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
