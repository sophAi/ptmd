*========================================================================
* File Name : edit_temp.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 西元2010年07月19日 (週一) 16時08分34秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_temp
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/ensemble.h"
      character input_name*25,sel*2
14    sel="99"
      do while(sel.ne."0")
        call show_parameter(0,"quit ")
        call show_parameter(1,"init_temp ")
        call show_parameter(2,"final_temp ")
        call show_parameter(3,"delta_temp ")
        write(*,*) "--------------------------------"
        read(*,*) sel
        if(sel.eq."1")call edit_parameter("init_temp ")
        if(sel.eq."2")call edit_parameter("final_temp ")
        if(sel.eq."3")call edit_parameter("delta_temp ")
      enddo 
      if(delta_temp.eq.0.D0.and.(final_temp-init_temp).ne.0.D0)then
        write(*,*) "Warning!This intervel of temperature will cause 
     &incorrect ensemble!"
        write(*,*) "Please try again!"
        write(*,*) "You could use default value."
        goto 14
      endif
      if(delta_temp.eq.0.D0.and.(final_temp-init_temp).eq.0.D0)then
        delta_temp=1.D0
        ensemble_num=1
      else
        ensemble_num=dint((final_temp-init_temp)/delta_temp)+1
      endif
      write(*,*) "The ensemble number is ",ensemble_num
      write(*,*)
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
