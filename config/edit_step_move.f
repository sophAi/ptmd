*========================================================================
* File Name : edit_step_move.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時10分09秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_step_move
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/step_move/step_move.h"
      character sel*2
      sel="99"
      do while(sel.ne."0")
        call show_parameter(0,"quit ")
        call show_parameter(1,"mva ")
        call show_parameter(2,"mvb ")
        call show_parameter(3,"mvc ")
        call show_parameter(4,"mvd ")
        call show_parameter(5,"mve ")
        call show_parameter(6,"mvf ")
        call show_parameter(7,"mvg ")
        call show_parameter(8,"mvh ")
        call show_parameter(9,"mvi ")
        write(*,*) "-------------------------------------------"
        read(*,*) sel
        if(sel.eq."1")call edit_parameter("mva ")
        if(sel.eq."2")call edit_parameter("mvb ")
        if(sel.eq."3")call edit_parameter("mvc ")
        if(sel.eq."4")call edit_parameter("mvd ")
        if(sel.eq."5")call edit_parameter("mve ")
        if(sel.eq."6")call edit_parameter("mvf ")
        if(sel.eq."7")call edit_parameter("mvg ")
        if(sel.eq."8")call edit_parameter("mvh ")
        if(sel.eq."9")call edit_parameter("mvi ") 
      enddo
      return
      end

      subroutine write_step_move
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/step_move/step_move.h"
      character yn*1
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
