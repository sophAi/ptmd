*========================================================================
* File Name : edit_simulation.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 2010-10-29
* Last Modified : Wed 16 Feb 2011 05:04:58 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : 
* Description : 
*========================================================================

      subroutine edit_simulation_record
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/simulation.h"
      character sel*2
      sel="99"
      do while(sel.ne."0")
        write(*,*) "Total loop=",final_loop
        call show_parameter(0,"quit ")
        call show_parameter(1,"simulation_rec_loop ")
        call show_parameter(2,"simulation_xyz_loop ")
        call show_parameter(3,"simulation_ufe_loop ")
        call show_parameter(4,"simulation_ufx_loop ")
        call show_parameter(5,"simulation_ufv_loop ")
        call show_parameter(6,"simulation_ufg_loop ")
        call show_parameter(7,"simulation_vfx_loop ")
        write(*,*) "-------------------------------------------"
        read(*,*) sel
        if(sel.eq."1")then
          write(*,*) "Total loop=",final_loop
          call edit_parameter("simulation_rec_loop ")
        endif
        if(sel.eq."2")then
          write(*,*) "Total loop=",final_loop
          call edit_parameter("simulation_xyz_loop ")
        endif
        if(sel.eq."3")then
          write(*,*) "Total loop=",final_loop
          call edit_parameter("simulation_ufe_loop ")
        endif
        if(sel.eq."4")then
          write(*,*) "Total loop=",final_loop
          call edit_parameter("simulation_ufx_loop ")
        endif
        if(sel.eq."5")then
          write(*,*) "Total loop=",final_loop
          call edit_parameter("simulation_ufv_loop ")
        endif
        if(sel.eq."6")then
          write(*,*) "Total loop=",final_loop
          call edit_parameter("simulation_ufg_loop ")
        endif
        if(sel.eq."7")then
          write(*,*) "Total loop=",final_loop
          call edit_parameter("simulation_vfx_loop ")
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
