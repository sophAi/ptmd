*========================================================================
* File Name : main.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 西元2010年06月29日 (週二) 17時34分39秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      program config
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/life.h"
      wscreen=.true.
      myid=0
      call check_file
      call update_version(version,update)
      call global_init
      write(*,*)
      write(*,*) "=========================="
      write(*,*) "  Sophai Configuration"
      write(*,*) "     By Po-Jen Hsu  "
      write(*,*) "       Ver.",version  
      write(*,*) "  Last Update:",update 
      write(*,*) "=========================="
      write(*,*) 
C=================================================
      call initial_file
      call initial_life
      call initial_job
      call initial_source
      call write_file_list
      call life_cycle
      call rename_files
      call edit_wscreen
      call write_wscreen
      stop
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
