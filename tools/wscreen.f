*========================================================================
* File Name : wscreen.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時08分41秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine check_wscreen
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       character yn*3
       file_name=file_path(:index(file_path," ")-1)//wscreen_file
       open(20,file=file_name,status="old")
       read(20,*) yn
       close(20)
       if(yn.eq."yes")wscreen=.true.
       if(yn.eq."no")wscreen=.false.
       return
       end

       subroutine edit_wscreen
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       integer onoff
       write(*,*) "0.Run the program in background"
       write(*,*) "1.Run the program and show the information on screen"
       read(*,*)onoff
       call change_wscreen(onoff)
       return
       end

       subroutine change_wscreen(onoff)
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       integer onoff
       if(onoff.eq.0)wscreen=.false.
       if(onoff.eq.1)wscreen=.true.
       return
       end

       subroutine write_wscreen
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       integer onoff
       character yn*3
       if(wscreen)yn="yes"
       if(.not.wscreen)yn="no"
       file_name=file_path(:index(file_path," ")-1)//wscreen_file
       open(20,file=file_name,status="old")
       write(20,*) yn
       close(20)
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
