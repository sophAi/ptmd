*========================================================================
* File Name : debug.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時04分51秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine check_debug
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       character yn*3
       file_name=file_path(:index(file_path," ")-1)//debug_file
       open(20,file=file_name,status="old")
       read(20,*) yn
       close(20)
       if(yn.eq."yes")debug=.true.
       if(yn.eq."no")debug=.false.
       return
       end

       subroutine edit_debug
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       integer onoff
       write(*,*) "0.Run the program in normal mode"
       write(*,*) "1.Run the program in debug mode"
       read(*,*)onoff
       call change_debug(onoff)
       return
       end

       subroutine change_debug(onoff)
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       integer onoff
       if(onoff.eq.0)debug=.false.
       if(onoff.eq.1)debug=.true.
       return
       end

       subroutine write_debug
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       integer onoff
       character yn*3
       if(debug)yn="yes"
       if(.not.debug)yn="no"
       file_name=file_path(:index(file_path," ")-1)//debug_file
       open(20,file=file_name,status="old")
       write(20,*) yn
       close(20)
       return
       end

       subroutine check_pes(show_method)
C show_method=xyz,pot,grd,all..
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/pes.h"
       character*3 show_method
       if(show_method.eq."all".or.show_method.eq."pot")
     &write(*,*) "pot=",pot
       if(show_method.eq."all".or.show_method.eq."xyz")then
         do I0=1,atom_num
           I1=I0*3
           write(*,"(I3,1x,F13.6,1x,F13.6,1x,F13.6)") 
     &I0,x(I1-2),x(I1-1),x(I1)
         enddo
       endif
       if(show_method.eq."all".or.show_method.eq."grd")then
         do I0=1,atom_num
           I1=I0*3
           write(*,"(I3,1x,F13.6,1x,F13.6,1x,F13.6)")
     &I0,grad(I1-2),grad(I1-1),grad(I1)
         enddo
       endif
C       pause
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
