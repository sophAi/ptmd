*========================================================================
* File Name : locate_current_pes.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 西元2010年06月22日 (週二) 11時50分18秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine locate_current_pes_id
C==========Priority pes_id>pes_type
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       include "../include/life.h"
       include "../include/job.h"
       include "../include/pes.h"
       integer read_pes_id
       character read_pes_flag*10,read_pes_type*15,read_pes_content*10
       logical locate_pes
       if(pes_type.eq."none ")then
         write(*,"(I5,1x,A8)")myid,"Skip PES"
         pes_id=0
         pes_content="none "
         return
       endif
       file_name=file_path(:index(file_path," ")-1)//pes_file
       locate_pes=.false.
       error_input=.false.
       call count_file_row(file_name)
C       write(*,*) "row=",file_row_num,file_line_num
       call count_file_line(file_name)
C       write(*,*) "line=",file_line_num
       open(20,file=file_name,status="old")
       do I0=1,file_line_num
         read(20,*) read_pes_flag,read_pes_id,read_pes_type,
     &read_pes_content
         if(pes_type.eq.read_pes_type.and.
     &pes_content.eq.read_pes_content)then
           pes_id=read_pes_id
           locate_pes=.true.
           pes_line_num=I0
           if(wscreen)then
             write(*,"(I5,1x,A13,1x,A15,1x,A13,1x,A10)")
     &myid,"Current pes: ",pes_type,",pes content=",pes_content
           endif
           read(20,*) read_pes_flag,read_pes_id
           if(read_pes_id.eq.pes_id)then
             do I1=1,2   !search forward 3 lines,is the pes_id is changed,it means 
!the pes_line_num is incorrect and should  -1
!ex, search fo CuAg, but in dat/pes, the first line is AgCu, so the pes_line_num_will miscount.
!Be sure in pes file, all alloy have four lines.
               read(20,*)read_pes_flag,read_pes_id
             enddo
             if(read_pes_id.ne.pes_id)pes_line_num=pes_line_num-1
           endif
           close(20)
C           write(*,*) "PES_line=",pes_line_num,pes_type,pes_content
           return
         endif 
       enddo
       write(*,*) "PES_line=",pes_line_num,pes_type,pes_content
       if(.not.locate_pes)then
         write(*,*) "<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>"
         write(*,*) "The pes you select is not in the database"
         write(*,*) "<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>"
         error_input=.true.
         close(20)
         return
       endif
       return
       end

       subroutine locate_current_pes_type
C==========Priority pes_id>pes_type
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       include "../include/life.h"
       include "../include/job.h"
       include "../include/pes.h"
       integer read_pes_id
       character read_pes_flag*10,read_pes_type*15,read_pes_content*10
       logical locate_pes
       if(pes_id.eq.0)then
         write(*,"(I5,1x,A8)")myid,"Skip PES"
         pes_type="none"
         return
       endif
       file_name=file_path(:index(file_path," ")-1)//pes_file
       error_input=.false.
       locate_pes=.false.
       call count_file_line(file_name)
       open(20,file=file_name,status="old")
       do I0=1,file_line_num
         read(20,*) read_pes_flag,read_pes_id,read_pes_type,
     &read_pes_content
         if(pes_id.eq.read_pes_id)then
           pes_type=read_pes_type
           pes_content=read_pes_content
           locate_pes=.true.
           pes_line_num=I0
           if(wscreen)then
             write(*,"(I5,1x,A13,1x,A15,1x,A13,1x,A10)")
     &myid,"Current pes: ",pes_type,",pes content=",pes_content
           endif
           close(20)
           return
         endif
       enddo
       if(.not.locate_pes)then
         write(*,*) "<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>"
         write(*,*) "The pes you select is not in the database"
         write(*,*) "<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>"
         error_input=.true.
         close(20)
         return
       endif
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
