*========================================================================
* File Name : global_pes.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2011年03月23日 (週三) 21時48分53秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine pes_cycle
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       include "../include/life.h"
       include "../include/job.h"
       include "../include/pes.h"
       character sel*2
       sel="99"
       do while(sel.ne."0")
         if(pes_content.eq."none ")then
           call edit_pes_type
           call edit_pes
           call edit_top
           call initial_pes
         endif
         write(*,*) "======================================="
         write(*,*) "Current Status:"
         write(*,*) "Last job: ",job_type
         write(*,*) "Last PES: ",pes_type
         write(*,*) "PES content: ",pes_content
         write(*,*) "Material: ",config_name
         write(*,*) "Initial Coordinate: ",init_coord_file_name
         call show_top
         write(*,*) "======================================="
         write(*,*) " 0.Preserve the current settings"
         write(*,*) " 1.Modify the PES settings"
         write(*,*) " 2.Modify the topological settings"
         write(*,*) "---------------------------------------"
         read(*,*) sel
         if(sel.eq."1")then
           call edit_pes_type
           call edit_pes
           call initial_pes
         endif
         if(sel.eq."2")then
           call edit_top
         endif
       enddo
       return
       end
   

       subroutine show_pes
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       include "../include/life.h"
       include "../include/job.h"
       include "../include/pes.h"
       integer read_pes_id
       character read_pes_flag*3,read_pes_type*15,read_pes_content*10
       character read_pes_type_temp*15,pes_type_matrix(1000)*15
       common/pes_type_matrix0/pes_type_matrix
       read_pes_type_temp="pes"
       file_name=file_path(:index(file_path," ")-1)//pes_file
       call count_file_line(file_name)
       open(20,file=file_name,status="old")
       I1=0
       write(*,*) "Reading ",file_name
       write(*,*) "PES list:"
       write(*,*) "======================================="
       write(*,"(A13)") "    0  no PES"
       total_pes_num=0
       do I0=1,file_line_num
         read(20,*) read_pes_flag,read_pes_id,read_pes_type,
     &read_pes_content
         if(read_pes_type.ne.read_pes_type_temp)then
           I1=I1+1
           read_pes_type_temp=read_pes_type
           total_pes_num=total_pes_num+1
           write(*,"(I5,2x,A15)") total_pes_num,read_pes_type
           pes_type_matrix(total_pes_num)=read_pes_type
         endif
       enddo
       write(*,*) "======================================="
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
