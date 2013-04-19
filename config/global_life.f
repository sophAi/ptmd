*========================================================================
* File Name : global_life.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年10月24日 (週日) 10時37分58秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine initial_life
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/life.h"
      loop_num=1
      life_num=1    
      file_name=file_path(:index(file_path," ")-1)//
     &"tmp."//life_file
      call clean_file(file_name)
      return
      end

      subroutine life_cycle
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/life.h"
      character yn*1
      yn="y"
      do while(yn.eq."y") 
        write(*,*) "The Life Cycle: ",life_num
        call job_cycle
        write(*,*) "Edit more life cycles?(y/n)"
        read(*,*) yn 
        write(*,*) "==================================="
        life_num=life_num+1
      enddo
      return
      end

      subroutine write_life
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/life.h"
      include "../include/job.h"
      include "../include/ensemble.h"
      include "../include/pes.h"
      include "../include/parameter.h"
      open(20,file=file_path(:index(file_path," ")-1)//
     &"tmp."//life_file,access="append")
      write(20,"(I3,1x,A5,1x,I5,1x,A4,1x,I5,1x,A7,1x,
     &I5,1x,A13,1x,I5,1x,A9,1x,I5,1x,A7,1x,I5)") 
     &6,
     &"life=",life_num,
     &"job=",job_num,   !this is the job sequent number
     &"job_id=",job_id,
     &"ensemble_num=",ensemble_num,
     &"loop_num=",loop_num
     &,"pes_id=",pes_id
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
