*========================================================================
* File Name : global_file.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時10分44秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine initial_file
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      file_line_num=0 !Don't forget to clean this file--fili_list.tmp,use clean_file(file_list)
      file_name=file_path(:index(file_path," ")-1)//
     &"tmp."//list_changed_file
      call clean_file(file_name)
      return
      end

      subroutine cycle_file(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      character yn*1
      yn="y"
      do while(yn.eq."n")
C        call edit_file
        write(*,*) "The program allows you to edit more than 1 cycle."
        write(*,*) "Would you like to edit additional cycles?(y/n)" 
        read(*,*) yn
      enddo
      return
      end

      subroutine write_file_list(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      open(20,file=file_path(:index(file_path," ")-1)//
     &"tmp."//list_changed_file,
     &status="replace")!default file_name=write_file_list.tmp
      write(20,*) life_file
      write(20,*) job_file
      write(20,*) parameter_file
      close(20)
      return
      end

      subroutine rename_files  ! For final output
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      character job_file_name(100)*60
      file_name=file_path(:index(file_path," ")-1)//
     &"tmp."//list_changed_file
      call count_file_line(file_name)
      open(20,file=file_name,status="old")
      do I0=1,file_line_num
        read(20,*) job_file_name(I0)
        call rename(file_path(:index(file_path," ")-1)//
     &"tmp."//job_file_name(I0),
     &file_path(:index(file_path," ")-1)//job_file_name(I0))
      enddo 
      close(20,status="delete")
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
