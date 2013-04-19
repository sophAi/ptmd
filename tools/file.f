*========================================================================
* File Name : file.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Thu 24 Mar 2011 12:53:20 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine check_file_exist(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      integer len1
      file_exist=.false.
      len1=index(file_name," ")-1
C      do while(.not.file_exist)
      inquire(file=file_name(:len1),exist=file_exist)
      if(.not.file_exist)then
        if(wscreen)write(*,"(I5,1x,A11,1x,A80)") 
     &myid,"Can't find ",file_name
C          write(*,*) "Please prepare this file : ",file_name
C          write(*,*) "If you done, press Enter and continue."
C          write(*,*) "Otherwise, restart the program when you're ready"
C          pause
      else
C          if(wscreen)write(*,"I5,1x,A13,1x,A80") 
C     &myid,"File existed=",file_name
      endif
C      enddo
      return
      end

      subroutine count_file_header_line(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/header.h"
      integer len1
      character dummy1*1
      header_line_num=0
      len1=index(file_name," ")-1
      inquire(file=file_name,exist=file_exist)
      if(file_exist)then
        open(21,file=file_name(:len1),form="formatted",status="old")
99      read(21,*,end=100)dummy1
        if(dummy1.ne."#")then
          close(21)
          return
        else
          header_line_num=header_line_num+1
        endif
        goto 99
100     rewind(21)
        close(21)       
      else
        write(*,*) "Error!!Can not locate file=",file_name
        stop
      endif
      return
      end

      subroutine count_file_header_line_unformatted(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/header.h"
      integer len1
      character dummy1*1
      header_line_num=0
      len1=index(file_name," ")-1
      inquire(file=file_name,exist=file_exist)
      if(file_exist)then
        open(21,file=file_name(:len1),form="unformatted",status="old")
99      read(21,end=100)dummy1
        if(dummy1.ne."#")then
          close(21)
          return
        else
          header_line_num=header_line_num+1
        endif
        goto 99
100     rewind(21)
        close(21)
      else
        write(*,*) "Error!!Can not locate file=",file_name
        stop
      endif
      return
      end




      subroutine count_file_line(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      integer len1
      character dummy1*30
      file_line_num=0
      len1=index(file_name," ")-1
      inquire(file=file_name,exist=file_exist)
      if(file_exist)then
        open(99,file=file_name(:len1),form="formatted",status="old")
99      read(99,*,end=100)dummy1
        file_line_num=file_line_num+1
        goto 99
100     rewind(99)
        close(99)
      else
        write(*,*) "Error!!Can not locate file=",file_name
        stop
      endif
      return
      end

      subroutine count_file_line_unformatted(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      integer len1
      character dummy1*30
      file_line_num=0
      len1=index(file_name," ")-1
      inquire(file=file_name,exist=file_exist)
      if(file_exist)then
        open(21,file=file_name(:len1),form="unformatted",status="old")
15      read(21,end=17)dummy1
        file_line_num=file_line_num+1
        goto 15
17      rewind(21)
        close(21)
      else
        write(*,*) "Error!! file doesn't exist!"
        stop
      endif
      return
      end

      subroutine clean_file(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      integer len1
      len1=index(file_name," ")-1
      open(20,file=file_name(:len1),status="replace")
      close(20,status="delete")
      return
      end

      subroutine count_file_row(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      integer len1
      character dummy1(50)*30
      file_row_num=0
      len1=index(file_name," ")-1
      call count_file_line(file_name(:len1))
      open(20,file=file_name(:len1),status="old")
      do J1=1,file_line_num-1
        read(20,*)
      enddo
13    file_row_num=file_row_num+1
      read(20,*,end=15) (dummy1(I1),I1=1,file_row_num)
      backspace(20)
      goto 13
15    file_row_num=file_row_num-1
      close(20)
      return
      end

      subroutine wait_till_file_close(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
11    inquire(file=file_name,opened=file_opened)
      if(file_opened)goto 11       !make sure no one is opening it
      return
      end

      subroutine wait_till_file_exist(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/job.h"
11    inquire(file=file_name,exist=file_exist)
      if(.not.file_exist)then
        do I0=1,job_point_id*100
          I1=1
        enddo
        goto 11       !make sure no one is opening it
      endif
      return
      end

      subroutine check_file_name(file_name,file_main,file_ext)
! Input file_name, return file_main and file_ext
! file_main is the file_name without extension, file_ext is the extension part.
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/job.h"
      file_main=file_name(:index(file_name,".")-1)
      file_ext=file_name(index(file_name,".")+1:index(file_name," ")-1)
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
