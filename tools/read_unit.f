*========================================================================
* File Name : read_unit.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Mon 21 Mar 2011 08:15:27 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      function read_unit(unit_type,symbol,in_unit)  ! All character variables must add on space
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      integer len1,len2,len3
      real*8 read_value
      character*30 read_unit_type,read_symbol,read_in_unit
      character*30 unit_type,symbol,in_unit
      logical locate_unit
      locate_unit=.false.
      len1=index(unit_type," ")-1
      len2=index(symbol," ")-1
      len3=index(in_unit," ")-1
      file_name=file_path(:index(file_path," ")-1)//unit_file
      call count_file_line(file_name)
      open(20,file=file_name,status="old")
      do I0=1,file_line_num
        read(20,*)
     &read_unit_type,read_symbol,read_value,read_in_unit
        if(read_unit_type(:len1).eq.unit_type(:len1).and.
     &read_symbol(:len2).eq.symbol(:len2).and.
     &read_in_unit(:len3).eq.in_unit(:len3))then
          read_unit=read_value
C          if(wscreen)write(*,*)symbol,"=",read_unit
          close(20)
          return
        endif
      enddo
      if(.not.locate_unit)then
        write(*,*) "Error, can not locate this unit!!"
        close(20)
        stop
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
