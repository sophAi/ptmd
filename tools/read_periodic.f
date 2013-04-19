*========================================================================
* File Name : read_periodic.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 2011-03-21
* Last Modified : Thu 24 Mar 2011 11:42:29 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : 
* Description : 
*========================================================================

      subroutine periodic_init
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/periodic.h"
      file_name=file_path(:index(file_path," ")-1)//periodic_file
      call count_file_header_line(file_name)
      open(21,file=file_name,status="old")
      do I0=1,header_line_num
        read(21,*)
      enddo
      periodic_total_num=0
99    periodic_total_num=periodic_total_num+1
      read(21,*,end=100) periodic_atomic_num(periodic_total_num),
     &periodic_element(periodic_total_num),
     &periodic_atomic_mass(periodic_total_num)
      goto 99
100   close(21)
      return
      end

      subroutine periodic_mass
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/periodic.h"
      include "../include/pes.h"
      do 99 I0=1,atom_num
        mass(I0)=0.D0
        periodic_id(I0)=0
        do I1=1,periodic_total_num
          if(atom_name(I0).eq.periodic_element(I1))then
            mass(I0)=periodic_atomic_mass(I1)
            periodic_id(I0)=I1
            goto 99
          endif
        enddo
99    continue          
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
