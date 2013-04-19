*========================================================================
* File Name : pdb_file.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Thu 24 Mar 2011 04:34:27 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine read_pdb_file(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      integer frame_num,total_frame_num,read_frame
      integer skip_line_start,skip_line_end
      real*8 second_real8
      character dummy(5)*4
      file_line_num=0
      call check_file_exist(file_name)
      call count_file_line(file_name)
      call check_pdb(file_name,skip_line_start,skip_line_end)
      open(20,file=file_name,status="old")
      total_frame_num=dint(dble(file_line_num)/
     &dble(atom_num+skip_line_start+skip_line_end))
      if(total_frame_num.eq.1)then
        frame_num=1
      else
        write(*,*) "There are ", total_frame_num, " frame(s)"
        write(*,*) "Please select a frame"
        read(*,*) frame_num
        read_frame=0
        do while(read_frame.ne.frame_num-1)
          read_frame=read_frame+1
          do I1=1,atom_num+skip_line_start+skip_line_end
            read(20,*)        
          enddo  
        enddo
      endif
      do I1=1,skip_line_start
        read(20,*)
      enddo
      do I1=1,atom_num
        I2=I1*3
        read(20,*) (dummy(I3),I3=1,5),x(I2-2),x(I2-1),x(I2)
        atom_name(I1)=dummy(3)
      enddo
      close(20)
      return
      end

      subroutine check_pdb(file_name,skip_line_start,skip_line_end)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      integer skip_line_start,skip_line_end,total_col_num_per_frame
      character read_dummy*3
      atom_num=0
      skip_line_start=0
      skip_line_end=1
      total_col_num_per_frame=0
      open(20,file=file_name,status="old")
      read(20,*)read_dummy
      do while(read_dummy.ne."ATO".and.read_dummy.ne."Ato".and.
     &read_dummy.ne."ato")
        read(20,*)read_dummy
        skip_line_start=skip_line_start+1
      enddo
      do while(read_dummy.eq."ATO".or.read_dummy.eq."Ato".or.
     &read_dummy.eq."ato")
        read(20,*)read_dummy
        atom_num=atom_num+1
      enddo
      do while(read_dummy.ne."END".and.read_dummy.ne."End".and.
     &read_dummy.ne."end")
        read(20,*)read_dummy
        skip_line_end=skip_line_end+1
      enddo
      ndim=atom_num*3
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
