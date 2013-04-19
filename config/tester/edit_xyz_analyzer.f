*========================================================================
* File Name : edit_xyz_analyzer.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 23-09-2010
* Last Modified : Tue 19 Jul 2011 10:11:09 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : 
* Description : 
*========================================================================

      subroutine edit_xyz_analyzer
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/header.h"
      include "../../include/pes.h"
      include "../../include/PTMC/PTMC.h"
      include "../../include/BIMD/BIMD.h"
      real*8 dist_temp(atom_num_max),coord_multiply_factor
      character sel*1,sel2*1
98    write(*,*) "1.Read formatted file"
      write(*,*) "2.Read unformatted file"
      read(*,*)sel
      write(*,*) "Please input the full file name:"
      read(*,*)file_name
      if(sel.eq."1")then   
        write(*,*) "1. xyz (single frame)"
        write(*,*) "2. xyz (a range of frames)"
        write(*,*) "3. pdb (single frame)"
        write(*,*) "4. pdb (a range of frames)"
        read(*,*) sel2
        if(sel.eq.1)then
          call read_xyz_file(file_name)
        else if(sel.eq.2)then
        else if (sel.eq.3)then
          call read_pdb_file(file_name)
        else if(sel.eq.4)then
        endif
      endif
      write(*,*) "1. Correct center of mass(COM)"
      write(*,*) "2. Multiply a factor to the coordinates"
      read(*,*) sel2
      if(sel2.eq."1")then 
        call centroid
        write(*,*) "COM:",ctd_x,ctd_y,ctd_z
        write(*,*) "Will output the atomic distance for each atom"
        open(file=30,file="output.dat",status="replace")
        do I0=1,atom_num
          I1=I0*3
          dist_temp(I0)=dsqrt((x(I1-2)-ctd_x)**2+(x(I1-1)-ctd_y)**2+
     &(x(I1)-ctd_z)**2)
          write(30,*) I0,dist_temp(I0)
        enddo
        close(30)
      endif
      if(sel2.eq."2")then
        write(*,*) "The original coordinates are:"
        do I0=1,atom_num
          I1=I0*3
          write(*,*) atom_name(I0),"atom=",I0,",x=",x(I1-2),",
     &y=",x(I1-1),",z=",x(I1)
        enddo
        write(*,*) "Please input the multiply factor"
        read(*,*) coord_multiply_factor
        write(*,8) "The new coordinates are:"
        open(file=31,file="output.xyz",status="replace")
        write(31,*) atom_num,0
        write(31,*) "pot= ",0
        do I0=1,atom_num
          I1=I0*3
          x(I1-2)=x(I1-2)*coord_multiply_factor
          x(I1-1)=x(I1-1)*coord_multiply_factor
          x(I1)=x(I1)*coord_multiply_factor
          write(*,*) atom_name(I0),",atom=",I0,",x=",x(I1-2),",
     &y=",x(I1-1),",z=",x(I1)
          write(31,*)atom_name(I0),x(I1-2),x(I1-1),x(I1)
        enddo
        write(*,*) "Output the new coordinates to output.xyz"
        close(31)
      endif
      return
      end

      subroutine xyz_n_frame_analyzer(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      integer line_num_per_frame
      call check_file_exist(file_name)
      call count_file_line(file_name)
      open(21,file=file_name,status="old")
      read(21,*) atom_num
      rewind(21)
      total_frame_num=dint(dble(file_line_num)/dble(atom_num+2))
      line_num_per_frame=2+atom_num
      if(total_frame_num.eq.1)then
        init_loop=1
        final_loop=1
        delta_loop=1
      else
        write(*,*) "There are ", total_frame_num, " frame(s)"
        write(*,*) "Each frame has ",atom_num," atoms"
        write(*,*) "======================================="
        write(*,*) "Please input the initial frame number"
        read(*,*) init_loop
        write(*,*) "Please input the final frame number"
        read(*,*) final_loop
        write(*,*) "Please input the frame interval"
        read(*,*) delta_loop
      endif
      call loop_assignment
      do I0=1,init_loop_int-1
        do I1=1,line_num_per_frame
          read(21,*)
        enddo
      enddo
      do I0=1,total_loop_int
        read(21,*)
        read(21,*)
        
      enddo
      do I0=1,delta_loop_int-1
        do I1=1,line_num_per_frame
          read(21,*)
        enddo
      enddo
      close(21)
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
