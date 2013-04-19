*========================================================================
* File Name : edit_read_headers.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 23-09-2010
* Last Modified : Wed 04 May 2011 10:53:28 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : 
* Description : 
*========================================================================

      subroutine edit_read_headers
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/header.h"
      include "../../include/pes.h"
      include "../../include/simulation.h"
      include "../../include/PTMC/PTMC.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/BIMD/BIMD_restore.h"
      integer frame_num,start_loop_int,restore_max_loop_int
      character read_header*25,sel*1
98    write(*,*) "1.Read formatted file"
      write(*,*) "2.Read unformatted file"
      write(*,*) "3.Read unformatted BIMD rec file"
      read(*,*)sel
      write(*,*) "Please input the full file name:"
      read(*,*)file_name
99    if(sel.eq."1")then
        call show_formatted_header(file_name)
      else if(sel.eq."2")then
        call show_unformatted_header(file_name)
      else if(sel.eq."3")then  
        write(*,*) "Please input the frame number (start from 0)"
        read(*,*) frame_num
        simulation_restore_file=file_name(:index(file_name," ")-1)
        call BIMD_restore(frame_num,start_loop_int,restore_max_loop_int)
        write(*,*) "time_label=",time_label
        write(*,*) "start_loop=",start_loop_int
        write(*,*) "simulation_rec_loop=",simulation_rec_loop
        write(*,*) "simulation_delta_time=",simulation_delta_time
        write(*,*) "temp=",temp
        write(*,*) "atom_num=",atom_num
     
      else 
        write(*,*) "Error,please select again!"
        goto 98
      endif
      write(*,*) "Please input the parameter name:(Ctrl+C to quit)"
      read(*,*) read_header
      if(sel.eq."1")call read_formatted_header(file_name,read_header)
      if(sel.eq."2")call read_unformatted_header(file_name,read_header)
      if(header_found)then
        write(*,*) "source_type=",header_source_type,",read value=",
     &header_output
      else
        write(*,*) "Header not found. Please try again."
      endif
      goto 99
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
