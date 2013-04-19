*========================================================================
* File Name : fourier_init.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 2011-02-07
* Last Modified : Thu 21 Apr 2011 11:14:30 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : 
* Description : 
*========================================================================

      subroutine fourier_init
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/job.h"
      include "../../include/file.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/fourier.h"
      if(job_skip)return
      if(source_file_flag.eq.1.or.source_file_flag.eq.2)then
        call read_unformatted_header(header_file_name,"ndim_fac ")
        call read_unformatted_header(header_file_name,"file_x_dim ")
        call read_unformatted_header(header_file_name,"file_y_dim ")
        fourier_dim=file_y_dim-1
      endif
      fourier_window_time_step=total_loop_int
      if(file_x_dim.gt.fourier_window_time_step)then
        final_loop=init_loop+dble(fourier_window_time_step)*delta_loop-1
        if(wscreen)write(*,"(I5,1x,A36,1x,F15.1)")
     &myid,"Warning!!final loop exceed,reset to ",final_loop
      endif
      if(wscreen)then
        if(fourier_method.eq.1)then
          write(*,"(I5,1x,A41)")
     &myid,"Calculate discrete Fourier transformation"
        elseif(fourier_method.eq.2)then
          write(*,"(I5,1x,A37)")
     &myid,"Calculate Fast Fourier transformation"
        endif
        write(*,"(I5,1x,A16,1x,I13)")
     &myid,"Window time step=",fourier_window_time_step
        write(*,"(I5,1x,A11,1x,F11.7)")
     &myid,"delta time=",fourier_delta_time
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
