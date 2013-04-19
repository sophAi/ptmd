*========================================================================
* File Name : hist_output.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Tue 05 Apr 2011 03:32:18 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine hist_output
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/job.h"
      include "../../include/file.h"
      include "../../include/header.h"
      include "../../include/pes.h"
      include "../../include/tools/histogram.h"
      include "../../include/tools/moment.h"
      include "../../include/tester/hist.h"
      include "../../include/tester/tester.h"
      character*80 octave_hist_header_file*80,
     &octave_moment_header_file*80
      if(job_skip)return
      if(tester_output_flag.eq.1)then
        octave_hist_header_file=
     &hist_file_name(:index(hist_file_name," ")-1)//"o"
        if(wscreen)write(*,"(I5,1x,A34,1x,A80)")
     &myid,"Output histogram octave header to ",octave_hist_header_file
        call write_histogram_octave_header(octave_hist_header_file)
      endif
      file_name=
     &hist_file_name(:index(hist_file_name," ")-1)
      if(wscreen)write(*,"(I5,1x,A20,1x,A80)")
     &myid,"Output histogram to ",file_name
      call write_histogram(file_name)
      total_moment_x_num=1
      total_moment_y_num=hist_dim
      total_moment_z_num=4
      file_y_dim=hist_dim
      file_z_dim=6
      call hist2moment
      if(tester_output_flag.eq.1)then
        octave_moment_header_file=
     &hist_mom_file_name
     &(:index(hist_mom_file_name," ")-1)//"o"
        if(wscreen)write(*,"(I5,1x,A31,1x,A80)")
     &myid,"Output moment octave header to ",octave_moment_header_file
        file_x_dim=1  !total frame num
        call write_moment_octave_header(octave_moment_header_file)      
      endif
      file_name=
     &hist_mom_file_name
     &(:index(hist_mom_file_name," ")-1)
      if(wscreen)write(*,"(I5,1x,A35,1x,A80)")
     &myid,"Output moments of the histogram to ",file_name
      call write_moment_header(file_name,1,1)
      call write_moment(file_name,1) 
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
