*========================================================================
* File Name : edit_tester.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2013-04-19 10:51:43
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_tester
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/ensemble.h"
      include "../../include/job.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/tester/tester.h"
      character yn*1,yn2*1
      write(*,*)"<<<<TESTER MENU>>>>"
105   write(*,*)" 1.Sliding windows with correlation martix"
      write(*,*)" 2.Generating USR file from input xyz"
      write(*,*)" 3.Calculating score function from usr of ufx files"
      write(*,*)" 4.Calculating score function from moment files"
      write(*,*)" 5.Calculating correlation function from ufv files"
C      write(*,*)" 6.Calculating FFT"
      write(*,*)" 6.Calculating histogram"
      write(*,*)" 7.Bond vector for TCF and CT"
      write(*,*)" 8.Convert data to unformatted file"
      write(*,*)" 9.Read and test headers of output file"
C      write(*,*)"10.Rename, move, or delete massive files"
      read(*,*) tester_id
      if(tester_id.eq.1)then
        tester_flag="sliding_correlation"
        call edit_sliding
      else if(tester_id.eq.2)then
        tester_flag="usr_output"
        call edit_usr_output
      else if(tester_id.eq.3)then
        tester_flag="usr_score"
        call edit_usr_score
      else if(tester_id.eq.4)then
        tester_flag="mom_score"
        call edit_mom_score
      else if(tester_id.eq.5)then
        tester_flag="corr"
        call edit_corr
      else if(tester_id.eq.13)then
        tester_flag="fourier"
        call edit_fourier
      else if(tester_id.eq.6)then
        tester_flag="histogram"
        call edit_hist
      else if(tester_id.eq.7)then
        tester_flag="bond_vector"
        call edit_bond_vector 
      else if(tester_id.eq.8)then
        tester_flag="convert_uf"
        call edit_convert_uf
      else if(tester_id.eq.9)then
        tester_flag="read_headers"
        call edit_read_headers
      else
        write(*,*)"Error, please select again!"
        goto 105
      endif
      if(source_type.eq."non")then
        write(*,*)"Detect no source_type. Please select again!"
        call edit_source_type
        call write_source
      endif
      if(source_file_type.eq."non")then
        write(*,*)"Detect no source_file_type. Please select again!"
        call edit_source_file_type
        call write_source
      endif
      return
      end

      subroutine write_tester
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/ensemble.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/tester/tester.h"

      if(tester_flag.eq."sliding_correlation")then
        call write_sliding
      else if(tester_flag.eq."usr_output")then
        call write_usr_output
      else if(tester_flag.eq."usr_score")then
        call write_usr_score
      else if(tester_flag.eq."mom_score")then
        call write_mom_score
      else if(tester_flag.eq."bond_vector")then
        call write_bond_vector
      else if(tester_flag.eq."corr")then
        call write_corr
      else if(tester_flag.eq."fourier")then
        call write_fourier
      else if(tester_flag.eq."histogram")then
        call write_hist
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
