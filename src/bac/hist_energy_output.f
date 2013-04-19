*========================================================================
* File Name : hist_energy_output.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 西元2010年07月26日 (週一) 17時12分46秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine hist_energy_output
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/histogram.h"
      include "../include/VACF/VACF.h"
      include "../include/tester/hist_energy.h"
      integer total_frame
      file_name=
     &hist_output_file_name(:index(hist_output_file_name," ")-1)
      open(21,file=file_name,status="replace")
      file_name=
     &VACF_hist_xyz_name(:index(VACF_hist_xyz_name," ")-1)
      call clean_file(file_name)  
      write(21,"(A150)")
     &"  #     bin_id     bin_to_pot    lmin_hist_nor     pot_hist_nor 
     &lmin_hist_org     pot_hist_org"
      do I0=1,hist_max_pot
        if(I0.le.hist_max_lmin)then
          if(reflash_hist_pot(I0).eq.1.or.
     &reflash_hist_lmin(I0).eq.1)then
            pot_hist(I0)=global_pot+dble(I0-1)*hist_interval
C            write(*,*) pot_hist(I0),global_pot,hist_interval
            write(21,"(I13,1x,F20.7,1x,F12.10,1x,F12.10,1x,I13,1x,I13)")
     &I0,pot_hist(I0),dble(hist_lmin(I0))/dble(hist_total_lmin),
     &dble(hist_pot(I0))/dble(hist_total_pot),hist_lmin(I0),
     &hist_pot(I0) 
          endif
        else
          if(reflash_hist_pot(I0).eq.1)then
            write(21,"(I13,1x,F20.7,1x,F12.10,1x,F12.10,1x,I13,1x,I13)")
     &I0,pot_hist(I0),0.D0,
     &dble(hist_pot(I0))/dble(hist_total_pot),0,
     &hist_pot(I0)
          endif
        endif
      enddo
      total_frame=0
      do I0=1,hist_max_lmin
        if(reflash_hist_lmin(I0).eq.1)then
          total_frame=total_frame+1
        endif
      enddo
      do I0=1,hist_max_lmin
        if(reflash_hist_lmin(I0).eq.1)then
          do I1=1,ndim
            x(I1)=hist_xyz_lmin(I0,I1)
          enddo
          call pes
          call write_xyz_file(file_name,"app",I0,total_frame,3)
        endif
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
