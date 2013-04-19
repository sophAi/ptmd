*========================================================================
* File Name : DFT_main.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Mon 28 Feb 2011 03:52:43 PM CST
* Last Modified : Mon 28 Feb 2011 03:52:43 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine DFT_main   !Discrete Fourier Transformation
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/job.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/header.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/tester/ft.h" 
      integer total_loop_int_fixed
      real*8 read_value(observe_time_step_max)
      if(job_skip)return
      if(wscreen)write(*,"(I5,1x,A25)") 
     &myid,"Start reading source file"
      init_loop_int=dint(init_loop)
      final_loop_int=dint(final_loop)
      delta_loop_int=dint(delta_loop)
      total_loop_int=dint((final_loop-init_loop+1.D0)/delta_loop)
      call power2array(total_loop_int,total_loop_int_fixed)
      if(wscreen)write(*,"(I5,1x,A40,1x,I13)")
     &myid,"Fixed the total loops to the power of 2=",
     &total_loop_int_fixed
      open(30,file=source_file_name,status="old")
C     gvacf_observe_time_step is the time step that total data points of gvacf vs time
C     gvacf_window_time_step is the time window used in FFT and must be <= gvacf_observe_time_step
      theta=2.D0*pi/dble(2*total_loop_int)
      do I0=1,fourier_dim
        read(30,*)! first line is the definition
        do I1=1,init_loop_int-1
          read(30,*)
        enddo
        do I1=1,total_loop_int
          read(30,*) fft_time(I1),(read_temp(I2),I2=1,fourier_dim-1),
     &read_value(I1)
        enddo
        do I1=total_loop_int+1,total_loop_int_fixed
          read_value(I1)=0.D0
        enddo 
        if(wscreen)write(*,"(I5,1x,A12,1x,I4,1x,A9)")
     &myid,"FFT for index",I0,"completed"
        call readft(read_value,total_loop_int_fixed,1)
        do I1=1,delta_loop_int-1
          read(30,*)
        enddo
        rewind(30)
      enddo
      if(wscreen)write(*,"(I5,1x,A22)")
     &myid,"FFT completed"
C=============Output DFT==========================================
      pi=read_unit("Pi ","Pi ","1 ")
      theta=2.D0*pi/dble(2*final_loop+1)   !w=(2*pi*k)/N and F=F*dt 
      theta_t=theta/gvacf_delta_time
      file_name=
     &gvacf_real_psd_file_name(:index(gvacf_real_psd_file_name," ")-1)
      open(25,file=file_name,status="replace")
      file_name=
     &gvacf_img_psd_file_name(:index(gvacf_img_psd_file_name," ")-1)
      open(26,file=file_name,status="replace")
      write(25,"(A62,I5,A1)")
     &"# freq_in_rad DFT_mean_real DFT_real_atom(1) to DFT_real_atom("
     &,atom_num,")"
      write(26,"(A65,I5,A1)")
     &"# freq_in_rad DFT_mean_image DFT_image_atom(1) to DFT_image_atom(
     &",atom_num,")"
C      do I1=-(gvacf_observe_time_step-1),gvacf_observe_time_step-1
      do I1=1,total_loop_int_fixed
        if(dabs(read_value(I1)).gt.0.001D0)then
          write(25,102) theta_t*dble(I1),
     &(read_value(I0)*gvacf_delta_time,I0=1,fourier_dim)
        endif
      enddo
102   format(F14.7,1x,F14.7,1000(1x,F14.7))
      close(25)
      close(26)
C========End of DFT and output======================================
C=============gvacf Option:Output and delete xyz===================
C      if(gvacf_option.eq.1)then   !Delete recorded xyz file only!
C        if(wscreen)write(*,"(I5,1x,A32)")myid,
C     &"Start deleting recorded xyz file"        
C        call clean_file(source_file_name)
C      endif
C      if(gvacf_option.eq.2)then   !Delete recorded xyz file and rename gvacf output
C        if(wscreen)write(*,"(I5,1x,A32)")myid,
C     &"Start deleting recorded xyz file"        
C        call clean_file(source_file_name)
C        if(wscreen)write(*,"(I5,1x,A32)")myid,
C     &"Start renaming gvacf output files"        
C        call gvacf_rename
C      endif
C      if(gvacf_option.eq.3)then   !Rename gvacf output only(add time label)
C        if(wscreen)write(*,"(I5,1x,A32)")myid,
C     &"Start renaming gvacf output files" 
C        call gvacf_rename
C      endif
C=================================================================
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
