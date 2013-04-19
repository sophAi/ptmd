*========================================================================
* File Name : fourier_main.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 2011-02-07
* Last Modified : Thu 21 Apr 2011 11:14:52 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : 
* Description : 
*========================================================================

      subroutine fourier_main
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/fourier.h"
      if(fourier_method.eq.1)then
C        call DFT_main
      endif
      if(fourier_method.eq.2)then
C        call FFT main
      endif
      return
      end

      subroutine DFT_main   !Discrete Fourier Transformation
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/job.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/header.h"
      include "../../include/tester/fourier.h"
      include "../../include/tools/ft.h"
      integer total_loop_int_fixed
      real*8 read_value(observe_time_step_max),pi,theta,theta_t
      if(job_skip)return
      if(wscreen)write(*,"(I5,1x,A25)")
     &myid,"Start reading source file"
C      call power2array(total_loop_int,total_loop_int_fixed)
C      if(wscreen)write(*,"(I5,1x,A40,1x,I13)")
C     &myid,"Fixed the total loops to the power of 2=",
C     &total_loop_int_fixed
      open(30,file=source_file_name,status="old")
      theta=2.D0*pi/dble(2*total_loop_int)
      do I0=1,fourier_dim
        read(30,*)! first line is the definition
        do I1=1,init_loop_int-1
          read(30,*)
        enddo
        do I1=1,total_loop_int
C          read(30,*) fourier_time(I1),(read_temp(I2),
C     &I2=1,fourier_dim-1),read_value(I1)
        enddo
        do I1=total_loop_int+1,total_loop_int_fixed
          read_value(I1)=0.D0
        enddo
        if(wscreen)write(*,"(I5,1x,A12,1x,I4,1x,A9)")
     &myid,"FFT for index",I0,"completed"
        call realft(read_value,total_loop_int_fixed,1)
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
      theta_t=theta/fourier_delta_time
      file_name=
     &fourier_dft_file_name(:index(fourier_dft_file_name," ")-1)
      open(25,file=file_name,status="replace")
      file_name=
     &fourier_dft_file_name(:index(fourier_dft_file_name," ")-1)
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
     &(read_value(I0)*fourier_delta_time,I0=1,fourier_dim)
        endif
      enddo
102   format(F14.7,1x,F14.7,1000(1x,F14.7))
      close(25)
      close(26)
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
