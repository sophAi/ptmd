*========================================================================
* File Name : corr_main.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Thu 21 Apr 2011 10:50:24 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine corr_main   !For any dimension
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/job.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/header.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/corr.h" 
      include "../../include/tools/correlation.h"
      integer ishift_init,ishift_final,ishift_step_buffer,v_total_index
      integer report_step
      real*8 real_time
C============modified============================
      real*8 vv0(atom_num_max),vv(atom_num_max),mean_vv0,mean_vv
      real*8 ct(atom_num_max),mean_ct
      real*8 v_total(atom_num_max*ndim_fac*time_step_max)
      if(job_skip)return
C===========Write headers=====================
      call corr_header_file
      report_step=corr_observe_time_step/10
      open(23,file=corr_cor_file_name,access="append")
      write(23,"(A12)")"# name: corr"
      write(23,"(A14)")"# type: matrix"
      write(23,"(A8,1x,I13)")"# rows: ",corr_observe_time_step+1
      write(23,"(A11,1x,I5)")"# columns: ",corr_dim+1
      if(wscreen)write(*,"(I5,1x,A30,1x,A80)")
     &myid,"Write correlation function to ",corr_cor_file_name
C=====================================
      if(wscreen)write(*,"(I5,1x,A25)")
     &myid,"Start reading source file"
C      write(*,*) corr_total_time_step,corr_member_num,ndim_fac
      ndim_fac=3
C      call count_file_header_line_unformatted(source_file_name)
      header_line_num=1
      open(20,file=source_file_name,form="unformatted",status="old")
      do I0=1,header_line_num
        read(20)
      enddo
      I4=0
      do I1=1,init_loop_int-1
        read(20)
      enddo
      do I1=1,corr_total_time_step
        read(20) (v(I2),I2=1,ndim)
        do I2=1,corr_member_num
          do I3=1,ndim_fac        
            v_total_index=(corr_total_time_step*
     &((I2-1)*ndim_fac+(I3-1))+I1)
            v_total(v_total_index)=v((I2-1)*ndim_fac+I3)
          enddo
        enddo
        do I4=1,delta_loop_int-1
          read(20)
        enddo
      enddo
C===================Use standard TACF formulation=========================
      if(corr_method.eq.1)then
        if(wscreen)write(*,"(I5,1x,A35)")
     &myid,"Calculate Auto-correlation function"
        mean_ct=0.D0
         mean_vv0=0.D0
        do I1=1,corr_member_num
          vv0(I1)=0.D0
          ct(I1)=0.D0
          do I2=1,ndim_fac   !Calculate vv0
            ishift_step_buffer=
     &corr_total_time_step*((I1-1)*ndim_fac+(I2-1))
            ishift_init=ishift_step_buffer+1
            ishift_final=ishift_step_buffer+corr_total_time_step
            do I3=ishift_init,ishift_final
              vv0(I1)=vv0(I1)+v_total(I3)**2
            enddo
          enddo
          vv0(I1)=vv0(I1)/dble(corr_total_time_step)
          mean_vv0=mean_vv0+vv0(I1)
          ct(I1)=vv0(I1)
        enddo
        mean_vv0=mean_vv0/dble(corr_member_num)
        mean_ct=mean_vv0
        write(23,"(F3.1,1x,1001(1x,F14.6)))") 0.D0,mean_vv0,
     &(vv0(I2),I2=1,corr_member_num)

        do I0=1,corr_observe_time_step
          mean_vv=0.D0
          do I1=1,corr_member_num
            vv(I1)=0.D0
            do I2=1,ndim_fac
              ishift_step_buffer=
     &corr_total_time_step*((I1-1)*ndim_fac+(I2-1))
              ishift_init=ishift_step_buffer+1
              ishift_final=ishift_step_buffer+corr_total_time_step-I0
              do I3=ishift_init,ishift_final
                vv(I1)=vv(I1)+v_total(I3)*v_total(I3+I0)
              enddo
            enddo 
            vv(I1)=vv(I1)/dble(corr_total_time_step-I0)
            mean_vv=mean_vv+vv(I1)
            ct(I1)=ct(I1)+vv(I1)
          enddo
          mean_vv=mean_vv/dble(corr_member_num)
          mean_ct=mean_ct+mean_vv
          real_time=dble(I0)*corr_delta_time
          write(23,"(F15.6,1x,(1001(1x,F14.6)))") real_time,mean_vv,
     &(vv(I2),I2=1,corr_member_num)
          if(wscreen.and.mod(I0,report_step).eq.0)
     &write(*,"(I5,1x,A10,1x,I13,1x,A5)")
     &myid,"Time step=",I0,"done!"
        enddo
C==============Use FFT====================================
      else if(corr_method.eq.2)then

      endif
C=============Output Correlation time======================
      if(wscreen) write(*,"(I5,1x,A26,1x,A80)")
     &myid,"Write correlation time to ",corr_dtf_file_name
      open(24,file=corr_dtf_file_name,access="append")
      write(24,"(A10)")"# name: ct"
      write(24,"(A14)")"# type: matrix"
      write(24,"(A9)")"# rows: 1"
      write(24,"(A11,1x,I5)")"# columns: ",corr_dim
      write(24,"(F15.6,1x,1000(F15.6,1x))")
     &mean_ct,(ct(I2),I2=1,corr_member_num)
      close(23)
      close(24)
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
