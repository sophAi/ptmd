*========================================================================
* File Name : hist_energy_main.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 西元2010年07月26日 (週一) 17時17分37秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================

      subroutine hist_energy_main
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/job.h"
      include "../../include/pes.h"
      include "../../include/histogram.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/hist_energy.h"
      if(source_type.eq."pdb".or.source_type.eq."xyz")
     &call hist_energy_xyz_pdb
      if(source_type.eq."ufx".or.source_type.eq."pot")
     &call hist_energy_ufx_pot
      return
      end



      subroutine hist_energy_ufx_pot
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/job.h"
      include "../../include/pes.h"
      include "../../include/histogram.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/hist_energy.h"
      integer hist_init_loop_int,hist_final_loop_int,
     &hist_delta_loop_int,hist_total_loop_int
      character dummy(20)*1
      if(job_skip)return
      call histogram_init
      open(21,file=hist_input_file_name,status="old")
      call clean_file(hist_output_file_name)
      hist_init_loop_int=dint(init_loop)
      hist_final_loop_int=dint(final_loop)
      hist_delta_loop_int=dint(delta_loop)
      hist_total_loop_int=dint((final_loop-init_loop+1)/delta_loop)
      read(21,*) (dummy(I1),I1=1,4),ndim
      rewind(21)
      atom_num=ndim/3
      if(source_type.eq."pot")hist_method=1
C======Jump to init_loop(can be >1)===
      if(hist_method.eq.0)then  !calculate pot and lmin
        do I0=1,hist_init_loop_int-1
          read(21,*)
        enddo
C=======Start from init_loop==========
        do I0=1,hist_total_loop_int
          read(21,*) (dummy(I1),I1=1,3),pot,dummy(20),(x(I2),I2=1,ndim)
          call histogram_pot_lmin
C=======retrive Coordinates===========
          do I1=1,hist_delta_loop_int-1 !Delta loop skip frames
            read(21,*)
          enddo
        enddo
      else if(hist_method.eq.1)then !calculate pot only
        do I0=1,hist_init_loop_int-1
          read(21,*)
        enddo
C=======Start from init_loop==========
        do I0=1,hist_total_loop_int
          read(21,*) (dummy(I1),I1=1,3),pot
          call histogram_pot
C=======retrive Coordinates===========
          do I1=1,hist_delta_loop_int-1 !Delta loop skip frames
            read(21,*)
          enddo
        enddo
      else if(hist_method.eq.2)then !calculate lmin only
        do I0=1,hist_init_loop_int-1
          read(21,*)
        enddo
C=======Start from init_loop==========
        do I0=1,hist_total_loop_int
          read(21,*) (dummy(I1),I1=1,3),pot,dummy(20),(x(I2),I2=1,ndim)
          call histogram_lmin
C=======retrive Coordinates===========
          do I1=1,hist_delta_loop_int-1 !Delta loop skip frames
            read(21,*)
          enddo
        enddo
      endif
      close(21)
      return
      end 
 
      subroutine hist_energy_xyz_pdb
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/job.h"
      include "../../include/pes.h"
      include "../../include/histogram.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/hist_energy.h"
      integer hist_init_loop_int,hist_final_loop_int,read_num,
     &hist_delta_loop_int,hist_total_loop_int,
     &skip_line_start,skip_line_end   !skip_line_start and skip_line_end is applicable for both pdb and xyz format
      integer read_x_col,total_line_per_frame
      real*8 hist_norm
      character dummy(20)*1
      if(job_skip)return
C=======Read atom_num====================
      call histogram_init
      if(source_type.eq."xyz")then
        open(19,file=hist_input_file_name,status="old")
        read(19,*)atom_num
        close(19)
        skip_line_start=2 !Atom_num ,Description_line
        skip_line_end=0  !None
        read_x_col=2 !Start from the 2nd column
        read_num=atom_num
      else if(source_type.eq."pdb")then
        call check_pdb(hist_input_file_name,skip_line_start,
     &skip_line_end)  
        read_x_col=6     !Start from the 6th column
C        write(*,*) "skip=",skip_line_start,skip_line_end,atom_num
        read_num=atom_num
      else if(source_type.eq."ufx")then
      endif
C======Start converting the hist vectors==
      open(21,file=hist_input_file_name,status="old")
      call clean_file(hist_output_file_name)
      hist_init_loop_int=dint(init_loop)
      hist_final_loop_int=dint(final_loop)
      hist_delta_loop_int=dint(delta_loop)
      hist_total_loop_int=dint((final_loop-init_loop+1)/delta_loop)
C======Jump to init_loop(can be >1)===
      total_line_per_frame=read_num+skip_line_start+skip_line_end
      do I0=1,hist_init_loop_int-1
        do I1=1,total_line_per_frame
          read(21,*)
        enddo 
      enddo
C=======Start from init_loop==========
      do I0=1,hist_total_loop_int
        do I1=1,skip_line_start-1  ! Skip the header lines
          read(21,*)
        enddo
        read(21,*)dummy(1),pot
C=======retrive Coordinates===========
        do I1=1,read_num
          I2=I1*3
          read(21,*)
     &(dummy(I3),I3=1,read_x_col-1),x(I2-2),x(I2-1),x(I2)
        enddo
        if(hist_method.eq.0)call histogram_pot_lmin
        if(hist_method.eq.1)call histogram_pot
        if(hist_method.eq.2)call histogram_lmin
C=======construct hist_energy
        do I1=1,skip_line_end   ! Skip the end lines
          read(21,*)
        enddo
        do I1=1,hist_delta_loop_int-1 !Delta loop skip frames
          do I2=1,total_line_per_frame
            read(21,*)
          enddo
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
