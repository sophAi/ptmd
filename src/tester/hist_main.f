*========================================================================
* File Name : hist_main.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Mon 25 Apr 2011 03:08:53 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================

      subroutine hist_main
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/job.h"
      include "../../include/pes.h"
      include "../../include/tools/histogram.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/hist.h"
      if(hist_method.eq.1)
     &call hist_unformatted_eng   !read ufe file output energy histogram
      if(hist_method.eq.2)
     &call hist_unformatted_lmin  !read ufx file and output energy local minima histogram
      if(hist_method.eq.3)
     &call hist_unformatted_grad
      if(hist_method.eq.4)
     &call hist_xyz_pdb           !The same with 2
      if(hist_method.eq.5)        
     &call hist_dat               !Read a dat file
      if(hist_method.eq.6)
     &call hist_dat               !Read from scr
      if(hist_method.eq.7)
     &call hist_psd               !Read from psd
      return
      end

      subroutine hist_unformatted_eng
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/job.h"
      include "../../include/file.h"
      include "../../include/header.h"
      include "../../include/pes.h"
      include "../../include/tools/histogram.h"
      include "../../include/tester/hist.h"
      integer before_read_hist_col
      character read_source_type*3
      if(job_skip)return
      call histogram_init
      before_read_hist_col=0
      call read_unformatted_header(header_file_name,"source_type ")
      call read_unformatted_header(header_file_name,"file_x_dim ")
      call read_unformatted_header(header_file_name,"file_y_dim ")
      hist_dim=file_y_dim
      if(file_x_dim.lt.total_loop_int)then
        total_loop_int=file_x_dim
        if(wscreen)write(*,"(I5,1x,A36,1x,I12)")
     &myid,"Warning!!total loop exceed,reset to ",total_loop_int
      endif
C      call count_file_header_line_unformatted(source_file_name)
      header_line_num=1
      open(21,file=source_file_name,
     &form="unformatted",status="old")
      do I0=1,init_loop_int-1+header_line_num  !include the header lines
        read(21)
      enddo
      if(wscreen)then
        write(*,"(I5,1x,A13,1x,A80)")
     &myid,"Reading from ",source_file_name
        write(*,"(I5,1x,A26)")myid,"Start generating histogram"
      endif
      do I0=1,total_loop_int
        read(21)(hist_read_value(I2),I2=1,hist_dim)
C        write(*,*)hist_dim,(hist_read_value(I2),I2=1,hist_dim)
C        pause
        call histogram
        do I1=1,delta_loop_int-1
          read(21)
        enddo
      enddo
C      do I0=1,hist_dim
C        write(*,*) I0,hist_total_num(I0),hist_missing_num(I0)
C      enddo
      close(21)
      header_annotation=
     &"'pot  hist_binding_pot hist_binding_pot_per_atom(1)~(n)'"
      call hist_output
      return
      end

      subroutine hist_unformatted_lmin
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/job.h"
      include "../../include/file.h"
      include "../../include/header.h"
      include "../../include/pes.h"
      include "../../include/tools/histogram.h"
      include "../../include/tester/hist.h"
      integer before_read_hist_col
      real*8 dummy_real,read_x(ndim_max),hist_rec_lmin(hist_bin_max),
     &hist_rec_lmin_xyz(hist_bin_max,ndim_max)
      character read_source_type*3
      if(job_skip)return
      call histogram_init
      hist_total_num=0
      do I0=1,hist_bin_max
        hist_rec_lmin(I0)=0.D0
        do I1=1,ndim
          hist_rec_lmin_xyz(I0,I1)=0.D0
        enddo
      enddo
      before_read_hist_col=0
      call read_unformatted_header(header_file_name,"source_type ")
      call read_unformatted_header(header_file_name,"file_x_dim ")
      call read_unformatted_header(header_file_name,"ndim ")
      atom_num=ndim/3
      hist_dim=atom_num+1  !will compute pot and pot_per_atom
      if(file_x_dim.lt.total_loop_int)then
        total_loop_int=file_x_dim
        if(wscreen)write(*,"(I5,1x,A36,1x,I12)")
     &myid,"Warning!!total loop exceed,reset to ",total_loop_int
      endif
      min_PES_method=1
C      call count_file_header_line_unformatted(source_file_name)
      header_line_num=1
      open(21,file=source_file_name,
     &form="unformatted",status="old")
      do I0=1,init_loop_int-1+header_line_num  ! include the header lines
        read(21)
      enddo
      if(wscreen)then
        write(*,"(I5,1x,A13,1x,A80)")
     &myid,"Reading from ",source_file_name
        write(*,"(I5,1x,A26)")myid,"Start generating histogram"
      endif
      do I0=1,total_loop_int
        read(21) (x(I1),I1=1,ndim)
C        write(*,*)"pot before=",pot
        call min_PES
C        write(*,*)"pot after=",pot
C        pause
        hist_read_value(1)=pot/dble(atom_num)
        do I1=1,atom_num
          hist_read_value(1+I1)=pot_per_atom(I1)
        enddo 
        call histogram
        if(hist_rec_lmin(hist_bin_id(1)).eq.0.D0)then
          hist_rec_lmin(hist_bin_id(1))=hist_read_value(1)
          do I1=1,ndim
            hist_rec_lmin_xyz(hist_bin_id(1),I1)=x(I1)
          enddo
        endif 
        do I1=1,delta_loop_int-1
          read(21)
        enddo
      enddo
      close(21)
      header_annotation=
     &"'pot  hist_binding_lmin hist_binding_lmin_per_atom(1)~(n)'"
      call hist_output
      if(wscreen)write(*,"(I5,1x,A28,1x,A80)")myid,
     &"Output optimal structure to ",hist_min_xyz_file_name
      call clean_file(hist_min_xyz_file_name)
      do I0=1,hist_bin_max
        if(hist_rec_lmin(I0).ne.0.D0)then
          pot=hist_rec_lmin(I0)
          do I1=1,ndim
            x(I1)=hist_rec_lmin_xyz(I0,I1)
          enddo
          call write_xyz_file_simple(hist_min_xyz_file_name,"app",I0)
        endif
      enddo
      return
      end

      subroutine hist_unformatted_grad
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/job.h"
      include "../../include/file.h"
      include "../../include/header.h"
      include "../../include/pes.h"
      include "../../include/tools/histogram.h"
      include "../../include/tester/hist.h"
      integer before_read_hist_col
      real*8 dummy_real,read_x(ndim_max)
      character read_source_type*3
      if(job_skip)return
      call histogram_init
      before_read_hist_col=0
      call read_unformatted_header(header_file_name,"source_type ")
      call read_unformatted_header(header_file_name,"file_x_dim ")
      call read_unformatted_header(header_file_name,"ndim ")
      atom_num=ndim/3
      hist_dim=atom_num+1  !will compute norm and rms norm
      if(file_x_dim.lt.total_loop_int)then
        total_loop_int=file_x_dim
        if(wscreen)write(*,"(I5,1x,A36,1x,I12)")
     &myid,"Warning!!total loop exceed,reset to ",total_loop_int
      endif
      min_PES_method=1
C      call count_file_header_line_unformatted(source_file_name)
      header_line_num=1
      open(21,file=source_file_name,
     &form="unformatted",status="old")
      do I0=1,init_loop_int-1+header_line_num  !include the header lines
        read(21)
      enddo
      if(wscreen)then
       write(*,"(I5,1x,A13,1x,A80)")
     &myid,"Reading from ",source_file_name
        write(*,"(I5,1x,A26)")myid,"Start generating histogram"
      endif
      do I0=1,total_loop_int
        read(21) (grad(I1),I1=1,ndim)
        hist_read_value(1)=0.D0
        do I1=1,atom_num
          I2=I1*3
          hist_read_value(1+I1)=grad(I2-2)**2+grad(I2-1)**2+grad(I2)**2
          hist_read_value(1)=hist_read_value(1)+hist_read_value(1+I1)
          hist_read_value(1+I1)=dsqrt(hist_read_value(1+I1))
        enddo
        hist_read_value(1)=dsqrt(hist_read_value(1))
        call histogram
        do I1=1,delta_loop_int-1
          read(21)
        enddo
      enddo
      close(21)
      header_annotation="'grad  hist_grad_rms hist_grad(1)~(n)'"
      call hist_output
      return
      end


      subroutine hist_dat
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/job.h"
      include "../../include/file.h"
      include "../../include/header.h"
      include "../../include/pes.h"
      include "../../include/tools/histogram.h"
      include "../../include/tester/hist.h"
      integer before_read_hist_col,dummy_index_num
      character read_source_type*3
      real*8 dummy_index(2)
      if(job_skip)return
      call read_formatted_header(header_file_name,"file_x_dim ")
      call read_formatted_header(header_file_name,"file_y_dim ")
      if(file_y_dim.eq.1)then
        dummy_index_num=0
        hist_dim=1
      else
        dummy_index_num=1
        hist_dim=file_y_dim-1
      endif
      call histogram_init
      call count_file_header_line(source_file_name)
      open(21,file=source_file_name,status="old")
      do I0=1,init_loop_int-1+header_line_num  !include the header line
        read(21,*)
      enddo
      if(wscreen)then
        write(*,"(I5,1x,A13,1x,A80)")
     &myid,"Reading from ",source_file_name
        write(*,"(I5,1x,A26)")myid,"Start generating histogram"
      endif
      do I0=1,total_loop_int
        read(21,*)(dummy_index(I2),I2=1,dummy_index_num),
     &(hist_read_value(I1),I1=1,hist_dim)
        call histogram
        do I1=1,delta_loop_int-1
          read(21,*)
        enddo
      enddo
      close(21)
C      do I0=1,hist_dim
C        write(*,*) I0,hist_total_num(I0),hist_missing_num(I0)
C      enddo
      header_annotation="'hist_x  hist_y1~hist_yn'"
      call hist_output
      return
      end
 
      subroutine hist_scr
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/job.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/header.h"
      include "../../include/tools/histogram.h"
      include "../../include/tester/hist.h"
      integer before_read_hist_col
      character read_source_type*3,dummy1*2
      if(job_skip)return
      call histogram_init
      before_read_hist_col=0
      call count_file_header_line(source_file_name)
      open(21,file=source_file_name,status="old")
      do I0=1,init_loop_int-1+header_line_num !include the header lines
        read(21,*)
      enddo
      if(wscreen)then
        write(*,"(I5,1x,A13,1x,A80)")
     &myid,"Reading from ",source_file_name
        write(*,"(I5,1x,A26)")myid,"Start generating histogram"
      endif
      do I0=1,total_loop_int
        read(21,*)dummy1,hist_read_value
        call histogram
        do I1=1,delta_loop_int-1
          read(21,*)
        enddo
      enddo
      close(21)
      header_annotation="'scr0~1  hist_scr(1)~scr(n)'"
      call hist_output
      return
      end
 
      subroutine hist_psd
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/job.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/tools/histogram.h"
      include "../../include/tester/hist.h"
      integer before_read_hist_col
      real*8 dummy2(atom_num_max)
      character read_source_type*3,dummy1*2
      if(job_skip)return
      before_read_hist_col=0
      call count_file_header_line(source_file_name)
      open(21,file=source_file_name,status="old")
      do J0=1,atom_num+1
        call histogram_init
        do I0=1,init_loop_int-1+header_line_num !include the header lines
          read(21,*)
        enddo
        if(wscreen)then
          write(*,"(I5,1x,A13,1x,A80)")
     &myid,"Reading from ",source_file_name
          write(*,"(I5,1x,A26)")myid,"Start generating histogram"
        endif
        do I0=1,total_loop_int
          read(21,*)(dummy2(J1),J1=1,J0),hist_read_value
          call histogram
          do I1=1,delta_loop_int-1
            read(21,*)
          enddo
        enddo
!  output here

      enddo
      close(21)
      return
      end


 
      subroutine hist_xyz_pdb
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/job.h"
      include "../../include/pes.h"
      include "../../include/tools/histogram.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/hist.h"
      integer read_num,skip_line_start,skip_line_end   !skip_line_start and skip_line_end is applicable for both pdb and xyz format
      integer read_x_col,total_line_per_frame
      real*8 hist_norm
      character dummy(10)*3
      if(job_skip)return
C=======Read atom_num====================
      call histogram_init
      if(source_file_type.eq."xyz")then
        open(19,file=source_file_name,status="old")
        read(19,*)atom_num
        close(19)
        skip_line_start=2 !Atom_num ,Description_line
        skip_line_end=0  !None
        read_x_col=2 !Start from the 2nd column
        read_num=atom_num
      else if(source_file_type.eq."pdb")then
        call check_pdb(source_file_name,skip_line_start,
     &skip_line_end)  
        read_x_col=6     !Start from the 6th column
C        write(*,*) "skip=",skip_line_start,skip_line_end,atom_num
        read_num=atom_num
      else if(source_file_type.eq."ufx")then
      endif
C======Start converting the hist vectors==
      open(21,file=source_file_name,status="old")
      call clean_file(hist_file_name)
C======Jump to init_loop(can be >1)===
      total_line_per_frame=read_num+skip_line_start+skip_line_end
      do I0=1,init_loop_int-1
        do I1=1,total_line_per_frame
          read(21,*)
        enddo 
      enddo
C=======Start from init_loop==========
      do I0=1,total_loop_int
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
c        if(hist_method.eq.0)call histogram_pot_lmin
C        if(hist_method.eq.1)call histogram_pot
C        if(hist_method.eq.2)call histogram_lmin
C=======construct hist
        do I1=1,skip_line_end   ! Skip the end lines
          read(21,*)
        enddo
        do I1=1,delta_loop_int-1 !Delta loop skip frames
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
