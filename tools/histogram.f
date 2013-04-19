*========================================================================
* File Name : histogram.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Mon 07 Mar 2011 02:23:25 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine histogram_init
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/tools/histogram.h"
      do J0=1,hist_dim
        hist_total_num(J0)=0.D0
        hist_missing_num(J0)=0.D0
        do I0=1,hist_bin_max
          hist_y(J0,I0)=0.D0
          hist_y_nor(J0,I0)=0.D0
        enddo
      enddo
      do I0=1,hist_bin_max
        hist_x(I0)=hist_min+dble(I0-1)*hist_interval
      enddo
      hist_max_bin_id=dint((hist_max-hist_min)/hist_interval)
      hist_min_bin_id=1
      return
      end



      subroutine histogram
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/tools/histogram.h"
      do I0=1,hist_dim
        hist_bin_id(I0)=
     &dint((hist_read_value(I0)-hist_min)/hist_interval)+1
C        write(*,*) I0,hist_bin_id(I0),hist_read_value(I0)
C        pause
        if(hist_bin_id(I0).gt.hist_bin_max.or.hist_bin_id(I0).lt.1)then
          hist_missing_num(I0)=hist_missing_num(I0)+1.D0
C          write(*,*) hist_bin_id(I0)
C          pause
          return
        endif
        hist_y(I0,hist_bin_id(I0))=hist_y(I0,hist_bin_id(I0))+1.D0
        hist_total_num(I0)=hist_total_num(I0)+1.D0  !In case they are different for different hist_missing_num
      enddo
      return
      end

      subroutine normalized_histogram
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/tools/histogram.h"
      do J0=1,hist_dim
        do I0=hist_min_bin_id,hist_max_bin_id
          if(hist_y(J0,I0).ne.0.D0)then
            hist_y_nor(J0,I0)=hist_y(J0,I0)/hist_total_num(J0)
          endif
        enddo
      enddo
      return
      end 

      subroutine write_histogram_octave_header(file_name) !will also normalized the histogram
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/header.h"
      include "../include/pes.h"
      include "../include/tools/histogram.h"
      integer hist_map_num,hist_map(hist_bin_max)
      logical preserve_line
C=======check file_x_dim first====
      hist_map_num=0
      do I0=hist_min_bin_id,hist_bin_max
        preserve_line=.false.
        do I1=1,hist_dim
          if(hist_y(I1,I0).ne.0.D0)preserve_line=.true.
        enddo
        if(preserve_line)then
          hist_map_num=hist_map_num+1
          hist_map(hist_map_num)=I0
        endif
      enddo
C========End of checking=============
C========Basic Octave Headers=========================
      call creat_basic_octave_header(file_name,"new")
      open(20,file=file_name,access="append")
      write(20,"(A16)")"# name: hist_min"
      write(20,"(A14)")"# type: scalar"
      write(20,"(F15.6)")hist_min
      write(20,"(A16)")"# name: hist_max"
      write(20,"(A14)")"# type: scalar"
      write(20,"(F15.6)")hist_max
      write(20,"(A21)")"# name: hist_interval"
      write(20,"(A14)")"# type: scalar"
      write(20,"(F15.6)")hist_interval
      write(20,"(A19)")"# name: hist_method"
      write(20,"(A14)")"# type: scalar"
      write(20,"(I2)")hist_method
      write(20,"(A23)")"# name: hist_max_bin_id"
      write(20,"(A14)")"# type: scalar"
      write(20,"(I13)")hist_max_bin_id
      write(20,"(A20)")"# name: hist_bin_max"
      write(20,"(A14)")"# type: scalar"
      write(20,"(I13)")hist_bin_max
      write(20,"(A16)")"# name: hist_dim"
      write(20,"(A14)")"# type: scalar"
      write(20,"(I13)") hist_dim
C========hist_total_num a normalized factor=====================================
      write(20,"(A44)")"# This is the area of the original histogram"
      write(20,"(A21)")"# name: hist_norm_fac"
      write(20,"(A14)")"# type: matrix"
      write(20,"(A9)")"# rows: 1"
      write(20,"(A11,1x,I4)")"# columns: ",hist_dim
      write(20,*)(hist_total_num(I0),I0=1,hist_dim)
C========hist_id table======================================
      write(20,"(A22)")"# hist is a 2-D matrix"
      write(20,"(A23)")"# name: hist_matrix_dim"
      write(20,"(A14)")"# type: scalar"
      write(20,"(I1)")2
      write(20,"(A17)")"# name: hist_x_id"
      write(20,"(A14)")"# type: scalar"
      write(20,"(I1)")1
      write(20,"(A21)")"# name: hist_y_nor_id"
      write(20,"(A14)")"# type: matrix"
      write(20,"(A9)")"# rows: 1"
      write(20,"(A11,1x,I4)")"# columns: ",hist_dim
      write(20,"(3000(I4,1x))")(I0,I0=2,hist_dim+1)
      write(20,"(A17)")"# name: hist_y_id"
      write(20,"(A14)")"# type: matrix"
      write(20,"(A9)")"# rows: 1"
      write(20,"(A11,1x,I4)")"# columns: ",hist_dim
      write(20,"(3000(I4,1x))")(I0,I0=hist_dim+2,2*hist_dim+1)
      close(20)
      return
      end

      subroutine write_histogram(file_name) !will also normalized the histogram
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/header.h"
      include "../include/pes.h"
      include "../include/tools/histogram.h"
      integer hist_map(hist_bin_max)
      logical preserve_line
C=======check file_x_dim first====
      file_x_dim=0
      do I0=hist_min_bin_id,hist_bin_max
        preserve_line=.false.
        do I1=1,hist_dim
          if(hist_y(I1,I0).ne.0.D0)preserve_line=.true.
        enddo
        if(preserve_line)then
          file_x_dim=file_x_dim+1
          hist_map(file_x_dim)=I0
        endif
      enddo
C========End of checking=============
      header_source_type="his"
      header_par_num=11
      header_par_name(1)="pes_id"
      header_par_real(1)=dble(pes_id)
      header_par_name(2)="time_label"
      header_par_real(2)=time_label
      header_par_name(3)="ndim_fac"
      header_par_real(3)=1.D0
      header_par_name(4)="file_x_dim"
      header_par_real(4)=dble(file_x_dim)
      header_par_name(5)="file_y_dim"
      header_par_real(5)=dble(hist_dim+1)
      header_par_name(6)="file_z_dim"
      header_par_real(6)=0.D0
      header_par_name(7)="hist_min"
      header_par_real(7)=hist_min
      header_par_name(8)="hist_max"
      header_par_real(8)=hist_max
      header_par_name(9)="hist_interval"
      header_par_real(9)=hist_interval
      header_par_name(10)="hist_method"
      header_par_real(10)=hist_method
      header_par_name(11)="hist_bin_max_id"
      header_par_real(11)=hist_max_bin_id
      header_annotation=
     &"'hist_x hist_y_nor_1~hist_y_nor_n hist_y1~hist_yn'"
      call creat_formatted_header(file_name)
      open(21,file=file_name,access="append")
C========hist_y======================================
      write(21,"(A12)")"# name: hist"
      write(21,"(A14)")"# type: matrix"
      write(21,"(A8,1x,I6)")"# rows: ",file_x_dim
      write(21,"(A11,1x,I13)")"# columns: ",1+2*hist_dim
      do I0=1,file_x_dim
        write(21,*)
     &hist_x(hist_map(I0)),
     &(hist_y(I1,hist_map(I0))/hist_total_num(I1),I1=1,hist_dim),
     &(hist_y(I2,hist_map(I0)),I2=1,hist_dim)
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
