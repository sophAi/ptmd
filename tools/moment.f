*========================================================================
* File Name : moment.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Sun 10 Jun 2012 05:08:25 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================

      subroutine hist2moment
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/header.h"
      include "../include/tools/histogram.h"
      include "../include/tools/moment.h"
      real*8 var_1,var_2,var_3,var_4,var_temp
      do I0=1,moment_y_max
        ave_n(I0)=0.D0
        var_n(I0)=0.D0
        adev_n(I0)=0.D0
        sdev_n(I0)=0.D0
        skew_n(I0)=0.D0
        curt_n(I0)=0.D0
        do I1=hist_min_bin_id,hist_bin_max
          ave_n(I0)=ave_n(I0)+hist_y(I0,I1)*hist_x(I1)
        enddo
        var_1=0.D0
        var_2=0.D0
        var_3=0.D0
        var_4=0.D0
        ave_n(I0)=ave_n(I0)/hist_total_num(I0)
        do I1=hist_min_bin_id,hist_bin_max
          var_temp=(hist_x(I1)-ave_n(I0))
          var_1=var_1+var_temp*hist_y(I0,I1)
          var_2=var_2+(var_temp**2)*hist_y(I0,I1)
          var_3=var_3+(var_temp**3)*hist_y(I0,I1)
          var_4=var_4+(var_temp**4)*hist_y(I0,I1)
          adev_n(I0)=adev_n(I0)+dabs(var_temp)*hist_y(I0,I1)
        enddo
        adev_n(I0)=adev_n(I0)/hist_total_num(I0)
        var_n(I0)=(var_2-var_1**2/hist_total_num(I0))/
     &(hist_total_num(I0)-1)
        sdev_n(I0)=dsqrt(var_n(I0))
        if(var_n(I0).ne.0.D0)then
          skew_n(I0)=var_3/(hist_total_num(I0)*sdev_n(I0)**3)
          curt_n(I0)=var_4/(hist_total_num(I0)*var_n(I0)**2)-3.D0
        else
          skew_n(I0)=0.D0
          curt_n(I0)=0.D0
        endif
      enddo
      return
      end 


      subroutine moment(data_in,n)
C===============================
C     data_in:input data of dimension n
C     n:dimension of the data
C     ave: 1st moment or mean value of the data
C     adev: average deviation sum(|x-ave|)/N
C     sdev: standard deviation sqrt(sum(x-ave)^2)=sqrt(var)
C     var: 2nd moment or variation. The width of the distribution
C     skew: 3rd moment or skewness. The asymmetirc tail toward +x or -x.
C     Should be >sqrt(15/N) for safty.
C     curt: 4th moment or Kurtosis. Relative peakedness and flatness to
C     normal distribution. Positive case is leptokurtic and negative is
C     platykurtic. The in-between is called mesokurtic.
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/tools/moment.h"
      integer n
      real*8 data_in(n),n_real
      real*8 p,s,ep
C      if(n.le.1)pause 'n must be at least 2 in moment'
      s=0.D0
      n_real=dble(n)
      do 11 I0=1,n
        s=s+data_in(I0)
11    continue
      ave=s/n_real
      adev=0.D0
      var=0.D0
      skew=0.D0
      curt=0.D0
      ep=0.D0
      do 12 I0=1,n
        s=data_in(I0)-ave
        ep=ep+s
        adev=adev+dabs(s)
        p=s*s
        var=var+p
        p=p*s
        skew=skew+p
        p=p*s
        curt=curt+p
12    continue
      adev=adev/n_real
      var=(var-ep**2/n_real)/(n_real-1.D0)
      sdev=dsqrt(var)
      if(var.ne.0.D0)then
        skew=skew/(n_real*sdev**3)
        curt=curt/(n_real*var**2)-3.D0
      else
C        pause 'no skew or kurtosis when zero variance in moment'
      skew=0.D0
      curt=0.D0
      endif
      return
      end

      subroutine moment_rapallo(data_in,n)
C===============================
C     data_in:input data of dimension n
C     n:dimension of the data
C     ave: 1st moment or mean value of the data
C     adev: average deviation sum(|x-ave|)/N
C     sdev: standard deviation sqrt(sum(x-ave)^2)=sqrt(var)
C     var: 2nd moment or variation. The width of the distribution
C     skew: 3rd moment or skewness. The asymmetirc tail toward +x or -x.
C     Should be >sqrt(15/N) for safty.
C     curt: 4th moment or Kurtosis. Relative peakedness and flatness to
C     normal distribution. Positive case is leptokurtic and negative is
C     platykurtic. The in-between is called mesokurtic.
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/tools/moment.h"
      integer n
      real*8 data_in(n),n_real
      real*8 p,s,ep
C      if(n.le.1)pause 'n must be at least 2 in moment'
      s=0.D0
      n_real=dble(n)
      do 11 I0=1,n
        s=s+data_in(I0)
11    continue
      ave=s/n_real
      adev=0.D0
      var=0.D0
      skew=0.D0
      curt=0.D0
      ep=0.D0
      do 12 I0=1,n
        s=data_in(I0)-ave
        ep=ep+s
        adev=adev+dabs(s)
        p=s*s
        var=var+p
        p=p*s
        skew=skew+p
        p=p*s
        curt=curt+p
12    continue
      adev=adev/n_real
      var=(var-ep**2/n_real)/(n_real-1.D0)
      sdev=dsqrt(var)
      if(var.ne.0.D0)then
        skew=skew/(n_real*sdev**3)
        curt=curt/(n_real*var**2)-3.D0
      else
C        pause 'no skew or kurtosis when zero variance in moment'
      skew=0.D0
      curt=0.D0
      endif
      var=sqrt(var)
      skew=skew**(1/3)
      write(*,*) "curt=",curt
      curt=curt**(1/4)
      write(*,*) "curt**1/4=",curt
      pause
      return
      end


      subroutine write_moment_octave_header(file_name)
      implicit none  
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/header.h"
      include "../include/pes.h"
      include "../include/tools/moment.h"
      call creat_basic_octave_header(file_name,"new")
      open(20,file=file_name,access="append")
      write(20,"(A41)")"# The frame num of the moment descriptors"
      write(20,"(A23)")"# name: total_frame_num"
      write(20,"(A14)")"# type: scalar"
      write(20,*) file_x_dim
      write(20,"(A52)")
     &"# The number of moment descriptor members in a frame"
      write(20,"(A22)")"# name: mom_member_num"
      write(20,"(A14)")"# type: scalar"
      write(20,*) total_moment_y_num
      write(20,"(A56)")
     &"# The total number of moment descriptors for each member"
      write(20,"(A22)")"# name: moment_des_num"
      write(20,"(A14)")"# type: scalar"
      write(20,*)total_moment_z_num
      write(20,"(A27)")
     &"# The length of each moment"
      write(20,"(A29)")"# name: moment_dim_per_moment"
      write(20,"(A14)")"# type: scalar"
      write(20,*)total_moment_x_num
C========index map of moment=====================
      write(20,"(A24)")"# moment is a 3-D matrix"
      write(20,"(A25)")"# name: moment_matrix_dim"
      write(20,"(A14)")"# type: scalar"
      write(20,"(I1)")3
      write(20,"(A23)")"# name: moment_frame_id"
      write(20,"(A14)")"# type: scalar"
      write(20,"(I1)")1
      write(20,"(A24)")"# name: moment_member_id"
      write(20,"(A14)")"# type: scalar"
      write(20,"(I1)")2
      write(20,"(A21)")"# name: moment_ave_id"
      write(20,"(A14)")"# type: scalar"
      write(20,"(I1)")3
      write(20,"(A21)")"# name: moment_var_id"
      write(20,"(A14)")"# type: scalar"
      write(20,"(I1)")4
      write(20,"(A22)")"# name: moment_skew_id"
      write(20,"(A14)")"# type: scalar"
      write(20,"(I1)")5
      write(20,"(A22)")"# name: moment_curt_id"
      write(20,"(A14)")"# type: scalar"
      write(20,"(I1)")6
      write(20,"(A22)")"# name: moment_sdev_id"
      write(20,"(A14)")"# type: scalar"
      write(20,"(I1)")7
      write(20,"(A22)")"# name: moment_adev_id"
      write(20,"(A14)")"# type: scalar"
      write(20,"(I1)")8
      write(20,"(A17)")"# name: moment_id"
      write(20,"(A14)")"# type: matrix"
      write(20,"(A9)")"# rows: 1"
      write(20,"(A12)")"# columns: 4"
      write(20,*)(I0,I0=3,6)
      close(20)
      return
      end
  

      subroutine write_moment_header
     &(file_name,frame_num,total_frame_num)
      implicit none      !For frame_num>1, use access_method="app"
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/header.h"
      include "../include/pes.h"
      include "../include/tools/moment.h"
      integer frame_num,total_frame_num
      header_source_type="mom"
      header_par_num=8
      header_par_name(1)="pes_id"
      header_par_real(1)=dble(pes_id)
      header_par_name(2)="time_label"
      header_par_real(2)=time_label
      header_par_name(3)="ndim_fac"
      header_par_real(3)=1.D0
      header_par_name(4)="file_x_dim"
      header_par_real(4)=dble(total_frame_num)
      header_par_name(5)="file_y_dim"
      header_par_real(5)=dble(total_moment_y_num*total_moment_z_num)+1
      header_par_name(6)="total_moment_y_num"
      header_par_real(6)=dble(total_moment_y_num)
      header_par_name(7)="total_moment_z_num"
      header_par_real(7)=dble(total_moment_z_num)
      header_par_name(8)="total_moment_x_num"
      header_par_real(8)=dble(total_moment_x_num)
      header_annotation=
     &"'IDx IDy ave(1st) var(2nd) skew(3rd) curt(4th) sdev adev'"
      call creat_formatted_header(file_name)
      open(21,file=file_name,access="append")
      write(21,"(A14)")"# name: moment"
      write(21,"(A14)")"# type: matrix"
      write(21,"(A8,1x,I13)")"# rows: ",
     &total_frame_num*total_moment_y_num
      write(21,"(A11,1x,I5)")"# columns: ",
     &total_moment_z_num+4
      close(21)
      return
      end

      subroutine write_moment(file_name,frame_num)
      implicit none      !For frame_num>1, use access_method="app"
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/header.h"
      include "../include/pes.h"
      include "../include/tools/moment.h"
      integer frame_num
      open(21,file=file_name,access="append")
      do I0=1,total_moment_y_num
        write(21,"(I1,1x,I13,6(1x,F15.8))")
     &frame_num,I0,ave_n(I0),var_n(I0),skew_n(I0),
     &curt_n(I0),sdev_n(I0),adev_n(I0)
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
