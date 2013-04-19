*========================================================================
* File Name : usr.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Tue 06 Nov 2012 09:11:12 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine usr_init
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      include "../include/tools/usr.h" 
      include "../include/tools/moment.h"
      total_moment_z_num=16
      do I0=1,total_moment_z_num
        mom(I0)=0.D0
      enddo
      return
      end

      subroutine usr
      implicit none
C===========================================
C     ctd:the molecular centroid
C     cst:the closest atom to ctd
C     fct:the farthest atom to ctd
C     ftf:the farthest atom to fct
C===========================================
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      include "../include/tools/moment.h"
      include "../include/tools/usr.h"
      integer cst_top_id,fct_top_id,ftf_top_id
      real*8 dist_ctd_to_cst,dist_ctd_to_fct
      real*8 dist_fct_to_ftf
      call centroid_usr  !rewrite
      dist_ctd_to_cst=9999.D0
      dist_ctd_to_fct=0.D0
      dist_fct_to_ftf=0.D0
      do J0=1,top_usr_num
        J1=top_usr_id(J0)*3
        dist_ctd(J0)=dsqrt((x(J1-2)-com_x)**2
     &+(x(J1-1)-com_y)**2+(x(J1)-com_z)**2)   !record dist_ct
        if(dist_ctd(J0).gt.dist_ctd_to_fct)then
          dist_ctd_to_fct=dist_ctd(J0)
          fct_id=top_usr_id(J0)         !determine fct atom, corresponding to real atomic id
          fct_top_id=J0                 !corresponding to top id among top_usr_num
        endif
        if(dist_ctd(J0).lt.dist_ctd_to_cst)then
          dist_ctd_to_cst=dist_ctd(J0)
          cst_id=top_usr_id(J0)         !determine cst atom
          cst_top_id=J0
        endif
      enddo
C=========We accept J0=fct_id. Total n=top_usr_num========
      J2=fct_id*3      !use_read atomic id
      do J0=1,fct_top_id-1
        J1=top_usr_id(J0)*3
        dist_fct(J0)=dsqrt((x(J1-2)-x(J2-2))**2+(x(J1-1)-x(J2-1))**2+
     &(x(J1)-x(J2))**2)      !record dist_fct
        if(dist_fct(J0).gt.dist_fct_to_ftf)then
          dist_fct_to_ftf=dist_fct(J0)
          ftf_id=top_usr_id(J0)         !determine ftf atom
          ftf_top_id=J0
        endif
      enddo
      dist_fct(fct_top_id)=0.D0
      do J0=fct_top_id+1,top_usr_num
        J1=top_usr_id(J0)*3
        dist_fct(J0)=dsqrt((x(J1-2)-x(J2-2))**2+(x(J1-1)-x(J2-1))**2+
     &(x(J1)-x(J2))**2)      !record dist_fct to index(J0) total number top_usr_num
        if(dist_fct(J0).gt.dist_fct_to_ftf)then
          dist_fct_to_ftf=dist_fct(J0)
          ftf_id=top_usr_id(J0)         !determine ftf atom
          ftf_top_id=J0
        endif
      enddo
C=========We accept J0=cts_id. Total n=top_usr_num===============
      J3=cst_id*3
      do J0=1,cst_top_id-1
        J1=top_usr_id(J0)*3
        dist_cst(J0)=dsqrt((x(J1-2)-x(J3-2))**2+
     &(x(J1-1)-x(J3-1))**2+(x(J1)-x(J3))**2)
      enddo
      dist_cst(cst_top_id)=0.D0
      do J0=cst_top_id+1,top_usr_num
        J1=top_usr_id(J0)*3
        dist_cst(J0)=dsqrt((x(J1-2)-x(J3-2))**2+
     &(x(J1-1)-x(J3-1))**2+(x(J1)-x(J3))**2)
      enddo
C=========We accept J0=ftf_id. Total n=top_usr_num=================
      J4=ftf_id*3
      do J0=1,ftf_top_id-1    
        J1=top_usr_id(J0)*3 
        dist_ftf(J0)=dsqrt((x(J1-2)-x(J4-2))**2+
     &(x(J1-1)-x(J4-1))**2+(x(J1)-x(J4))**2) !record dist_ftf
      enddo
      dist_ftf(ftf_top_id)=0.D0
      do J0=ftf_top_id+1,top_usr_num
        J1=top_usr_id(J0)*3
        dist_ftf(J0)=dsqrt((x(J1-2)-x(J4-2))**2+
     &(x(J1-1)-x(J4-1))**2+(x(J1)-x(J4))**2) !record dist_ftf
      enddo
C=========Calculating moment============
C For fast calculating score,data from 1st to 3rd
C moment follows the order of the original paper 4th and adev are
C arranged to the last(less important).
C Usage: Use 1~12 data for 1st to 3rd moment,1~16 with additional 4th
C moment, and finally 1~20 with adev data

      call moment(dist_ctd,top_usr_num)
      mom(1)=ave  !for ctd
C      mom(5)=var
      mom(5)=sdev
      mom(9)=skew
      mom(13)=curt
C      usr_moment(17)=adev
C      usr_3_moment(1)=ave
C      usr_3_moment(2)=var
C      usr_3_moment(3)=skew
C      usr_4_moment(1)=ave
C      usr_4_moment(2)=var
C      usr_4_moment(3)=skew
C      usr_4_moment(4)=curt   !4'th additional moment
C      usr_4_moment_ex(1)=ave
C      usr_4_moment_ex(2)=var
C      usr_4_moment_ex(3)=adev !Additional average deviation
C      usr_4_moment_ex(4)=skew
C      usr_4_moment_ex(5)=curt
      call moment(dist_cst,top_usr_num)
      mom(2)=ave   !for cst
C      mom(6)=var
      mom(6)=sdev
      mom(10)=skew
      mom(14)=curt
C      usr_moment(18)=adev
C      usr_3_moment(4)=ave
C      usr_3_moment(5)=var
C      usr_3_moment(6)=skew
C      usr_4_moment(5)=ave
C      usr_4_moment(6)=var
C      usr_4_moment(7)=skew
C      usr_4_moment(8)=curt   !4'th additional moment
C      usr_4_moment_ex(6)=ave
C      usr_4_moment_ex(7)=var
C      usr_4_moment_ex(8)=adev !Additional average deviation
C      usr_4_moment_ex(9)=skew
C      usr_4_moment_ex(10)=curt
      call moment(dist_fct,top_usr_num)
      mom(3)=ave  !for fct
C      mom(7)=var
      mom(7)=sdev
      mom(11)=skew
      mom(15)=curt
C      usr_moment(19)=adev
C      usr_3_moment(7)=ave
C      usr_3_moment(8)=var
C      usr_3_moment(9)=skew
C      usr_4_moment(9)=ave
C      usr_4_moment(10)=var
C      usr_4_moment(11)=skew
C      usr_4_moment(12)=curt   !4'th additional moment
C      usr_4_moment_ex(11)=ave
C      usr_4_moment_ex(12)=var
C      usr_4_moment_ex(13)=adev !Additional average deviation
C      usr_4_moment_ex(14)=skew
C      usr_4_moment_ex(15)=curt
      call moment(dist_ftf,top_usr_num)
      mom(4)=ave    !for ftf
C      mom(8)=var
      mom(8)=sdev
      mom(12)=skew
      mom(16)=curt
C      usr_moment(20)=adev
C      usr_3_moment(10)=ave
C      usr_3_moment(11)=var
C      usr_3_moment(12)=skew
C      usr_4_moment(13)=ave
C      usr_4_moment(14)=var
C      usr_4_moment(15)=skew
C      usr_4_moment(16)=curt   !4'th additional moment
C      usr_4_moment_ex(16)=ave
C      usr_4_moment_ex(17)=var
C      usr_4_moment_ex(18)=adev !Additional average deviation
C      usr_4_moment_ex(19)=skew
C      usr_4_moment_ex(20)=curt
C=====================================
      return
      end

      subroutine write_usr_octave_header(file_name)
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
      write(20,"(A21)")"# name: mom_group_num"
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
      write(20,"(A17)")"# name: moment_id"
      write(20,"(A14)")"# type: matrix"
      write(20,"(A9)")"# rows: 1"
      write(20,"(A11,1x,I5)")"# columns: ",total_moment_z_num
      write(20,*)(I0,I0=3,3+total_moment_z_num)
      write(20,"(A14)")"# name: pot_id"
      write(20,"(A14)")"# type: scalar"
      write(20,*)4+total_moment_z_num
      write(20,"(A14)")"# name: cst_id"
      write(20,"(A14)")"# type: scalar"
      write(20,*)5+total_moment_z_num
      write(20,"(A14)")"# name: fct_id"
      write(20,"(A14)")"# type: scalar"
      write(20,*)6+total_moment_z_num
      write(20,"(A14)")"# name: ftf_id"
      write(20,"(A14)")"# type: scalar"
      write(20,*)7+total_moment_z_num
      close(20)
      return
      end



      subroutine write_usr(file_name,frame_num_x,frame_num_y,
     &access_method)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/tools/moment.h"
      include "../include/tools/usr.h"
      integer frame_num_x,frame_num_y
      character access_method*3
      if(access_method.eq."app")then
        open(21,file=file_name,access="append")
      else
        open(21,file=file_name,status="replace")
      endif
      write(21,"(I13,1x,I3,1x,(16(F11.5,1x)),F14.8,(3(1x,I3)))")
     &frame_num_x,frame_num_y,
     &(mom(J2),J2=1,total_moment_z_num),  !The data organization is in usr:ctd_mom1,cst_mom1,fct_mom1,ftf_mom1,ctd_mom2,cst_mom2,fct_mom2,ftf_mom2,ctd_mom3,cst_mom3,fct_mom3,ftf_mom3,ctd_mom4,cst_mom4,fct_mom4,ftf_mom4
     &pot,cst_id,fct_id,ftf_id
      close(21)
      return
      end

      subroutine write_usr_unformatted(file_name,frame_num_x,
     &frame_num_y,access_method)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/tools/moment.h"
      include "../include/tools/usr.h"
      integer frame_num_x,frame_num_y
      character access_method*3
      if(access_method.eq."app")then
        open(21,file=file_name,form="unformatted",access="append")
      else
        open(21,file=file_name,form="unformatted",status="replace")
      endif
      write(21)
     &frame_num_x,frame_num_y,(mom(J2),J2=1,total_moment_z_num),  !The data organization is in usr
     &pot,cst_id,fct_id,ftf_id
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
