*========================================================================
* File Name : 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 
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
      include "../include/usr/usr.h" 
      do I0=1,total_usr_moment
        usr_moment(I0)=0.D0
      enddo
      do I0=1,usr_3_moment_max
        usr_3_moment(I0)=0.D0
      enddo
      do I0=1,usr_4_moment_max
        usr_4_moment(I0)=0.D0
      enddo
      do I0=1,usr_4_moment_ex_max
        usr_4_moment_ex(I0)=0.D0
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
      include "../include/usr/usr.h"
      real*8 dist_ctd_to_cst,dist_ctd_to_fct
      real*8 dist_fct_to_ftf
      real*8 ave,adev,sdev,var,skew,curt
      call centroid
      dist_ctd_to_cst=9999.D0
      dist_ctd_to_fct=0.D0
      dist_fct_to_ftf=0.D0
      do J0=1,atom_num
        J1=J0*3
        dist_ctd(J0)=dsqrt((x(J1-2)-ctd_x)**2
     &+(x(J1-1)-ctd_y)**2+(x(J1)-ctd_z)**2)   !record dist_ct
        if(dist_ctd(J0).gt.dist_ctd_to_fct)then
          dist_ctd_to_fct=dist_ctd(J0)
          fct_id=J0         !determine fct atom
        endif
        if(dist_ctd(J0).lt.dist_ctd_to_cst)then
          dist_ctd_to_cst=dist_ctd(J0)
          cst_id=J0         !determine cst atom
        endif
      enddo
C=========We accept J0=fct_id. Total n=atom_num.========
      J2=fct_id*3
      do J0=1,fct_id-1
        J1=J0*3
        dist_fct(J0)=dsqrt((x(J1-2)-x(J2-2))**2+(x(J1-1)-x(J2-1))**2+
     &(x(J1)-x(J2))**2)      !record dist_fct
        if(dist_fct(J0).gt.dist_fct_to_ftf)then
          dist_fct_to_ftf=dist_fct(J0)
          ftf_id=J0         !determine ftf atom
        endif
      enddo
      dist_fct(fct_id)=0.D0
      do J0=fct_id+1,atom_num
        J1=J0*3
        dist_fct(J0)=dsqrt((x(J1-2)-x(J2-2))**2+(x(J1-1)-x(J2-1))**2+
     &(x(J1)-x(J2))**2)      !record dist_fct
        if(dist_fct(J0).gt.dist_fct_to_ftf)then
          dist_fct_to_ftf=dist_fct(J0)
          ftf_id=J0         !determine ftf atom
        endif
      enddo
C=========We accept J0=cts_id. Total n=atom_num.
      J3=cst_id*3
      do J0=1,cst_id-1
        J1=J0*3
        dist_cst(J0)=dsqrt((x(J1-2)-x(J3-2))**2+
     &(x(J1-1)-x(J3-1))**2+(x(J1)-x(J3))**2)
      enddo
      dist_cst(cst_id)=0.D0
      do J0=cst_id+1,atom_num
        J1=J0*3
        dist_cst(J0)=dsqrt((x(J1-2)-x(J3-2))**2+
     &(x(J1-1)-x(J3-1))**2+(x(J1)-x(J3))**2)
      enddo
C=========We accept J0=ftf_id. Total n=atom_num.
      J4=ftf_id*3
      do J0=1,ftf_id-1    
        J1=J0*3 
        dist_ftf(J0)=dsqrt((x(J1-2)-x(J4-2))**2+
     &(x(J1-1)-x(J4-1))**2+(x(J1)-x(J4))**2) !record dist_ftf
      enddo
      dist_ftf(ftf_id)=0.D0
      do J0=ftf_id+1,atom_num
        J1=J0*3
        dist_ftf(J0)=dsqrt((x(J1-2)-x(J4-2))**2+
     &(x(J1-1)-x(J4-1))**2+(x(J1)-x(J4))**2) !record dist_ftf
      enddo
C=========Calculating moment============
C For fast calculating score,data from 1st to 3rd
C moment follows the order of the original paper 4th and adev are
C arranged to the last(less important).
C Usage: Use 1~12 data for 1st to 3rd moment,1~16 with additional 4th
C moment, and finally 1~20 with adev data

      call moment(dist_ctd,13,ave,adev,sdev,var,skew,curt)
      usr_moment(1)=ave  !for ctd
      usr_moment(2)=var
      usr_moment(3)=skew
      usr_moment(13)=curt
      usr_moment(17)=adev    !actually adev is useless
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
      call moment(dist_cst,13,ave,adev,sdev,var,skew,curt)
      usr_moment(4)=ave   !for cst
      usr_moment(5)=var
      usr_moment(6)=skew
      usr_moment(14)=curt
      usr_moment(18)=adev
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
      call moment(dist_fct,13,ave,adev,sdev,var,skew,curt)
      usr_moment(7)=ave  !for fct
      usr_moment(8)=var
      usr_moment(9)=skew
      usr_moment(15)=curt
      usr_moment(19)=adev
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
      call moment(dist_ftf,13,ave,adev,sdev,var,skew,curt)
      usr_moment(10)=ave    !for ftf
      usr_moment(11)=var
      usr_moment(12)=skew
      usr_moment(16)=curt
      usr_moment(20)=adev
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

      subroutine write_usr(file_name,frame_num,access_method,
     &min_method)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/usr/usr.h"
      integer frame_num
      character access_method*3,min_method*3
      if(min_method.eq."min") call min_pes
      if(access_method.eq."app")then
        open(21,file=file_name,access="append")
      else
        open(21,file=file_name,status="replace")
      endif
      call usr
      write(21,"(I13,1x,F13.8,1x,(3(I3,1x)),(20(F8.5,1x)))")
     &frame_num,pot,
     &cst_id,fct_id,ftf_id,
     &(usr_moment(J2),J2=1,total_usr_moment)  !The data organization is in usr
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
