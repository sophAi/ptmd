*========================================================================
* File Name : p_matrix.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年10月23日 (週六) 16時15分09秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine p_matrix_cn   !1D p_matrix case
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/cn/cn.h"
      include "../include/tester/tester.h"
      include "../include/tester/sliding.h"
      real*8 real_dummy1
      integer cn_org_exist_id(p_matrix_max),time_step
      integer cn_new_table(p_matrix_max)
      integer cn_new_table_id2cn_matrix_id(p_matrix_max)
      integer cn_new_table_cn2new_talbe_id(3000)
      integer p_matrix(p_matrix_max)
      open(21,file=source_file_name,status="old")
      do I0=1,p_matrix_max
        cn_org_exist_id(I0)=0
      enddo
      total_p_num=0
30    read(21,*,end=31)time_step,real_dummy1,cn_name
      total_p_num=total_p_num+1
      call cn2digits
      do I1=1,total_pair_digits
        cn_org_exist_id(cn_matrix(I1))=1   !construction the table first
      enddo
      goto 30
31    rewind(21)
      open(22,file=source_file_name(:index(sliding_file_name," ")-1)
     &//".tab",status="replace")
      write(22,*) 
     &"# renew_existed_id renew_4digits cn_matrix_id ;total_time_step=",
     &total_p_num
      p_matrix_length=0
      do I1=1,p_matrix_max
        if(cn_org_exist_id(I1).eq.1)then
          p_matrix_length=p_matrix_length+1
          call digitexmatrix(pair_digits,dig1,dig2,dig3,dig4,I1,1)
          cn_new_table(p_matrix_length)=pair_digits     !conversion of new_table_id to 4 digits
          cn_new_table_id2cn_matrix_id(p_matrix_length)=I1    !conversion of new_table_id to cn_matrix_id
          cn_new_table_cn2new_talbe_id(pair_digits)=
     &p_matrix_length               !reverse conversion of 4digits to new_table_id 
          write(22,*) p_matrix_length,pair_digits,I1
        endif
      enddo
      close(22)
C==========End of construction P table=======================
C==========Start of construction p time series===============
      open(23,file=sliding_file_name(:index(sliding_file_name," ")-1)
     &//".pts",status="replace")   !p_matrix time series
      do I0=1,total_p_num
        do I1=1,p_matrix_length
          p_matrix(I1)=0
        enddo
        read(21,*)time_step,real_dummy1,cn_name
        call cn2digits
        do I1=1,total_pair_digits
          J1=cn_new_table_cn2new_talbe_id(cn_digits(I1))   !reverse 4digits to cn_new_table_id
          p_matrix(J1)=cn_aboundance(I1)
        enddo
        write(23,"(999(I4,1x))")(p_matrix(I1),I1=1,p_matrix_length)
      enddo
      close(23)
      close(21)
      return
      end

      subroutine p_matrix_anl  !2D to 1D p_matrix case
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/cn/cn.h"
      include "../include/tester/tester.h"
      include "../include/tester/sliding.h"
      integer p_matrix(atom_num_max),read_neighbor(atom_num_max)
      integer p_table_x_atom(atom_num_max),
     &p_table_y_atom(atom_num_max),p_table_xy2id(100,100)
      integer int_dummy,neighbor_num
      character char_dummy*10
      call count_file_line(source_file_name)
      total_p_num=file_line_num/atom_num
      open(23,file=source_file_name(:index(source_file_name," ")-1)
     &//".tab",status="replace")
          write(23,*) 
     &"# renew_p_table_id renew_pair_x renew_pair_y ;total_time_step=",
     &total_p_num
      p_matrix_length=0
      do I1=1,atom_num-1
        do I2=I1+1,atom_num
          p_matrix_length=p_matrix_length+1
          p_table_x_atom(p_matrix_length)=I1
          p_table_y_atom(p_matrix_length)=I2        !convert from p_table_id to a pair
          p_table_xy2id(I1,I2)=p_matrix_length      !convert from x-y pairs to p_table_id
          write(23,"(I10,1x,I5,1x,I5)")p_matrix_length,I1,I2
        enddo
      enddo
      close(23)
      open(21,file=source_file_name,status="old")
      open(22,file=source_file_name(:index(source_file_name," ")-1)
     &//".pts",status="replace")
      do I0=1,total_p_num
        do I1=1,p_matrix_length
          p_matrix(I1)=0
        enddo
        do I1=1,atom_num
          read(21,*) int_dummy,neighbor_num,char_dummy,
     &(read_neighbor(I2),I2=1,neighbor_num)
          do I3=1,neighbor_num
            J3=p_table_xy2id(I1,read_neighbor(I3))   !the p_table_id for I1 and read_neighbor(I3) pairs
            p_matrix(J3)=1
          enddo
        enddo
        write(22,"(100(I1,1x))")(p_matrix(I1),I1=1,p_matrix_length)
      enddo
      close(22)
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
