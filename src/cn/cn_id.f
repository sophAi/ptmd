*========================================================================
* File Name : cn_id.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年10月23日 (週六) 17時01分39秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine cn2name
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/cn/cn.h"
      include "../../include/PTMC/PTMC.h"
      character pair_char(cn_max)*4,pair_char_temp*4,
     &cn_num_name*4,pair_num_char(cn_max)*4,pair_num_char_temp*4
      total_pair_digits=0
      do J0=1,total_neighbors
        if(pair_aboundance(J0).ne.0)then
          total_pair_digits=total_pair_digits+1
          call int2char4(pair_digits_index(J0),pair_char_temp)
          pair_char(total_pair_digits)=pair_char_temp
          call int2char4(pair_aboundance(J0),pair_num_char_temp)
          pair_num_char(total_pair_digits)=pair_num_char_temp
        endif
      enddo
      call int2char4(total_pair_digits,cn_num_name)
      cn_name=cn_num_name
      do J0=1,total_pair_digits
        cn_name=
     &cn_name(:index(cn_name," ")-1)//pair_char(J0)//pair_num_char(J0)
C        write(*,*) cn_name,total_pair_digits
      enddo
      cn_num_name=cn_name
C      write(*,*) cn_name,cn_num_name(3:4),ichar(cn_num_name(4:4))-48
C      pause
      call cn2digits
      return
      end

      subroutine cn2digits
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/cn/cn.h"
      include "../../include/PTMC/PTMC.h"
      character pair_char(cn_max)*4,pair_char_temp*4,
     &cn_num_name*4,pair_num_char(cn_max)*4,pair_num_char_temp*4
      total_pair_digits=(ichar(cn_name(1:1))-48)*1000
     &+(ichar(cn_name(2:2))-48)*100+(ichar(cn_name(3:3))-48)*10
     &+ichar(cn_name(4:4))-48
      do J0=1,total_pair_digits
        I0=4+(J0-1)*8
        dig1=ichar(cn_name(I0+1:I0+1))-48
        dig2=ichar(cn_name(I0+2:I0+2))-48
        dig3=ichar(cn_name(I0+3:I0+3))-48
        dig4=ichar(cn_name(I0+4:I0+4))-48
C        write(*,*)"digs=",dig1,dig2,dig3,dig4
        call digitexmatrix(pair_digits,dig1,dig2,dig3,dig4,
     &matrix_index,2) 
        cn_digits(J0)=pair_digits
        cn_matrix(J0)=matrix_index
        cn_aboundance(J0)=(ichar(cn_name(I0+5:I0+5))-48)*1000+
     &(ichar(cn_name(I0+6:I0+6))-48)*100+
     &(ichar(cn_name(I0+7:I0+7))-48)*10+ichar(cn_name(I0+8:I0+8))-48
C        write(*,*)"result",
C     &J0,cn_digits(J0),cn_aboundance(J0),cn_matrix(J0)
        call digitexmatrix(pair_digits,dig1,dig2,dig3,dig4,
     &matrix_index,1)
C        write(*,*) "TEST=",pair_digits,pair_digits_index(matrix_index),
C     &matrix_index
      enddo
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
