*========================================================================
* File Name : cn_output.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時51分08秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine cn_output
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/cn/cn.h"
      do J1=1,total_neighbors
        if(pair_aboundance(J1).ne.0)then
          pair_aboundance_total(J1)=pair_aboundance_total(J1)+1
          pair_analysis(J0,J1)=pair_aboundance(J1)
        endif
      enddo
      do J1=1,total_neighbors
        if(pair_aboundance_total(J1).ne.0)then
          pair_digits=pair_digits_index(J1)
          write(*,31)J0,pair_analysis(J0,J1),
     &pair_digits_index(J1),J1,pair_num 
           do J1=1,total_neighbors
             if(pair_analysis(J0,J1).ne.0)then
               write(file_index2,24)
     &I2,pair_analysis(J0,J1),ETAB_MIN_GL(J0),pair_digits_index(J1),
     &J0,J1,pair_num,file_name1
             endif
           enddo
         endif
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
