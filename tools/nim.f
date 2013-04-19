*========================================================================
* File Name : nim.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年10月22日 (週五) 15時52分13秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine nim_init
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/tools/nim.h"
       nim_eff_num=0
       do I0=1,nim_dim_max-1
         nim_var_num(I0)=0
         do I1=I0+1,nim_dim_max         !left-bottom triangle
           nim_flag(I0,I1)=0  !I1>I0
         enddo
       enddo
       return
       end
 
       subroutine nim_eff
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/tools/nim.h"
       integer nim_eff_temp(nim_dim_max),nim_eff_temp_num
       logical nim_eff_true
!need restore to 0,nim_flag(),nim_eff_num,nim_var_num()
       call nim_init
       do I0=1,nim_dim-1     !row  ,note I0 is pure row number, but I1,I2 in next next loops are not. Use nim_eff_temp(I1) or nim_eff_temp(I2)
         nim_eff_temp_num=0
         do I1=I0+1,nim_dim    !collect the eff pool for a certain column,left-bottom triangle
           if (nim_matrix(I0,I1).ge.nim_threshold)then !for I1>I0
             nim_eff_temp_num=nim_eff_temp_num+1
             nim_eff_temp(nim_eff_temp_num)=I1
           endif
         enddo
!============================
         do I1=1,nim_eff_temp_num  !scan for eff var in this column,the ref.
           if (nim_flag(I0,nim_eff_temp(I1)).eq.0)then !choose the non-overlapped element
             nim_eff_num=nim_eff_num+1
             nim_eff_var(nim_eff_num,1)=I0 !record the first two elements, need to sort them later.
             nim_eff_var(nim_eff_num,2)=nim_eff_temp(I1)  !Note, I1 here is not the column number, nim_eff_temp(I1) is.
             nim_var_num(nim_eff_num)=2
             nim_flag(I0,nim_eff_temp(I1))=1
C============================
             do I2=1,I1-1  !nim_eff_temp(I1)>nim_eff_temp(I2) for nim_flag(a,b) we only use b>a
               if (nim_matrix(nim_eff_temp(I2),nim_eff_temp(I1)).ge.
     &nim_threshold)then   !eff var candidate
                 nim_eff_true=.true.
                 do I3=2,nim_var_num(nim_eff_num) !nim_eff_var(nim_eff_nim,I3)
                   if (
     &nim_matrix(nim_eff_temp(I2),nim_eff_var(nim_eff_num,I3)).lt.
     &nim_threshold)then
                     nim_eff_true=.false.
                   endif
                 enddo
                 if(nim_eff_true)then
                   nim_var_num(nim_eff_num)=nim_var_num(nim_eff_num)+1
                   nim_eff_var(nim_eff_num,nim_var_num(nim_eff_num))=
     &nim_eff_temp(I2)
                   nim_flag(I0,nim_eff_temp(I2))=1  !switch off the flag so the next
                   do I3=2,nim_var_num(nim_eff_num)-1
                     nim_flag(
     &nim_eff_temp(I2),nim_eff_var(nim_eff_num,I3))=1
                   enddo
                 endif
               endif
             enddo
             do I2=I1+1,nim_eff_temp_num   !nim_eff_temp(I2)>nim_eff_temp(I1) for nim_flag(a,b) we only use b>a
               if (nim_matrix(nim_eff_temp(I1),nim_eff_temp(I2)).ge.
     &nim_threshold)then  !eff var candidate. We use nim_matrix(I2,I1) for left-bottom triangle
                 nim_eff_true=.true.
                 do I3=2,nim_var_num(nim_eff_num) !nim_eff_temp(I2)>nim_eff_var(nim_eff_num,I3)
                   if (
     &nim_matrix(nim_eff_var(nim_eff_num,I3),nim_eff_temp(I2)).lt.
     &nim_threshold)then  !check if one of them is not eff
                     nim_eff_true=.false.
                   endif
                 enddo
                 if (nim_eff_true)then
                   nim_var_num(nim_eff_num)=nim_var_num(nim_eff_num)+1
                   nim_eff_var(nim_eff_num,nim_var_num(nim_eff_num))=
     &nim_eff_temp(I2)
                   nim_flag(I0,nim_eff_temp(I2))=1
                   do I3=2,nim_var_num(nim_eff_num)-1
                     nim_flag(
     &nim_eff_var(nim_eff_num,I3),nim_eff_temp(I2))=1
                   enddo
                 endif
               endif
             enddo
           endif
         enddo
       enddo
!output nim_eff_num,nim_var_num(),nim_eff_var(,)
!sorting cost too much, use hashing instead
       call nim_hash
       return
       end

       subroutine nim_hash
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/tools/nim.h"
       integer nim_temp
       do I0=1,nim_eff_num
         nim_hash_sum(I0)=0
         nim_hash_tra(I0)=1
         do I1=1,nim_var_num(I0)
           nim_hash_sum(I0)=nim_hash_sum(I0)+nim_eff_var(I0,I1)  
           nim_hash_tra(I0)=nim_hash_tra(I0)*nim_eff_var(I0,I1)
         enddo
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
