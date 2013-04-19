*========================================================================
* File Name : BIMD_ave.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時45分34秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine BIMD_Cv
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       include "../../include/file.h"
       include "../../include/pes.h"
       include "../../include/BIMD/BIMD.h"

       return
       end

       subroutine BIMD_ave_bond
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       include "../../include/file.h"
       include "../../include/pes.h"
       include "../../include/BIMD/BIMD.h"
       real*8 total_bond,bond(atom_num_max,atom_num_max)
       real*8 total_i_bond(atom_num_max)
       do J1=1,atom_num
         do J3=1,atom_num
           if(J3.ne.J1)then
             ave_dist(J1,J3)=ave_dist (J1,J3)/
     &(final_loop-init_loop+1.D0)
             ave_dist2(J1,J3)=ave_dist2(J1,J3)/
     &(final_loop-init_loop+1.D0)
           endif
         enddo
       enddo
       total_bond=0.D0
       do J1=1,atom_num
         total_i_bond(J1)=0.D0
         do J3=1,atom_num
           if(J3.ne.J1) then
             bond(J1,J3)=dsqrt(ave_dist2(J1,J3)-ave_dist(J1,J3)**2)/
     &ave_dist(J1,J3)
             total_i_bond(J1)=total_i_bond(J1)+bond(J1,J3)
           endif
         enddo
         total_bond=total_bond+total_i_bond(J1)
       enddo
       ave_bond=total_bond*(1.D0/dble(atom_num*(atom_num-1)))
       return
       end

       subroutine BIMD_temper
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       include "../../include/file.h"
       include "../../include/pes.h"
       include "../../include/BIMD/BIMD.h"

       return
       end


       subroutine BIMD_equ_Cv
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       include "../../include/file.h"
       include "../../include/pes.h"
       include "../../include/BIMD/BIMD.h"

       return
       end

       subroutine BIMD_equ_ave_bond
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       include "../../include/file.h"
       include "../../include/pes.h"
       include "../../include/BIMD/BIMD.h"

       return
       end

       subroutine BIMD_equ_temper
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       include "../../include/file.h"
       include "../../include/pes.h"
       include "../../include/BIMD/BIMD.h"

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
