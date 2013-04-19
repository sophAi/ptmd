*========================================================================
* File Name : pes_assignment.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 西元2010年06月22日 (週二) 11時48分56秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine initial_pes
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/pes.h"
       include "../include/job.h"
       if(job_type.eq."SKIP")return
       if(pes_type.eq."gupta_pure")then
         call read_pes_gupta_pure
       endif
       if(pes_type.eq."gupta_alloy")then
         call read_pes_gupta_alloy
       endif
       if(pes_type.eq."qscff")then
C         call read_pes_qsscff
       endif
       if(pes_type.eq."eam")then
C         call read_pes_eam
       endif
       if(pes_type.eq."sophai")then
C         call read_pes_sophai
       endif
       if(pes_type.eq."sophai_sleep")then
C         call read_pes_sophai_sleep
       endif
       return
       end

       subroutine pes
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/pes.h"
       real*8 pes_gupta_pure_function,pes_gupta_alloy_function,
     &pes_qscff_function,pes_eam_function,pes_sophai_function,
     &pes_sophai_sleep_function,pes_restore_grad_function,
     &pes_restore_no_grad_function
       external pes_gupta_pure_function,pes_gupta_alloy_function,
     &pes_qscff_function,pes_eam_function,pes_sophai_function,
     &pes_sophai_sleep_function,pes_restore_grad_function,
     &pes_restore_no_grad_function
C       if(find_1st_grad.and.numerical_1st_grad)then     !using numerical gradient

C       endif
C       if(find_2nd_grad.and.numerical_2nd_grad)then     !Cal Hessian Matrix

C       endif
       if(pes_type.eq."gupta_pure")then
         pot=pes_gupta_pure_function()
C     &+pes_restore_grad_function
       endif
       if(pes_type.eq."gupta_alloy")then
         pot=pes_gupta_alloy_function()
C     &+pes_restore_grad_function
       endif
       if(pes_type.eq."qscff")then
C         pot=pes_qsscff_function
       endif
       if(pes_type.eq."eam")then
C         pot=pes_eam_function
       endif
       if(pes_type.eq."sophai")then
C         pot=pes_sophai_function
       endif
       if(pes_type.eq."sophai_sleep")then
C         pot=pes_sophai_sleep_funxtion
       endif
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
