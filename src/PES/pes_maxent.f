*========================================================================
* File Name : pes_maxent.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時55分18秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine read_pes_maxent
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/PES/maxent.h"
      maxent_interval_unit=maxent_upper_unit-maxent_lower_unit
      return
      end

*********************
*  Maximum Entropy  *
*********************
      function pes_maxent_func
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/PES/maxent.h"
      real*8 s_temp,x_temp,dx_temp
      do J0=1,ndim
        lamda(J0)=x(J0)
      enddo
      s=0.5D0*(maxent_interval_unit)*
     &(maxent_func(maxent_upper_unit)+maxent_func(maxent_lower_unit))
      do J0=2,iter_max
        I0=2**(J0-2)
        s_temp=0.D0
        dx_temp=maxent_interval_unit/dble(I0)
        x_temp=maxent_lower_unit+0.5D0*dx_temp
        do J1=1,I0
          s_temp=s_temp+maxent_func(x_temp)
          x_temp=x_temp+dx_temp
        enddo 
        s=0.5D0*(s+maxent_interval_unit*s_temp/dble(I0)
        if(
      enddo
      return
      end

      subroutine maxent_integral(func)
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/PES/maxent.h"
      real*8 func
      external func
      if
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
