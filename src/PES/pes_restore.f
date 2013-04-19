*========================================================================
* File Name : pes_restore.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時55分39秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
***************************************************
*     n-D Boundary Restoring force potential      *
*     parameter: restore_fac,outer_bound_radius   *
***************************************************
      function pes_restore_grad_function()
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      real*8 dist_square,pes_restore_grad_function
      pes_restore_grad_function=0.D0
      do J1=1,atom_num
        J3=ndim_fac*J1
        dist_square=0.D0
        do J2=1,ndim_fac
          dist_square=dist_square+x(J3-J2+1)**2
          grad(J3-J2+1)=grad(J3-J2+1)+restore_fac*16.D0*
     &(dist_square-outer_bound_radius)*x(J3-J2+1)
        enddo
        if (dist_square.gt.outer_bound_radius) then
          evap=.true.
          restore_per_atom(J1)=restore_fac*
     &(dist_square-outer_bound_radius)**2 
          pes_restore_grad_function=pes_restore_grad_function+
     &restore_per_atom(J1)
        endif
      enddo
      return
      end

      function pes_restore_no_grad_function()
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      real*8 dist_square,pes_restore_no_grad_function
      pes_restore_no_grad_function=0.D0
      do J1=1,atom_num
        J3=ndim_fac*J1
        dist_square=0.D0
        do J2=1,ndim_fac
          dist_square=dist_square+x(J3-J2+1)**2
        enddo
        if (dist_square.gt.outer_bound_radius) then
          evap=.true.
          restore_per_atom(J1)=restore_fac*
     &(dist_square-outer_bound_radius)**2
          pes_restore_no_grad_function=pes_restore_no_grad_function+
     &restore_per_atom(J1)
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
