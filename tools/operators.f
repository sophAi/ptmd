*========================================================================
* File Name : operators.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時06分04秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine operator_MC_move
      implicit none 
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      do I0=1,ndim
C        x(I0)=x_prev(I0)+moving_length*rnd()*((-1.D0)**(dint(rnd())))
         x(I0)=x_prev(I0)+moving_length*rnd()
      enddo
      return
      end

      subroutine operator_BH_move
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      do I0=1,ndim
!        x(I0)=x_prev(I0)+moving_length*rnd()*((-1.D0)**(dint(rnd())))
        x(I0)=x_prev(I0)+moving_length*rnd()*((-1.D0)**(rnd()))
      enddo
      return
      end

      subroutine operator_permutation_lmin
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      real*8 x_lmin(ndim_max),pot_lmin
      logical found_lmin
      pot_lmin=pot_prev
      do I0=1,ndim
        x_lmin(I0)=x_prev(I0)
      enddo
      found_lmin=.false.
      do I0=1,atom_num_a
        J0=I0*ndim_fac
        do I1=atom_num_a+1,atom_num_b
          J1=I1*ndim_fac
          do I2=0,ndim_fac-1
            x(J0-I2)=x_prev(J1-I2)
            x(J1-I2)=x_prev(J0-I2)
          enddo
          call min_PES
          if(pot.lt.pot_lmin)then
            found_lmin=.true.
            pot_lmin=pot
            do I3=1,ndim
              x_lmin(I3)=x(I3)
            enddo 
          endif  
        enddo
      enddo
      if(found_lmin)then
        pot=pot_lmin
        do I0=1,ndim
          x(I0)=x_lmin(I0)
        enddo
      endif
      return
      end

      subroutine operator_inversion
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      real*8 x_temp
11    J0=dint(rnd()*dble(ndim))+1
      J1=dint(rnd()*dble(ndim))+1
      if(J0.gt.ndim.or.J1.gt.ndim.or.J1.eq.J0)
     &goto 11
      x_temp=x(J0)
      x(J0)=x(J1)
      x(J1)=x_temp
      return
      end

      subroutine operator_permutation
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      real*8 x_temp
11    J0=ndim_fac*(dint(rnd()*dble(atom_num_a))+1)
      J1=ndim_fac*(dint(rnd()*dble(atom_num_b))+atom_num_a+1)
      if(J0.gt.ndim_a.or.J1.gt.ndim_b)goto 11
      do I0=0,ndim_fac-1
        x_temp=x(J0-I0)
        x(J0-I0)=x(J1-I0)
        x(J1-I0)=x_temp
      enddo
      return
      end

      subroutine operator_arithmetic
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      do I0=1,ndim
        x(I0)=0.5D0*(x(I0)+x2(I0))
      enddo
      return
      end

      subroutine operator_geometic
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      do I0=1,ndim
        x(I0)=dsqrt(dabs(x(I0)*x2(I0)))
      enddo
      return
      end

      subroutine operator_crossing
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      real*8 x_temp
      do I0=1,ndim
        if(rnd().ge.0.5D0)then
          x_temp=x2(I0)
          x2(I0)=x(I0)
          x(I0)=x_temp
        endif
      enddo
      return
      end

      subroutine operator_twopoint
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      real*8 x_temp(ndim_max),x2_temp(ndim_max)
11    J0=dint(rnd()*dble(ndim))+1
      if(J0.gt.ndim)goto 11
      do I0=1,ndim
        x_temp(I0)=x(I0)
        x2_temp(I0)=x2(I0)
      enddo
      J1=ndim-J0
      do I0=1,J1
        x(I0)=x2_temp(J0+J1)
        x2(I0)=x_temp(J0+J1)
      enddo
      J2=0
      do I0=I1+1,ndim
        J2=J2+1
        x(I0)=x_temp(J2)
        x2(I0)=x2_temp(J2)
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
