*========================================================================
* File Name : comvel.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 西元2010年08月09日 (週一) 09時12分21秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
        subroutine comvel   !initialized and output v
        implicit none
        include "../../include/global_common.h"
        include "../../include/common.h"
        include "../../include/pes.h"
        include "../../include/BIMD/BIMD.h"
	real*8 dummy,dumx,dumy,dumz,temp_v,t_test,gauss,sum1
        real*8 kinetic_per_atom,total_mass
!       external gauss
        kinetic_per_atom=3D0*temp*kb
        kinetic_per_dim=temp*kb
        temp_v=dsqrt(kinetic_per_atom)
        do J1=1,atom_num
          J2=3*J1
          if(top_frz_io(J1).eq.1)then
            v(J2-2)=temp_v *gauss()/dsqrt(mass(J1))
            v(J2-1)=temp_v *gauss()/dsqrt(mass(J1))
            v(J2)= temp_v *gauss()/dsqrt(mass(J1))
          else
            v(J2-2)=0.D0   !for freezed particles
            v(J2-1)=0.D0
            v(J2)=0.D0
          endif
        enddo
        dumx=0.D0
        dumy=0.D0
        dumz=0.D0
        total_mass=0.D0
        do J1=1,atom_num
          J2=J1*3
          dumx=dumx+mass(J1)*v(J2-2) 
          dumy=dumy+mass(J1)*v(J2-1) 
          dumz=dumz+mass(J1)*v(J2) 
          total_mass=total_mass+mass(J1)
        enddo
        dumx=dumx/total_mass
        dumy=dumy/total_mass
        dumz=dumz/total_mass
        do J1=1,atom_num
          J2=J1*3
          if(top_frz_io(J1).eq.1)then
            v(J2-2)=v(J2-2) - dumx
            v(J2-1)=v(J2-1) - dumy
            v(J2)=v(J2) - dumz   
          else
            v(J2-2)=0.D0
            v(J2-1)=0.D0
            v(J2)=0.D0
          endif 
        enddo
        sum1=0.D0
        do J1=1,atom_num
          J2=J1*3
          sum1=sum1+mass(J1)*(v(J2-2)**2+ v(J2-1)**2 +v(J2)**2)
        enddo
        t_test=(sum1 / dble(atom_num))
        do J1=1,atom_num
          J2=J1*3
          if(top_frz_io(J1).eq.1)then
            v(J2-2)=v(J2-2)*dsqrt(kinetic_per_atom/t_test)
            v(J2-1)=v(J2-1)*dsqrt(kinetic_per_atom/t_test)
            v(J2)=v(J2)*dsqrt(kinetic_per_atom/t_test)
          else
            v(J2-2)=0.D0
            v(J2-1)=0.D0
            V(J2)=0.D0
          endif
        enddo
        call centre
!        call centre_velocity
!        call fix_angular
        return 
        end

        function gauss()
        implicit none
        include "../../include/global_common.h"
        include "../../include/common.h"
        include "../../include/BIMD/BIMD.h"
        real*8 b1,b3,b5,b7,b9,sum1,r,r2,i,x1,gauss
        parameter (b1=3.949846138D0 ,b3=0.252408784D0 )
        parameter (b5=0.076542912D0 ,b7=0.008355968D0 )
        parameter (b9=0.029899776D0)
        sum1=0.D0
        do I0=1,12
        call random(x1)
        sum1=sum1+x1
        end do
        r=(sum1 -6.D0)/4.D0
        r2=r*r
        gauss = (((( b9*r2 +b7 ) *r2 +b5 )*r2 +b3 )*r2+b1) *r
        return
        end


        subroutine random(x1)
        implicit none
        include "../../include/global_common.h"
        include "../../include/common.h"
        include "../../include/BIMD/BIMD.h"
        real*8 x1
        do j1=1,atom_num*3
           x1=1.D0*rnd()
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
