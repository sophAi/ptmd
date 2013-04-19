*========================================================================
* File Name : angular.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Wed 16 Feb 2011 04:14:26 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine angular_velocity
!Before fix angular momentum, you should fix center of the position 
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/pes.h"
       real*8 ax,ay,az,total_angular,total_angular_moment
       real*8 angular_temp1,angular_temp2,angular_temp3
       real*8 rx,ry,rz,d(atom_num_max)
       ax=0.D0
       ay=0.D0
       az=0.D0
       total_angular_moment=0.D0
       do I1=1,atom_num
         I2=I1*3
         ax=ax+mass(I1)*(x(I2-1)*v(I2)-x(I2)*v(I2-1))
         ay=ay+mass(I1)*(x(I2)*v(I2-2)-x(I2-2)*v(I2))
         az=az+mass(I1)*(x(I2-2)*v(I2-1)-x(I2-1)*v(I2-2))
       enddo       
       total_angular=dsqrt(ax**2+ay**2+az**2)
       ax=ax/total_angular
       ay=ay/total_angular
       az=az/total_angular
       do I1=1,atom_num
         I2=I1*3
         d(I1)=dsqrt((x(I2-1)*az-x(I2)*ay)**2
     &+(x(I2)*ax-x(I2-2)*az)**2+(x(I2-2)*ay-x(I2-1)*ax)**2)
         total_angular_moment=
     &total_angular_moment+mass(I1)*d(I1)**2
       enddo
       total_angular_moment=total_angular/total_angular_moment
       do I1=1,atom_num
         I2=I1*3
         angular_temp1=(x(I2-2)*ax+x(I2-1)*ay+x(I2)*az)/
     &total_angular**2
         rx=(x(I2-2)-ax*angular_temp1)/d(I1)
         ry=(x(I2-1)-ay*angular_temp1)/d(I1)
         rz=(x(I2)-az*angular_temp1)/d(I1)
         v(I2-2)=v(I2-2)+d(I1)*total_angular_moment*
     &(ry*az-rz*ay)
         v(I2-1)=v(I2-1)+d(I1)*total_angular_moment*
     &(rz*ax-rx*az)
         v(I2)=v(I2)+d(I1)*total_angular_moment*
     &(rx*ay-ry*ax)
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
