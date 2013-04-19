*========================================================================
* File Name : centre.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Tue 22 Feb 2011 04:43:14 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine centroid
C     Calculating centroid without moving all coordinate
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      include "../include/tools/usr.h"
      real*8 total_mass
      com_x=0.D0
      com_y=0.D0
      com_z=0.D0
      total_mass=0.D0
      do J0=1,atom_num
        J1=J0*3
        com_x=com_x+mass(J0)*x(J1-2)
        com_y=com_y+mass(J0)*x(J1-1)
        com_z=com_z+mass(J0)*x(J1)
        total_mass=total_mass+mass(J0)
      enddo
      com_x=com_x/total_mass
      com_y=com_y/total_mass
      com_z=com_z/total_mass
      return
      end


      subroutine centroid_usr
C     Calculating centroid without moving all coordinate
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      include "../include/tools/usr.h"
      real*8 total_mass
      com_x=0.D0
      com_y=0.D0
      com_z=0.D0
      total_mass=0.D0
      do I0=1,top_usr_num
        J0=top_usr_id(I0)
        J1=J0*3
        com_x=com_x+mass(J0)*x(J1-2)
        com_y=com_y+mass(J0)*x(J1-1)
        com_z=com_z+mass(J0)*x(J1)
        total_mass=total_mass+mass(J0)
      enddo
      com_x=com_x/total_mass
      com_y=com_y/total_mass
      com_z=com_z/total_mass
      return
      end


C**************************************************************************
C
C  Subroutine CENTRE moves the centre of mass to the origin.
C
C*********************************************************************
      subroutine centre
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      include "../include/tools/usr.h"
      call centroid
      do I1=1,atom_num
        I2=I1*3
        x(I2-2)=x(I2-2)-com_x
        x(I2-1)=x(I2-1)-com_y
        x(I2)=x(I2)-com_z
      enddo
      return
      end

      

      subroutine centroid_velocity
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      real*8 v_total_mass
      com_vx=0.D0
      com_vy=0.D0
      com_vz=0.D0
      v_total_mass=0.D0
      do I1=1,atom_num
        I2=I1*3
        com_vx=com_vx+v(I2-2)*mass(I1)
        com_vy=com_vy+v(I2-1)*mass(I1)
        com_vz=com_vz+v(I2)*mass(I1)
        v_total_mass=v_total_mass+mass(I1)
      enddo
      com_vx=com_vx/v_total_mass
      com_vy=com_vy/v_total_mass
      com_vz=com_vz/v_total_mass
      return
      end

      subroutine centre_velocity
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/pes.h"
      call centroid_velocity
      do I1=1,atom_num
        I2=I1*3
        v(I2-2)=v(I2-2)-com_vx
        v(I2-1)=v(I2-1)-com_vy
        v(I2)=v(I2)-com_vz
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
