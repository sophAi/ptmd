*========================================================================
* File Name : reduced_unti.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Tue 19 Jul 2011 10:47:14 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine fix_reduced_unit(read_rzero)  !if no_reduced = true then transfer all unit to real unit!
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       include "../include/pes.h"
       real*8 read_rzero
       logical fix_reduced
       fix_reduced=.false.
       do J1=1,atom_num
         J3=J1*3
         do J2=1,atom_num
           J4=J2*3
           if(J1.ne.J2)then
             dist(J1,J2)=dsqrt((x(J3-2)-x(J4-2))**2+
     &(x(J3-1)-x(J4-1))**2+(x(J3)-x(J4))**2)
             if(dist(J1,J2).lt.1.D0.and.read_rzero.gt.1.D0)
     &fix_reduced=.true.
           endif
         enddo
       enddo
       if(fix_reduced)then
         if(wscreen)write(*,"(I5,1x,A18,1x,F13.8)") 
     &myid,"Multiplying rzero=",read_rzero
         do J1=1,atom_num
           J2=J1*3
           x(J2-2)=x(J2-2)*read_rzero
           x(J2-1)=x(J2-1)*read_rzero
           x(J2)=x(J2)*read_rzero
         enddo
       endif
       return
       end

       subroutine check_reduced_unit(bond_less_1_number)
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       include "../include/pes.h"
       integer bond_less_1_number,total_bond_number
       bond_less_1_number=0
       total_bond_number=0
       do J1=1,atom_num
         J3=J1*3
         do J2=1,atom_num
           J4=J2*3
           if(J1.ne.J2)then
             total_bond_number=total_bond_number+1
             dist(J1,J2)=dsqrt((x(J3-2)-x(J4-2))**2+
     &(x(J3-1)-x(J4-1))**2+(x(J3)-x(J4))**2)
             if(dist(J1,J2).lt.1.D0)
     &bond_less_1_number=bond_less_1_number+1
           endif
         enddo
       enddo
       if(bond_less_1_number.gt.0)then
         write(*,*) myid," Warning! There are ",bond_less_1_number,
     &" bond distances smaller than 1 among ",total_bond_number,
     &" total bonds"
         write(*,*) myid," You may using a coordinate with reduced unit"
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
