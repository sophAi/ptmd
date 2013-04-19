*========================================================================
* File Name : utilities.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Mon 04 Apr 2011 10:47:47 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine update_version(version,update)
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/file.h"
       character dummy
       file_name=file_path(:index(file_path," ")-1)//version_file
       open(11,file=file_name,status="old")
       read(11,*) dummy
       read(11,*) update,dummy,version
       close(11)
       return
       end

       subroutine check_value(value)
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       real*8 value
       if(value.gt.inf_const.or.value.lt.-inf_const)then
         if(wscreen) write(*,*) "Warning!! Detect ",value
         Inf_flag=.true.
         NaN_flag=.false.
       else if(value.le.inf_const.and.value.ge.-inf_const)
     &then
         NaN_flag=.false.
         Inf_flag=.false.
       else
         if(wscreen) write(*,*) "Warning!! Detect ",value
         NaN_flag=.true.
         Inf_flag=.false.
       endif
       return
       end

       subroutine check_matrix(dimension_num,matrix)
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       integer dimension_num
       real*8 matrix(3000)
       NaN_flag=.false.
       Inf_flag=.false.
       do J1=1,dimension_num
         if(matrix(J1).gt.inf_const.or.matrix(J1).lt.-inf_const)then
           write(*,*) "Warning!! Detect",matrix(J1)," at ",J1
           Inf_flag=.true.
           NaN_flag=.false.
           return
         else if
     &(matrix(J1).le.inf_const.and.matrix(J1).ge.-inf_const)then
         else
           write(*,*) "Warning@@ Detect",matrix(J1)," at ",J1
           Inf_flag=.false.
           NaN_flag=.true.
           return
         endif
       enddo
       return
       end

       subroutine check_x
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/pes.h"
       write(*,"(A9,1x,I4,1x,A6,1x,I5)") 
     &"ndim_fac=",ndim_fac,",myid=",myid
       do I0=1,atom_num
         I1=ndim_fac*I0
         write(*,"(I4,1x,A6,1x,A4,1x,A3,1x,F13.6,1x,F13.6,1x,F13.6)") 
     &I0,",atom=",atom_name(I0),",x=",x(I1-2),x(I1-1),x(I1)
       enddo
       return
       end

       subroutine check_v
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/pes.h"
       write(*,"(A9,1x,I4,1x,A6,1x,I5)")
     &"ndim_fac=",ndim_fac,",myid=",myid
       do I0=1,atom_num
         I1=ndim_fac*I0
         write(*,"(I4,1x,A6,1x,A4,1x,A3,1x,F13.6,1x,F13.6,1x,F13.6)")
     &I0,",atom=",atom_name(I0),",v=",v(I1-2),v(I1-1),v(I1)
       enddo
       return
       end

       subroutine check_g
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/pes.h"
       write(*,"(A9,1x,I4,1x,A6,1x,I5)")
     &"ndim_fac=",ndim_fac,",myid=",myid
       do I0=1,atom_num
         I1=ndim_fac*I0
         write(*,"(I4,1x,A6,1x,A4,1x,A3,1x,F13.6,1x,F13.6,1x,F13.6)")
     &I0,",atom=",atom_name(I0),",g=",grad(I1-2),grad(I1-1),grad(I1)
       enddo
       return
       end

       subroutine check_pot
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/pes.h"
       write(*,"(A5,1x,I5,1x,A5,1x,F13.8,1x,A13,1x,F13.8)")
     &"myid=",myid,",pot=",pot,",binding_pot=",pot/dble(atom_num)
       return
       end

       subroutine check_mass
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/pes.h"
       write(*,"(A9,1x,I4,1x,A6,1x,I5)")
     &"ndim_fac=",ndim_fac,",myid=",myid
       do I0=1,atom_num
         write(*,"(I4,1x,A6,1x,A4,1x,A6,1x,F15.8)")
     &I0,",atom=",atom_name(I0),",mass=",mass(I0)
       enddo
       return
       end

       subroutine check_tmass
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/pes.h"
       write(*,"(A9,1x,I4,1x,A6,1x,I5)")
     &"ndim_fac=",ndim_fac,",myid=",myid
       do I0=1,atom_num
         write(*,"(I4,1x,A6,1x,A4,1x,A7,1x,F15.8)")
     &I0,",atom=",atom_name(I0),",tmass=",tmass(I0)
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
