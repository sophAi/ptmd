*========================================================================
* File Name : top.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Mon 04 Oct 2010 04:07:10 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine topology_usr_init
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/pes.h"
       include "../include/tools/usr.h"
       integer top_atom_id
       if(wscreen) write(*,"(I5,1x,A22)") myid,"Reading topology file!"
       open(21,file="topology.dat",status="old")
       read(21,*)usr_top_atom_num
       if(usr_top_atom_num.ne.atom_num)then
         write(*,*) "Error!!"
         stop
       endif
       do I0=1,atom_num
         read(21,*)top_atom_id,usr_top_matrix(I0)
         if(usr_top_matrix(I0).eq.0)usr_top_atom_num=usr_top_atom_num-1
       enddo
       close(21) 
       return
       end

       subroutine topology_usr
       implicit none
       include "../include/global_common.h"
       include "../include/common.h"
       include "../include/pes.h"
       include "../include/tools/usr.h"
       do I0=1,atom_num
         if(usr_top_matrix(I0).eq.0)then
           dist_ctd(I0)=0.D0
           dist_cst(I0)=0.D0
           dist_fct(I0)=0.D0
           dist_ftf(I0)=0.D0
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
