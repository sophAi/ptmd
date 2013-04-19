*========================================================================
* File Name : bond_vector_init.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2013-04-16 21:32:19
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine bond_vector_init
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/job.h"
      include "../../include/pes.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/bond_vector.h"
      include "../../include/BIMD/BIMD.h"
      total_bond_num=atom_num
      bond_ndim=total_bond_num*3
      do I0=1,total_bond_num
        I1=I0*3
        bond_id(I0,1)=dint(x(I1-2))
        bond_id(I0,2)=dint(x(I1-1))
      enddo 
      if(wscreen)then
        write(*,"(I5,1x,A23)")myid,"Generating bond vectors"
        write(*,"(I5,1x,A12,1x,I5)") myid,"Total bonds=",total_bond_num
        write(*,"(I5,1x,A10,F10.1,1x,A11,1x,F10.1,1x,A11,1x,F10.1)")
     &myid,"Init_loop=",init_loop,",final_loop=",
     &final_loop,",delta_loop=",delta_loop
      endif
      return
      end

      subroutine bond_vector_header(rec_file_type)
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/job.h"
      include "../../include/pes.h"
      include "../../include/header.h"
      include "../../include/simulation.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/bond_vector.h"
      character rec_file_type*3
      header_source_type="ufv"
      header_par_num=7
      header_par_name(1)="time_label"
      header_par_real(1)=time_label
      header_par_name(2)="ndim_fac"
      header_par_real(2)=ndim_fac
      header_par_name(3)="atom_num"
      header_par_real(3)=atom_num
      header_par_name(4)="file_x_dim"
      header_par_real(4)=dble(file_x_dim)
      header_par_name(5)="file_y_dim"
      header_par_real(5)=dble(file_y_dim)
      header_par_name(6)="simulation_ufv_loop"
      header_par_real(6)=simulation_ufv_loop
      header_par_name(7)="simulation_delta_time"
      header_par_real(7)=simulation_delta_time
      header_annotation="'vector(1)~(3n)'"
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
