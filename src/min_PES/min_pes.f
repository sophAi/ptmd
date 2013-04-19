*========================================================================
* File Name : min_pes.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年10月15日 (週五) 16時39分39秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
        subroutine min_pes
        implicit none
        include "../../include/global_common.h"
        include "../../include/common.h"
        include "../../include/file.h"
        include "../../include/pes.h"
        real*8 rcut_unit_bac
        rcut_unit_bac=rcut_unit
        rcut_unit=999999999.D0   ! Skip the BIMD evaperation option
        if (min_PES_method.EQ.0)then
C don't calculate pes at all
        else if (min_PES_method.eq.1)then
          call lbfgs_min
        else if(min_PES_method.eq.2)then
          call simplex_min
        else if(min_PES_method.eq.3)then
          call lbfgs_min
          call simplex_min
        else if(min_PES_method.eq.4)then
          call simplex_min
          call lbfgs_min
        else if(min_PES_method.eq.5)then
          call pes
        endif
        rcut_unit=rcut_unit_bac
        return
        end

        subroutine backup_pes(backup_method)
        implicit none
        include "../../include/global_common.h"
        include "../../include/common.h"
        include "../../include/pes.h"
        character backup_method*6
        if(backup_method.eq."backup")then
          pot_prev=pot
          RMS_prev=RMS
          do J1=1,ndim
            grad_prev(J1)=grad(J1)
            x_prev(J1)=x(J1)
          enddo
          do J1=1,atom_num
            bond_num_prev(J1)=bond_num(J1)
            pot_per_atom_prev(J1)=pot_per_atom(J1)
            do J2=1,atom_num
              if(J1.eq.J2)then
                dist_prev(J1,J2)=dist(J1,J2)
              endif
            enddo
          enddo
        else if(backup_method.eq."resume")then
          pot=pot_prev
          RMS=RMS_prev
          do J1=1,ndim
            grad(J1)=grad_prev(J1)
            x(J1)=x_prev(J1)
          enddo
          do J1=1,atom_num
            bond_num(J1)=bond_num_prev(J1)
            pot_per_atom(J1)=pot_per_atom_prev(J1)
            do J2=1,atom_num
              if(J1.eq.J2)then
                dist(J1,J2)=dist_prev(J1,J2)
              endif
            enddo
          enddo
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
