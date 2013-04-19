*========================================================================
* File Name : ndim_assignment.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 2010-11-25
* Last Modified : 2010年11月25日 (週四) 10時29分15秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : 
* Description : 
*========================================================================

      subroutine ndim_assignment
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      ndim=atom_num*ndim_fac
      do I0=1,atom_num
        do I1=1,ndim_fac
          ndim_id(I0,I1)=ndim_fac*(I0-1)+I1
        enddo
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
