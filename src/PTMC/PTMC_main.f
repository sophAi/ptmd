*========================================================================
* File Name : PTMC_main.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年10月25日 (週一) 07時32分41秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
C===========================================
C Dimensional units
C Length=10^-10m
C Time=10^-12s
C Energy=10^-3ev
C===========================================
      subroutine PTMC_main
      implicit none
      include "mpif.h"
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/ensemble.h"
      include "../../include/pes.h"
      include "../../include/PTMC/PTMC.h"
      integer temp_int
      call temp_assignment
      temp_int=dint(temp)
      call PTMC_thermal_init
      call step_move_init
      call PTMC
      call PTMC_thermal
      call PTMC_gather_thermal
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
