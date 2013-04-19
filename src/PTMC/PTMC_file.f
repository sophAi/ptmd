*========================================================================
* File Name : PTMC_file.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年10月22日 (週五) 08時51分02秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine PTMC_file
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/life.h"
      include "../../include/ensemble.h"
      include "../../include/job.h"
      include "../../include/pes.h"
      include "../../include/PTMC/PTMC.h"
      include "../../include/tester/tester.h"
      source_type="non"
      source_file_flag=2
      call file_assignment
      output_file=pes_type(:index(pes_type," ")-1)//"_"//
     &config_name(:index(config_name," ")-1)
      PTMC_thermal_file=output_path(:index(output_path," ")-1)//
     &ptmc_path(:index(ptmc_path," ")-1)//
     &dat_path(:index(dat_path," ")-1)//
     &"PTMC_thermal_"//output_file
      PTMC_pes_path=output_path(:index(output_path," ")-1)//
     &ptmc_path(:index(ptmc_path," ")-1)
      PTMC_pes_file="PTMC_pes_"//output_file
      PTMC_restore_file=PTMC_pes_path(:index(PTMC_pes_path," ")-1)//
     &rec_path(:index(rec_path," ")-1)//
     &"PTMC_restore_"//output_file
      PTMC_thermal_ref=PTMC_thermal_file
     &(:index(PTMC_thermal_file," ")-1)//".ref"
      PTMC_thermal_file=PTMC_thermal_file
     &(:index(PTMC_thermal_file," ")-1)//".dat"
      PTMC_pes_ref=PTMC_pes_path(:index(PTMC_pes_path," ")-1)
     &//xyz_path(:index(xyz_path," ")-1)
     &//PTMC_pes_file(:index(PTMC_pes_file," ")-1)
     &//"_"//int2char//".ref"
      PTMC_usr_file=PTMC_pes_path(:index(PTMC_pes_path," ")-1)
     &//PTMC_pes_file(:index(PTMC_pes_file," ")-1)
     &//usr_path(:index(usr_path," ")-1)
     &//"_"//int2char//".usr"
      PTMC_cnl_file=PTMC_pes_path(:index(PTMC_pes_path," ")-1)
     &//cnl_path(:index(cnl_path," ")-1)
     &//PTMC_pes_file(:index(PTMC_pes_file," ")-1)
     &//"_"//int2char//".cnl"
      PTMC_anl_file=
     &PTMC_pes_path(:index(PTMC_pes_path," ")-1)
     &//anl_path(:index(anl_path," ")-1)
     &//PTMC_pes_file(:index(PTMC_pes_file," ")-1)
     &//"_"//int2char//".anl"
C==========PTMC_pes_file updated====================
      PTMC_xyz_file=
     &PTMC_pes_path(:index(PTMC_pes_path," ")-1)
     &//xyz_path(:index(xyz_path," ")-1)
     &//PTMC_pes_file(:index(PTMC_pes_file," ")-1)
     &//"_"//int2char//".xyz"
C===================================================
      PTMC_restore_history=
     &PTMC_restore_file(:index(PTMC_restore_file," ")-1)
     &//"_"//int2char//".his"
      PTMC_restore_file=
     &PTMC_restore_file(:index(PTMC_restore_file," ")-1)
     &//"_"//int2char//".rec"
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
