*========================================================================
* File Name : simulation_file.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Wed 16 Feb 2011 05:07:43 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine simulation_file
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/life.h"
      include "../include/ensemble.h"
      include "../include/job.h"
      include "../include/pes.h"
      include "../include/tester/tester.h"
      include "../include/simulation.h"
      include "../include/BIMD/BIMD.h"
      include "../include/BIMD/BIMD_restore.h"
      include "../include/PTMC/PTMC.h"
      source_file_flag=1
      source_type="non"
      call source_assignment
     
      simulation_thermal_file=
     &result_root_path(:index(result_root_path," ")-1)//
     &dat_path(:index(dat_path," ")-1)//simulation_type//
     &"_dat_"//pes_type(:index(pes_type," ")-1)//"_"//
     &config_name(:index(config_name," ")-1)//".dat"

      simulation_restore_file=
     &result_root_path(:index(result_root_path," ")-1)//
     &rec_path(:index(rec_path," ")-1)//simulation_type//
     &"_rec_"//pes_file_name(:index(pes_file_name," ")-1)//
     &".rec"
      simulation_restore_history=
     &result_root_path(:index(result_root_path," ")-1)//
     &rec_path(:index(rec_path," ")-1)//simulation_type//
     &"_rec_"//pes_file_name(:index(pes_file_name," ")-1)//
     &".min"

      simulation_thermal_ref=simulation_thermal_file
     &(:index(simulation_thermal_file,".")-1)//".ref"
      simulation_pes_ref=
     &result_root_path(:index(result_root_path," ")-1)//
     &xyz_path(:index(xyz_path," ")-1)//simulation_type//
     &"_xyz_"//pes_type(:index(pes_type," ")-1)//"_"//
     &config_name(:index(config_name," ")-1)//".ref"

      simulation_usr_file=
     &result_root_path(:index(result_root_path," ")-1)//
     &mom_path(:index(usr_path," ")-1)//simulation_type//"_mom_"//
     &pes_file_name(:index(pes_file_name," ")-1)//".mom"

      simulation_cnl_file=
     &result_root_path(:index(result_root_path," ")-1)//
     &cnl_path(:index(cnl_path," ")-1)//simulation_type//"_cnl_"//
     &pes_file_name(:index(pes_file_name," ")-1)//".cnl"

      simulation_anl_file=
     &result_root_path(:index(result_root_path," ")-1)//
     &anl_path(:index(anl_path," ")-1)//simulation_type//"_anl_"//
     &pes_file_name(:index(pes_file_name," ")-1)//".anl"

      simulation_xyz_file=
     &result_root_path(:index(result_root_path," ")-1)//
     &xyz_path(:index(xyz_path," ")-1)//simulation_type//"_xyz_"//
     &pes_file_name(:index(pes_file_name," ")-1)//".xyz"
C========unformatted simple output for analysis=======================
      simulation_x_unformatted_file=
     &result_root_path(:index(result_root_path," ")-1)//
     &xyz_path(:index(xyz_path," ")-1)//simulation_type//"_ufx_"//
     &pes_file_name(:index(pes_file_name," ")-1)//".ufx"

      simulation_g_unformatted_file=
     &result_root_path(:index(result_root_path," ")-1)//
     &xyz_path(:index(xyz_path," ")-1)//simulation_type//"_ufg_"//
     &pes_file_name(:index(pes_file_name," ")-1)//".ufg"

      simulation_v_unformatted_file=
     &result_root_path(:index(result_root_path," ")-1)//
     &xyz_path(:index(xyz_path," ")-1)//simulation_type//"_ufv_"//
     &pes_file_name(:index(pes_file_name," ")-1)//".ufv"

      simulation_e_unformatted_file=
     &result_root_path(:index(result_root_path," ")-1)//
     &xyz_path(:index(xyz_path," ")-1)//simulation_type//"_ufe_"//
     &pes_file_name(:index(pes_file_name," ")-1)//".ufe"

      simulation_vfx_unformatted_file=
     &result_root_path(:index(result_root_path," ")-1)//
     &xyz_path(:index(xyz_path," ")-1)//simulation_type//"_vfx_"//
     &pes_file_name(:index(pes_file_name," ")-1)//".ufv"

C===================================================
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
