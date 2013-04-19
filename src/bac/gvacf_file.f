*========================================================================
* File Name : VACF_file.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年10月22日 (週五) 15時21分58秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine gvacf_file
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/life.h"
      include "../../include/ensemble.h"
      include "../../include/job.h"
      include "../../include/pes.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/tester/gvacf.h"
      include "../../include/tester/tester.h"
      source_type="ufv"
      call file_assignment
      VACF_path_name=output_path(:index(output_path," ")-1)//
     &bimd_path(:index(bimd_path," ")-1)
      if(source_file_flag.eq.1.or.source_file_flag.eq.2)then
        VACF_input_name=output_path(:index(output_path," ")-1)//
     &bimd_path(:index(bimd_path," ")-1)//
     &xyz_path(:index(xyz_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//".xyz"
      if(source_file_flag.eq.0)then
        VACF_input_name=source_file(:index(source_file," ")-1)
        source_file=source_file(:index(source_file,".")-1)//"_"
      else
        VACF_input_name=source_path(:index(source_file," ")-1)//
     &simulation_type//"_"//type_path//"_"//
     &pes_file_name(:index(pes_file_name," ")-1)//"."//source_type

        VACF_org_vaf_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &vaf_path(:index(vaf_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//"_org.vaf"
        VACF_nor_vaf_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &vaf_path(:index(vaf_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//"_nor.vaf"
        VACF_real_dft_name=VACF_path_name
     &(:index(VACF_path_name," ")-1)//
     &psd_path(:index(psd_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//
     &"_real.dft"
      VACF_img_dft_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &psd_path(:index(psd_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//"_img.dft"
      VACF_diff_name=VACF_path_name(:index(VACF_path_name," ")-1)//
     &dtf_path(:index(dtf_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//".diff"
      VACF_diff_dat_name=VACF_path_name
     &(:index(VACF_path_name," ")-1)//
     &dtf_path(:index(dtf_path," ")-1)//
     &source_file(:index(source_file," ")-1)//"diff.dat"
      VACF_hist_dat_name=VACF_path_name
     &(:index(VACF_path_name," ")-1)//
     &his_path(:index(his_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//
     &"_hist.dat"
      VACF_hist_xyz_name=VACF_path_name
     &(:index(VACF_path_name," ")-1)//
     &his_path(:index(his_path," ")-1)//
     &source_file(:index(source_file," ")-1)//int2char//
     &"_hist.xyz"
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
