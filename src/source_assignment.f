*========================================================================
* File Name : source_assignment.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 18-10-2010
* Last Modified : 2011年02月20日 (週日) 17時52分44秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : 
* Description : 
*========================================================================
      subroutine source_assignment_init
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/ensemble.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/tester/tester.h"
      source_file_name="none"
      source_type="non"
      source_file_type="non"
      return
      end
      subroutine source_assignment
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/ensemble.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/tester/tester.h"
      integer temp_int
      root_path=output_path(:index(output_path," ")-1)
      if(source_file_type.eq."dat")then
        type_path=dat_path(:index(dat_path," ")-1)
      else if(source_file_type.eq."rec")then
        type_path=rec_path(:index(rec_path," ")-1)
      else if(source_file_type.eq."xyz")then
        type_path=xyz_path(:index(xyz_path," ")-1)
      else if(source_file_type.eq."pdb")then
C
      else if(source_file_type.eq."ufe")then
        type_path=xyz_path(:index(xyz_path," ")-1)
      else if(source_file_type.eq."ufx")then
        type_path=xyz_path(:index(xyz_path," ")-1)
      else if(source_file_type.eq."ufv")then
        type_path=xyz_path(:index(xyz_path," ")-1)
      else if(source_file_type.eq."ufg")then
        type_path=xyz_path(:index(xyz_path," ")-1)
      else if(source_file_type.eq."his")then
        type_path=his_path(:index(his_path," ")-1)
      else if(source_file_type.eq."mom")then
        type_path=mom_path(:index(mom_path," ")-1)
      else if(source_file_type.eq."scr")then
        type_path=scr_path(:index(scr_path," ")-1)
      else if(source_file_type.eq."cor")then
        type_path=cor_path(:index(cor_path," ")-1)
      else if(source_file_type.eq."fft")then
        type_path=fft_path(:index(fft_path," ")-1)
      else if(source_file_type.eq."dtf")then
        type_path=dtf_path(:index(dtf_path," ")-1)
      else if(source_file_type.eq."cnl")then
        type_path=cnl_path(:index(cnl_path," ")-1)
      else if(source_file_type.eq."anl")then
        type_path=anl_path(:index(anl_path," ")-1)
      endif

      if(source_file_flag.eq.1)then
        simulation_path=bimd_path(:index(bimd_path," ")-1)
        simulation_type="BIMD"
      else if(source_file_flag.eq.2)then
        simulation_path=ptmc_path(:index(ptmc_path," ")-1)
        simulation_type="PTMC"
      endif
      if(pes_type.ne."none")then
        call temp_assignment
        temp_int=dint(temp)
        call int2char4(temp_int,parallel_parameter)
        call check_config_name
        pes_file_name=pes_type(:index(pes_type," ")-1)//
     &"_"//config_name(:index(config_name," ")-1)//"_"//
     &parallel_parameter
      endif
      result_root_path=root_path(:index(root_path," ")-1)//
     &simulation_path(:index(simulation_path," ")-1)

      source_path=result_root_path(:index(result_root_path," ")-1)//
     &type_path(:index(type_path," ")-1)
C      write(*,*) pes_file_name,source_path,result_root_path
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
