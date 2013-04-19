*========================================================================
* File Name : target_assignment.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 18-10-2010
* Last Modified : Mon 21 Feb 2011 10:52:14 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : 
* Description : 
*========================================================================
      subroutine target_assignment_init
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/ensemble.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/tester/tester.h"
      target_file_name="none"
      target_file_type="non"
      target_type="non"
      return
      end

      subroutine target_assignment
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/ensemble.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/tester/tester.h"
      integer temp_int
      root_path=output_path(:index(output_path," ")-1)
      if(target_file_type.eq."dat")then
        target_type_path=dat_path(:index(dat_path," ")-1)
      else if(target_file_type.eq."rec")then
        target_type_path=rec_path(:index(rec_path," ")-1)
      else if(target_file_type.eq."xyz")then
        target_type_path=xyz_path(:index(xyz_path," ")-1)
      else if(target_file_type.eq."pdb")then
C
      else if(target_file_type.eq."ufe")then
        target_type_path=xyz_path(:index(xyz_path," ")-1)
      else if(target_file_type.eq."ufx")then
        target_type_path=xyz_path(:index(xyz_path," ")-1)
      else if(target_file_type.eq."ufv")then
        target_type_path=xyz_path(:index(xyz_path," ")-1)
      else if(target_file_type.eq."ufg")then
        target_type_path=xyz_path(:index(xyz_path," ")-1)
      else if(target_file_type.eq."his")then
        target_type_path=his_path(:index(his_path," ")-1)
      else if(target_file_type.eq."mom")then
        target_type_path=mom_path(:index(mom_path," ")-1)
      else if(target_file_type.eq."scr")then
        target_type_path=scr_path(:index(scr_path," ")-1)
      else if(target_file_type.eq."cor")then
        target_type_path=cor_path(:index(cor_path," ")-1)
      else if(target_file_type.eq."fft")then
        target_type_path=fft_path(:index(fft_path," ")-1)
      else if(target_file_type.eq."dtf")then
        target_type_path=dtf_path(:index(dtf_path," ")-1)
      else if(target_file_type.eq."cnl")then
        target_type_path=cnl_path(:index(cnl_path," ")-1)
      else if(target_file_type.eq."anl")then
        target_type_path=anl_path(:index(anl_path," ")-1)
      endif

      if(target_file_flag.eq.1)then
        target_simulation_path=bimd_path(:index(bimd_path," ")-1)
        target_simulation_type="BIMD"
      else if(target_file_flag.eq.2)then
        target_simulation_path=ptmc_path(:index(ptmc_path," ")-1)
        target_simulation_type="PTMC"
      endif
      if(pes_type.ne."none")then
        call temp_assignment
        temp_int=dint(temp)
        call int2char4(temp_int,parallel_parameter)
        call check_config_name
        target_pes_file_name=pes_type(:index(pes_type," ")-1)//
     &"_"//config_name(:index(config_name," ")-1)//"_"//
     &parallel_parameter
      endif
      target_result_root_path=root_path(:index(root_path," ")-1)//
     &simulation_path(:index(simulation_path," ")-1)

      target_path=target_result_root_path
     &(:index(target_result_root_path," ")-1)//
     &target_type_path(:index(target_type_path," ")-1)
C      write(*,*) pes_file_name,target_path,result_root_path
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
