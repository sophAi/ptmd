*========================================================================
* File Name : VACF_main.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 西元2010年07月22日 (週四) 10時41分12秒
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
      subroutine VACF_main
      implicit none
      include "mpif.h"
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/ensemble.h"
      include "../../include/pes.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/PTMC/PTMC.h"
      include "../../include/VACF/VACF.h"
      integer temp_int
      pi=read_unit("Pi ","Pi ","1 ")
C        file_name=VACF_file_name(:index(VACF_file_name," ")-1)
      VACF_input_name=source_file(:index(source_file," ")-1)
c        call check_file_exist(file_name)
c        call wait_till_file_close(file_name)
C        call count_file_line(file_name)
C        VACF_total_time_step=dint(dble(file_line_num)/dble(atom_num+2))
C        write(*,*) "done",VACF_total_time_step,file_line_num
      call temp_assignment
      call VACF_file
C      file_name=output_path(:index(output_path," ")-1)//
C     &VACF_file_name(:index(VACF_file_name," ")-1)
      call check_file_exist(VACF_input_name)
      if(.not.file_exist.and.wscreen)then
        write(*,"(I5,1x,A10,1x,A80)") myid,"Error! no ",VACF_input_name
        return
      endif
C        write(*,"(I4,1x,A13,1x,A80)") myid,"Reading file=",file_name
C      endif
C      call wait_till_file_close(file_name)
C      call count_file_line(file_name)
C      total_time_step=dint(dble(file_line_num)/dble(atom_num+2))
C      total_time_step=1000000
C      if(wscreen)write(*,"I4,1x,A16,1x,I13")myid,"Total time step=",
C     &total_time_step
C=============Start Calculate VACF======================
      if(VACF_method.eq.1.or.VACF_method.eq.2)then
        call histogram_init
        call VACF
      endif
      if(VACF_method.eq.3)call GVACF
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
