*========================================================================
* File Name : BIMD_output.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Wed 03 Nov 2010 04:40:51 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
       subroutine BIMD_gather_thermal
       implicit none
       include "../../include/global_common.h"
       include "../../include/common.h"
       include "../../include/file.h"
       include "../../include/life.h"
       include "../../include/ensemble.h"
       include "../../include/job.h"
       include "../../include/pes.h"
       include "../../include/simulation.h"
       include "../../include/BIMD/BIMD.h"
       integer total_par_per_line,max_temp_num
       parameter(total_par_per_line=12,max_temp_num=3000)
       integer sort_matrix(max_temp_num),sort_num,sort_temp
       real*8 thermal_real(max_temp_num,total_par_per_line)
147    format(F15.6,1x,F10.5,7(1x,F12.6),1x,F15.0,1x,F10.7,1x,F14.1)
C       if(evap_num.gt.(final_loop-init_loop+1.D0)/4.D0)return
       call check_value(BIMD_Cv)
       if(NaN_flag.or.Inf_flag)return
       file_name=simulation_thermal_ref
     &(:index(simulation_thermal_ref," ")-1)
       call wait_till_file_close(file_name)
       open(20,file=file_name,status="replace")
       write(20,*)
     &"Time_label Temp Cv ave_bond ave_eng ave_pot ave_kinetic "
     &//"ave_temper temp_kb evap_num dt final_loop"
       write(20,*) "total_parameters_per_line=",total_par_per_line
       close(20)
       file_name=simulation_thermal_file
     &(:index(simulation_thermal_file," ")-1)
       call wait_till_file_close(file_name)
       open(21,file=file_name,access="append")
       write(21,147)
     &time_label,temp,BIMD_Cv,BIMD_ave_bond,BIMD_ave_energy,
     &BIMD_ave_pot,BIMD_ave_kinetic,
     &BIMD_ave_temper,temp*kb,dble(evap_num),simulation_delta_time,
     &final_loop
C       write(*,*) BIMD_ave_temper
       close(21)       
C==============Sorting thermal propertites========================
       return
       if(unfinished_job_num.ne.1)return
       call count_file_line(file_name)
       if(file_line_num.gt.max_temp_num)return
       sort_num=file_line_num
       call wait_till_file_close(file_name)
       open(22,file=file_name,status="old")
       do I0=1,file_line_num
         read(22,*)(thermal_real(I0,I1),I1=1,total_par_per_line)
         write(*,*)(thermal_real(I0,I1),I1=1,total_par_per_line)
       enddo
       close(22)
       do I0=1,file_line_num
         sort_matrix(I0)=I0
       enddo
       do I0=1,file_line_num
         do I1=I0+1,file_line_num
           if(thermal_real(sort_matrix(I0),2).gt.
     &thermal_real(sort_matrix(I1),2).and.
     &thermal_real(sort_matrix(I1),1).eq.time_label.and.
     &thermal_real(sort_matrix(I0),1).eq.time_label)then
             sort_temp=sort_matrix(I0)
             sort_matrix(I0)=sort_matrix(I1)
             sort_matrix(I1)=sort_temp
           endif
         enddo
       enddo
C       do I0=1,file_line_num
C         do I1=I0+1,file_line_num
C           if(thermal_real(sort_matrix(I0),1).gt.
C     &thermal_real(sort_matrix(I1),1)) then
C             sort_temp=sort_matrix(I0)
C             sort_matrix(I0)=sort_matrix(I1)
C             sort_matrix(I1)=sort_temp
C           endif
C         enddo
C       enddo
       call wait_till_file_close(file_name)
       call count_file_line(file_name)
       if(sort_num.ne.file_line_num)then
         write(*,"(I5,1x,A29)")myid, "Thermal file has been changed"
         return
       endif
       open(23,file=file_name,status="replace")
       do I0=1,sort_num
         write(23,147) (thermal_real(sort_matrix(I0),I1),I1=1,
     &total_par_per_line)
       enddo
       close(23)
C=============ene of sortin=======================================
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
