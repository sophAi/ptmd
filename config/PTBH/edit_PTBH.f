*========================================================================
* File Name : 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_PTMC
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/file.h"
      include "../../include/job.h"
      include "../../include/ensemble.h"
      include "../../include/BIMD/BIMD.h"
99    format(I5,1x,A12,A3)
100   format(A10,1x,I5,1x,A3,1x,A5,1x,I5,1x,A3)
      character global_file_name*60,yn*1
      write(*,*) "<<<<PTBH MENU>>>>"
      write(*,*) "please input the temperature range"
      write(*,*) "From(default=50.D0)"
      read(*,*) init_temp
      write(*,*) "To(default=1500.D0)"
      read(*,*) final_temp
      if(init_temp.eq.final_temp)then
        delta_temp=0.D0
      else
14      write(*,*) "Please input the interval of the temperature"
        write(*,*) "(default=100.D0)"
        read(*,*) delta_temp
        if(delta_temp.eq.0.D0.and.(final_temp-init_temp).ne.0.D0)then
          write(*,*) "Warning!This intervel of temperature will cause 
     &incorrect ensemble!"
          write(*,*) "Please try again!"
          write(*,*) "You could use default value."
          goto 14
        endif
      endif
      if(delta_temp.eq.0.D0.and.(final_temp-init_temp).eq.0.D0)then
        delta_temp=1.D0
        ensemble_num=1
      else
        ensemble_num=dint((final_temp-init_temp)/delta_temp)+1
      endif
      write(*,*) "The ensemble number is ",ensemble_num
      write(*,*) 
      write(*,*) "Please input the PTBH Step"
      write(*,*) "From(default=1.D0)"
      read(*,*) init_loop
      write(*,*) "To(default=1.D8)"
      read(*,*) final_loop
      write(*,*) "Record xyz?(y/n)"
      read(*,*) yn
      if(yn.eq."y")then
        BIMD_record_xyz=.true.
        write(*,*) "Record xyz every N steps(default=10)"
        write(*,*) "N="
        read(*,*) BIMD_rec_interval
        write(*,"A47,F10.6") 
     &" When you calculate VACF,your time_interval is ",
     &dble(BIMD_rec_interval)*dble(delta_time)
      else
        BIMD_record_xyz=.false.
        write(*,*) 
     &"Backup restoring point every N steps (default=1.D6)"
        write(*,*) "N="
        read(*,*) BIMD_rec_interval
      endif
      write(*,*) 
      return
      end

      subroutine write_PTMC
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/file.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/ensemble.h"
      include "../../include/BIMD/BIMD.h"
      integer record_xyz_flag,reset_thermal_flag
      if(BIMD_reset_thermal)then
        reset_thermal_flag=1
      else
        reset_thermal_flag=0
      endif
      if(BIMD_record_xyz)then
        record_xyz_flag=1
      else
        record_xyz_flag=0
      endif
      write(*,*)reset_thermal_flag
      file_name=file_path(:index(file_path," ")-1)//"tmp."//job_file
      open(20,file=file_name,access="append")
      write(*,*) "writing ",file_name
C      pause
      write(20,"I5,1x,A11,1x,I5,1x,A4,1x,I5,1x,A7,1x,I5,
     &1x,A13,1x,I5,1x,A5,1x,I5,1x,A7,1x,I5") 
     &atom_num,"atoms life=",
     &life_num,
     &"job=",job_num,
     &"job_id=",job_id,
     &"job_ensemble=",ensemble_num,
     &"loop=",loop_num,
     &"pes_id=",pes_id
      write(20,"A10,1x,F7.2,1x,A11,1x,F7.2,1x,A6,1x,
     &F6.2,1x,A10,1x,F13.1,1x,A11,1x,F13.1,1x,A6,1x,F10.7,
     &1x,A14,1x,I1,1x,A9,1x,I1,1x,A13,1x,F13.1")
     &"init_temp=",init_temp,
     &"final_temp=",final_temp,
     &"dTemp=",delta_temp,
     &"init_loop=",init_loop,
     &"final_loop=",final_loop,
     &"dTime=",delta_time,
     &"reset_thermal=",reset_thermal_flag,
     &"rec_xyz=",record_xyz_flag,
     &"rec_interval=",BIMD_rec_interval
      do I0=1,atom_num
        I1=I0*3
        if(I0.le.atom_num_a)then
          write(20,"A3,1x,3(F14.7)") atom_name_a,x(I1-2),x(I1-1),x(I1)
        else
          write(20,"A3,1x,3(F14.7)") atom_name_b,x(I1-2),x(I1-1),x(I1)
        endif
      enddo
      close(20)
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
