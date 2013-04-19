*========================================================================
* File Name : edit_BIMD.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Wed 16 Feb 2011 05:14:13 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_BIMD
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/file.h"
      include "../../include/job.h"
      include "../../include/ensemble.h"
      include "../../include/simulation.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/parameter.h"
      character yn*1,sel*2
      write(*,*) "<<<<BIMD MENU>>>>"
      call edit_temp
      source_type="non"
      sel="99"
      do while(sel.ne."0")
        call show_parameter(0,"quit ")
        call show_parameter(1,"init_loop ")
        call show_parameter(2,"final_loop ")
        call show_parameter(3,"simulation_delta_time ")
        call show_parameter(4,"simulation_reset_thermal ")
        write(*,*) " 5.Change the record interval"
        call show_parameter(6,"simulation_min_method ")
        write(*,*)"-------------------------------------------"
        read(*,*) sel
C=====================================================
        if(sel.eq."1")then
          call edit_parameter("init_loop ")
          if(mod(init_loop,1000.D0).eq.0.D0.
     &and.mod(final_loop,1000.D0).eq.0)then
            write(*,*) "<<REMINDER>>"
            write(*,*) "PJ:It seems that your total time step is ",
     &final_loop-init_loop+1.D0
            write(*,*) "not ",final_loop-init_loop
            write(*,*)
     &"And for the 1.D6 restoring interval,it will read the restoring"
            write(*,*)"at ",init_loop,"-1.D6 or 0"
            write(*,*)"which may cause the computing time longer"
            write(*,*)
     &"Would you like me to add 1 step for your initial time(y/n)"
            read(*,*)yn
            if(yn.eq."y")then
              par_real8_config=init_loop+1
              init_loop=par_real8_config
              call name2parameter("init_loop ","w")
              write(*,*) "Your initial time is ",init_loop
              write(*,*)"And your total time step will be ",
     &final_loop-init_loop+1.D0
            endif
          endif
        endif
        if(sel.eq."2")call edit_parameter("final_loop ")
        if(sel.eq."3")then
          call edit_parameter("simulation_delta_time ")
          write(*,"(A21,1x,F12.8,1x,A19)")
     &"The time interval is ",simulation_delta_time,
     &"pico second(10^-12s)"
          write(*,"(A28,1x,F30.20,1x,A6,1x,F15.8,1x,A19)")
     &"The real simulation time is ",
     &(final_loop-init_loop+1.D0)*simulation_delta_time*1.D-12,"sec or",
     &(final_loop-init_loop+1.D0)*simulation_delta_time,
     &"pico second(10^-12s)"
        endif
        if(sel.eq."4")then
          write(*,*) 
     &"Please select the starting point for calculating average"
          write(*,"(A48)")
     &"1.Calculate thermal quantities from the 1st step"
          write(*,"(A39,1x,F13.1,1x,A8)")
     &"2.Calculate thermal quantities from the",init_loop,"'th step"
          call edit_parameter("simulation_reset_thermal ")
        endif
        if(sel.eq."5")then
          call edit_simulation_record
        endif
        if(sel.eq."6")then
          write(*,*)
     &"Please select the minimization method for records"
          write(*,*) "in BIMD"
          write(*,*) "0.Skip anything"
          write(*,*) "1.LBFGS method"
          write(*,*) "2.Simplex method"
          write(*,*) "3.LBFGS then simplex method"
          write(*,*) "4.Simplex then LBFGS method"
          write(*,*) "5.Calculate potential without minimizing"
          write(*,*) "--------------------------"
          call edit_parameter("simulation_min_method ")
        endif
      enddo
      return
      end

      subroutine write_BIMD
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/pes.h"
      include "../../include/file.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/ensemble.h"
      include "../../include/simulation.h"
      include "../../include/BIMD/BIMD.h"
      file_name=file_path(:index(file_path," ")-1)//"tmp."//job_file
      open(20,file=file_name,access="append")
      write(20,"(I3,1x,I3,1x,A9,1x,I5,1x,A10,1x,F7.2,1x,
     &A11,1x,F7.2,1x,A11,1x,
     &F7.2,1x,A10,1x,F13.1,1x,A11,1x,F13.1,1x,A11,1x,F10.7,
     &1x,A14,1x,I2,1x,A20,1x,F13.1,1x,A20,1x,F13.1,1x,
     &A20,1x,F13.1,1x,A20,1x,F13.1,1x,A20,1x,F13.1,1x,A20,1x,F13.1,1x,
     &A20,1x,F13.1,1x,A11,1x,I2)")
     &15,0,
     &"ndim_fac=",ndim_fac,
     &"init_temp=",init_temp,
     &"final_temp=",final_temp,
     &"delta_temp=",delta_temp,
     &"init_loop=",init_loop,
     &"final_loop=",final_loop,
     &"delta_time=",simulation_delta_time,
     &"reset_thermal=",simulation_reset_thermal,
     &"simulation_rec_loop=",simulation_rec_loop,
     &"simulation_xyz_loop=",simulation_xyz_loop,
     &"simulation_ufe_loop=",simulation_ufe_loop,
     &"simulation_ufx_loop=",simulation_ufx_loop,
     &"simulation_ufv_loop=",simulation_ufv_loop,
     &"simulation_ufg_loop=",simulation_ufg_loop,
     &"simulation_vfx_loop=",simulation_vfx_loop,
     &"min_method=",simulation_min_method
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
