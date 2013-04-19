*========================================================================
* File Name : BIMD_restore.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Wed 04 May 2011 10:56:40 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine BIMD_program
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/simulation.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/BIMD/BIMD_restore.h"
      include "../../include/cn/cn.h"
      integer start_loop_int,restore_max_loop_int,init_frame
      real*8 org_init_loop,org_final_loop,org_BIMD_delta_time
     &,gap_init_loop,gap_final_loop,org_simulation_rec_loop,
     &org_simulation_xyz_loop,org_simulation_ufe_loop,
     &org_simulation_ufx_loop,org_simulation_ufv_loop,
     &org_simulation_ufg_loop,org_simulation_vfx_loop
      real*8 BIMD_delta_time_vel,final_loop_vel,
     &gap_rec_loop,gap_delta_time,
     &rec_simulation_rec_loop,rec_BIMD_delta_time
      parameter(BIMD_delta_time_vel=0.0001D0,
     &final_loop_vel=1.D5)
      start_loop=1.D0   !for BIMD_thermal, if BIMD_reset_thermal=2, reset start_loop to init_loop
      org_init_loop =init_loop
      org_final_loop=final_loop
      simulation_init_loop=dint(init_loop)
      simulation_final_loop=dint(final_loop)
      org_simulation_rec_loop=simulation_rec_loop
      org_simulation_xyz_loop=simulation_xyz_loop
C      simulation_xyz_loop=1.D20
      org_simulation_ufe_loop=simulation_ufe_loop
C      simulation_ufe_loop=1.D20
      org_simulation_ufx_loop=simulation_ufx_loop
C      simulation_ufx_loop=1.D20
      org_simulation_ufv_loop=simulation_ufv_loop
C      simulation_ufv_loop=1.D20
      org_simulation_ufg_loop=simulation_ufg_loop
C      simulation_ufg_loop=1.D20
      org_simulation_vfx_loop=simulation_vfx_loop
C      simulation_vfx_loop=1.D20
      org_BIMD_delta_time=simulation_delta_time
      rec_simulation_rec_loop=simulation_rec_loop
      rec_BIMD_delta_time=simulation_delta_time
      init_frame=dint(init_loop)
      if(simulation_rec_loop.le.org_final_loop.and.
     &org_init_loop.eq.1)then  !only this condition will clean the restoring points, if init_loop>1, it will try to resume the backup point
        call clean_file(simulation_restore_file)    !will force BIMD_restore generate 0 restore_max_loop_int
        call clean_file(simulation_restore_history)
        if(wscreen)write(*,"(I5,1x,A39)")
     &myid,"Erase all corresponding restoring files" 
      endif
C=========update parameter from restoring file========
      call BIMD_restore(init_frame,start_loop_int,restore_max_loop_int)
      gap_rec_loop=simulation_rec_loop
      gap_delta_time=simulation_delta_time
      rec_simulation_rec_loop=simulation_rec_loop
      rec_BIMD_delta_time=simulation_delta_time
      gap_init_loop=dble(start_loop_int+1)
      gap_final_loop=org_init_loop-1.D0
C     if restore_max_loop_int.eq.0, no restoring file, start from the beginning.
      if(restore_max_loop_int.eq.0)then  !if ture, detect no restoring file
C       Start velocity stablizer
         if(wscreen)write(*,"(I5,1x,A36,1x,F15.0,1x,A16,1x,F13.8)")
     &myid,"Start Velocity Stablizer with steps=",final_loop_vel,
     &",and delta time=",BIMD_delta_time_vel
        init_loop=1.D0
        final_loop=final_loop_vel
        a1 = BIMD_delta_time_vel*8.0D0/3.0D0
        a5 = BIMD_delta_time_vel*3.0D0/8.0D0
        simulation_rec_loop=final_loop+1.D0 !record nothing, reduce if statement
        simulation_xyz_loop=final_loop+1.D0
        simulation_ufe_loop=final_loop+1.D0
        simulation_ufx_loop=final_loop+1.D0
        simulation_ufv_loop=final_loop+1.D0
        simulation_ufg_loop=final_loop+1.D0
        simulation_vfx_loop=final_loop+1.D0
        call centre
C        call min_PES                     !For any starting point
        call BIMD                         !Stablize velocity
        call BIMD_thermal_init   !reset thermal
        if(wscreen)then
          write(*,"(I5,1x,A25)")
     &myid,"End of Velocity Stablizer"
          write(*,"(I5,1x,A30,1x,F15.0)")
     &myid,"Reset thermal quantities from ",init_loop
        endif
        simulation_rec_loop=org_simulation_rec_loop
        simulation_xyz_loop=org_simulation_xyz_loop
        simulation_ufe_loop=org_simulation_ufe_loop
        simulation_ufx_loop=org_simulation_ufx_loop
        simulation_ufv_loop=org_simulation_ufv_loop
        simulation_ufg_loop=org_simulation_ufg_loop
        simulation_vfx_loop=org_simulation_vfx_loop
        simulation_delta_time=org_BIMD_delta_time
        call BIMD_backup(0)      !Backup the (init_loop-1)'th loop
        call BIMD_history(simulation_restore_history,0,"app","min")
      endif
C===========fill up the empty region in BIMD_restoring_file
C     gap_init_loop=start_loop_int+1
C     gap_final_loop=org_init_loop-1
      if(gap_init_loop.gt.dble(restore_max_loop_int))then
        simulation_rec_loop=gap_rec_loop  !fill up the gap for restoring points
        if(wscreen.and.gap_final_loop.ge.gap_init_loop)
     &write(*,"(I5,1x,A30,1x,F15.0,1x,A4,1x,F15.0)")
     &myid,"Precede BIMD with backup from ",gap_init_loop," to ",
     &gap_final_loop
      else
        if(simulation_rec_loop.le.org_final_loop)then
          if(simulation_reset_thermal.eq.2)then
            call BIMD_thermal_init
            if(init_frame.eq.start_loop_int)then
              start_loop=org_init_loop+1.D0   !reset start_loop for BIMD_thermal
            else
              start_loop=org_init_loop
            endif
            if(wscreen)write(*,"(I5,1x,A30,1x,F15.0)")
     &myid,"Reset thermal quantities from ",start_loop
          endif
          if(org_final_loop.gt.dble(restore_max_loop_int))then
            gap_final_loop=dble(restore_max_loop_int)  !change the gap range from start_loop+1 to restore_max_loop_int
            org_init_loop=gap_final_loop+1.D0          !change the original range from restore_max_loop_int+1 to org_final_loop
            if(wscreen.and.org_final_loop.ge.org_init_loop)
     &write(*,"(I5,1x,A23,1x,F15.0)")
     &myid,"Will start backup from ",org_init_loop
          else
            gap_final_loop=org_final_loop
            org_init_loop=org_final_loop+1.D0
          endif
        endif
        simulation_rec_loop=org_final_loop+1.D0 !only proceed BIMD up to init_loop, record nothing
        if(wscreen.and.gap_final_loop.ge.gap_init_loop)
     &write(*,"(I5,1x,A53,1x,F15.0,1x,A2,1x,F15.0)")
     &myid,"Backup exists! Precede BIMD without backup from loop ",
     &gap_init_loop,"to",gap_final_loop
      endif
      if(gap_final_loop.ge.gap_init_loop)then
        init_loop=gap_init_loop   !start_loop_int+1
        final_loop=gap_final_loop !org_init_loop-1
        simulation_delta_time=gap_delta_time   
        a1 = simulation_delta_time*8.0D0/3.0D0
        a5 = simulation_delta_time*3.0D0/8.0D0
        call BIMD
        if(wscreen)write(*,"(I5,1x,A22)")
     &myid,"Precede BIMD completed"
      endif
C==========Perform the desirable region==========
      if(org_final_loop.ge.org_init_loop)then
        if(wscreen)write(*,"(I5,1x,A23,1x,F15.0,1x,A4,1x,F15.0)")
     &myid,"Start Normal BIMD from ",org_init_loop," to ",org_final_loop
        init_loop=org_init_loop
        final_loop=org_final_loop
        simulation_rec_loop=org_simulation_rec_loop
        simulation_xyz_loop=org_simulation_xyz_loop
        simulation_ufe_loop=org_simulation_ufe_loop
        simulation_ufx_loop=org_simulation_ufx_loop
        simulation_ufv_loop=org_simulation_ufv_loop
        simulation_ufg_loop=org_simulation_ufg_loop
        simulation_vfx_loop=org_simulation_vfx_loop
        simulation_delta_time=org_BIMD_delta_time
        a1 = simulation_delta_time*8.0D0/3.0D0
        a5 = simulation_delta_time*3.0D0/8.0D0
        if(simulation_reset_thermal.eq.2)then
          call BIMD_thermal_init
          start_loop=init_loop   !reset start_loop for BIMD_thermal
          write(*,"(I5,1x,A30,1x,F15.0)")
     &myid,"Reset thermal quantities from ",init_loop
        endif
        if(simulation_rec_loop.le.org_final_loop)then
          simulation_rec_loop=rec_simulation_rec_loop
          simulation_delta_time=rec_BIMD_delta_time
          if(wscreen)then 
            write(*,"(I5,1x,A37,1x,F16.0)")
     &myid,"Set the record interval of backup to ",simulation_rec_loop
            write(*,"(I5,1x,A32,1x,F13.8)")
     &myid,"Set the delta time of backup to ",simulation_delta_time
          endif
        endif
        call BIMD
        if(wscreen)write(*,"(I5,1x,A21)")myid,"Normal BIMD completed"
      else
        if(wscreen)write(*,"(I5,1x,A16)")myid,"Skip Normal BIMD"
      endif
C==========End of BIMD===========================
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
