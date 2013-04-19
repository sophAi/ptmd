*========================================================================
* File Name : BIMD_restore.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Wed 04 May 2011 10:57:18 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine BIMD_restore(frame_num,start_loop_int,
     &restore_max_loop_int)  
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/simulation.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/BIMD/BIMD_restore.h"
      include "../../include/cn/cn.h"
      integer frame_num,restore_max_loop_int,total_line_per_frame,
     &total_par_per_frame,read_frame_num,start_loop_int
      character read_test1*20,read_test2*20
      total_line_per_frame=10+2*atom_num !total line per restored frame
      total_par_per_frame=14+6+8*q_dim+2*atom_num
      file_name=
     &simulation_restore_file(:index(simulation_restore_file," ")-1)
      inquire(file=simulation_restore_file,exist=file_exist)
      if(.not.file_exist)then
        start_loop_int=0  !record restoring point
        restore_max_loop_int=0
        call BIMD_thermal_init
        if(wscreen)write(*,"(I5,1x,A46)")
     &myid,"No backup file. Will fill up the backup region"
        return
      endif
      open(22,file=file_name,form="unformatted",status="old")
602   read(22,end=603) time_label,read_frame_num,simulation_rec_loop
C      write(*,*) time_label,read_frame_num,simulation_rec_loop,frame_num
      if((frame_num-read_frame_num).le.simulation_rec_loop)then
        backspace(22)
        goto 604      !locate the frame
      endif
      do I1=1,total_line_per_frame-1
        read(22)
      enddo
      goto 602
603   do I1=1,total_line_per_frame+1
        backspace(22)
      enddo
604   read(22) time_label,start_loop_int,simulation_rec_loop
C     &,init_loop,final_loop
     &,simulation_delta_time,
     &temp,delta_temp,q_dim,atom_num,atom_name_a,atom_num_a,
     &atom_name_b,atom_num_b
      read(22)
     &BIMD_ave_energy,BIMD_ave_energy_square,BIMD_ave_pot,
     &BIMD_ave_kinetic,BIMD_ave_temper,
     &cn_name
      read(22) (ycp(I0),I0=1,q_dim)
C      write(*,*) (ycp(I0),I0=1,q_dim)
C      pause
      read(22) (fk0(I0),I0=1,q_dim)
      read(22) (fk1(I0),I0=1,q_dim)
      read(22) (fk2(I0),I0=1,q_dim)
      read(22) (yk0(I0),I0=1,q_dim)
      read(22) (yk1(I0),I0=1,q_dim)
      read(22) (yk2(I0),I0=1,q_dim)
      read(22) (yk3(I0),I0=1,q_dim)
      do I0=1,atom_num
        read(22)(BIMD_ave_dist(I0,I1),I1=1,atom_num)
      enddo
      do I0=1,atom_num
        read(22)(BIMD_ave_dist2(I0,I1),I1=1,atom_num)
      enddo
605   read(22,end=606)
      goto 605 
606   do I1=1,total_line_per_frame+1
        backspace(22)
      enddo
      read(22) time_label,restore_max_loop_int
      if(wscreen)then
        write(*,"(I5,1x,A22,1x,A80)")
     &myid,"Found the Backup file=",file_name
        write(*,"(I5,1x,A36,1x,I15)")
     &myid,"Total loops of the restoring points=",restore_max_loop_int
        write(*,"(I5,1x,A29,1x,I15)")
     &myid,"Locate the restoring point at",start_loop_int
C        write(*,*) "simulation rec backup interval=",simulation_rec_loop
C        simulation_rec_loop=1.D6
C        simulation_restore_file="test.rec"
C        call BIMD_backup(0)
C        pause
      endif
99    close(22)
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
