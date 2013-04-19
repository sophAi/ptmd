*========================================================================
* File Name : BIMD_restore.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Wed 04 May 2011 10:56:11 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine BIMD_backup(frame_num)
      implicit none      !Backup point will only be stored above the final loop
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/simulation.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/BIMD/BIMD_restore.h"
      include "../../include/cn/cn.h"
      integer frame_num
      real*8 org_ufe
C      if(frame_num.le.dint(last_loop))return
      file_name=
     &simulation_restore_file(:index(simulation_restore_file," ")-1)
      open(21,file=file_name,form="unformatted",access="append")
      write(21)
     &time_label,frame_num,simulation_rec_loop
C     &,init_loop,final_loop
     &,simulation_delta_time,
     &temp,delta_temp,q_dim,atom_num,atom_name_a,atom_num_a,
     &atom_name_b,atom_num_b
C      write(*,*) "simulation_rec_loop=",simulation_rec_loop
      write(21)
     &BIMD_ave_energy,BIMD_ave_energy_square,BIMD_ave_pot,
     &BIMD_ave_kinetic,BIMD_ave_temper,
     &cn_name
      write(21) (ycp(I0),I0=1,q_dim)
      write(21) (fk0(I0),I0=1,q_dim)
      write(21) (fk1(I0),I0=1,q_dim)
      write(21) (fk2(I0),I0=1,q_dim)
      write(21) (yk0(I0),I0=1,q_dim)
      write(21) (yk1(I0),I0=1,q_dim)
      write(21) (yk2(I0),I0=1,q_dim)
      write(21) (yk3(I0),I0=1,q_dim)
      do I0=1,atom_num
        write(21)(BIMD_ave_dist(I0,I1),I1=1,atom_num)
      enddo
      do I0=1,atom_num
        write(21)(BIMD_ave_dist2(I0,I1),I1=1,atom_num)
      enddo
      if(wscreen)then
        if(cn_prev_name.ne.cn_name)then
         write(*,"(I5,1x,A5,1x,I13,1x,A40,1x,A80)") 
     &myid,"Loop=",frame_num,". Jump to a new local minimum. Backup to",
     &simulation_restore_file
        else
          write(*,"(I5,1x,A5,1x,I13,1x,A10,1x,A80)") 
     &myid,"Loop=",frame_num,",Backup to",simulation_restore_file
        endif
      endif
99    close(21)
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
