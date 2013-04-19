*========================================================================
* File Name : BIMD_restore_anl.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年10月22日 (週五) 14時57分51秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine BIMD_backup(frame_num)
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/BIMD/BIMD_restore.h"
      include "../../include/tools/cn.h"
      include "../../include/tools/anl.h"
      integer frame_num
      real*8 org_pot
C      org_pot=pot
C      call lbfgs_init
C      call lbfgs_min
      call pes
      write(*,*) "pot=",pot
      pause
      call cn_init
      call cn
      call cn2name
      goto 13
      if(frame_num.le.dint(last_loop))return
      open(21,file=BIMD_restore_file,form="unformatted",access="append")
      write(21)
     &time_label,frame_num,BIMD_rec_interval,init_loop,
     &final_loop,BIMD_delta_time,
     &temp,delta_temp,q_dim,atom_num,atom_name_a,atom_num_a,
     &atom_name_b,atom_num_b
      write(21)
     &ave_energy,ave_energy_square,ave_pot,ave_kinetic,ave_temper,
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
        write(21)(ave_dist(I0,I1),I1=1,atom_num)
      enddo
      do I0=1,atom_num
        write(21)(ave_dist2(I0,I1),I1=1,atom_num)
      enddo
      if(wscreen)then
        if(cn_prev_name.ne._name)then
         write(*,"I5,1x,A5,1x,I13,1x,A40,1x,A60") 
     &myid,"Step=",frame_num,". Jump to a new local minimum. Backup to",
     &BIMD_restore_file
        else
          write(*,"I5,1x,A5,1x,I13,1x,A10,1x,A60") 
     &myid,"Step=",frame_num,",Backup to",BIMD_restore_file
        endif
      endif
99    close(21)
13    open(22,file=BIMD_restore_history,access="append")
C      write(22,*)"# frame_num lmin pot cn_name_of_lmin"
C      write(22,"I13,1x,F17.8,1x,F17.8,1x,A200")frame_num,pot,org_pot,
C     &cn_name
      write(22,"I13,1x,F17.8,1x,A200")frame_num,pot,
     &cn_name
      do I0=1,atom_num
        write(22,"I13,1x,I5,1x,A10,1x,100(I3,1x)")
     &I0,anl_num(I0),"neighbors:",(anl_neighbor(I0,I1),I1=1,anl_num(I0))
      enddo
      close(22)
      cn_prev_name=cn_name
      return
      end

      subroutine BIMD_restore
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/BIMD/BIMD_restore.h"
      include "../../include/tools/tools.h"
      integer start_frame,loop_line_num
      real*8 init_loop_org,final_loop_org,rec_interval_org,
     &init_loop_gap
      real*8 init_loop_bac,final_loop_bac,rec_interval_bac
      logical record_xyz_bac
      loop_line_num=10+2*atom_num
      last_loop=0.D0
      init_loop_bac=init_loop
      init_loop_gap=init_loop
      final_loop_bac=final_loop
      rec_interval_bac=BIMD_rec_interval
      record_xyz_bac=BIMD_record_xyz
      inquire(file=BIMD_restore_file,exist=file_exist)
      if(file_exist)then
        if(init_loop_bac.eq.1.D0)then
          if(wscreen)write(*,"I5,1x,A19")
     &myid,"Starting a new BIMD"
          call clean_file(BIMD_restore_file)
          call clean_file(BIMD_restore_history)
          return
        else
          open(20,file=BIMD_restore_file,
     &form="unformatted",status="old")
          read(20) time_label,start_frame,rec_interval_org  !read_interval_org and initial start_frame
          close(20)
          call count_file_line_unformatted(BIMD_restore_file)
          last_loop=dble(file_line_num/loop_line_num)*rec_interval_org
          if(wscreen)write(*,"I5,1x,A38,1x,F13.1")
     &myid,"Backup file exist.Previous final loop=",last_loop
          if(init_loop_bac.ge.dble(start_frame))then
            if(init_loop_gap.gt.last_loop)init_loop=last_loop
            open(21,file=BIMD_restore_file,
     &form="unformatted",status="old")
            do I0=2,dint(init_loop/rec_interval_org)
              do I1=1,loop_line_num
                read(21)
              enddo
            enddo
            read(21)
     &time_label,start_frame,rec_interval_org,init_loop_org,
     &final_loop_org,BIMD_delta_time,temp,delta_temp,q_dim,
     7atom_num,atom_name_a,atom_num_a,atom_name_b,atom_num_b
            read(21)
     &ave_energy,ave_energy_square,ave_pot,ave_kinetic,ave_temper
     &,read_CN_name
            read(21) (ycp(I0),I0=1,q_dim)
            read(21) (fk0(I0),I0=1,q_dim)
            read(21) (fk1(I0),I0=1,q_dim)
            read(21) (fk2(I0),I0=1,q_dim)
            read(21) (yk0(I0),I0=1,q_dim)
            read(21) (yk1(I0),I0=1,q_dim)
            read(21) (yk2(I0),I0=1,q_dim)
            read(21) (yk3(I0),I0=1,q_dim)
            do I0=1,atom_num
              read(21)(ave_dist(I0,I1),I1=1,atom_num)
            enddo
            do I0=1,atom_num
              read(21)(ave_dist2(I0,I1),I1=1,atom_num)
            enddo
            close(21)
            if(mod(init_loop_bac,rec_interval_bac).eq.0)then
              init_loop_bac=init_loop_bac+1.D0
            endif
            if(wscreen)write(*,"I5,1x,A26,1x,I13")
     &myid,"Reading restoring point at",start_frame
          else
            start_frame=0
          endif
        endif
        init_loop=dble(start_frame)+1.D0
        final_loop=init_loop_bac-1.D0
        if(init_loop_gap.gt.last_loop)then
          BIMD_rec_interval=rec_interval_bac
        else
          BIMD_rec_interval=final_loop-init_loop+100.D0
        endif
        BIMD_record_xyz=.false.
      else 
        if(init_loop_bac.eq.1.D0)then
          if(wscreen)write(*,"I5,1x,A19")
     &myid,"Starting a new BIMD"
          return
        else
          start_frame=0
          init_loop=dble(start_frame)+1.D0
          final_loop=init_loop_bac-1.D0
          BIMD_rec_interval=rec_interval_bac
          BIMD_record_xyz=.false.
        endif
      endif 
      if(final_loop.ge.init_loop)then
        if(wscreen)write(*,"I5,1x,A25,1x,F13.1,1x,A2,1x,F13.1") 
     &myid,"Preceding BIMD from loop ",init_loop,"to",
     &final_loop
        call BIMD
        if(wscreen)write(*,"I5,1x,A24")
     &myid,"Preceding BIMD completed"
      endif
      if(BIMD_reset_thermal)then
        if(wscreen)write(*,"I5,1x,A24")myid,"reset thermal quantities"
        call BIMD_init_thermal
        start_loop=init_loop_bac
      else
        start_loop=1.D0
      endif
      init_loop=init_loop_bac
      final_loop=final_loop_bac
      BIMD_rec_interval=rec_interval_bac 
      BIMD_record_xyz=record_xyz_bac
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
