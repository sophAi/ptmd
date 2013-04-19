*========================================================================
* File Name : BIMD.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Thu 12 Jan 2012 02:09:12 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine BIMD
C================Input parameters======================================
C global_energy,initial x,v,simulation_delta_time,delta_temp,init_loop,final_loop,equ_loop,mass_a,mass_b,atom_num,atom_num_a,ndim
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/BIMD/BIMD_restore.h"
      include "../../include/pes.h"
      include "../../include/tools/usr.h"
      include "../../include/simulation.h"
      integer min_PES_org
      integer rec_loop_int,xyz_loop_int,ufe_loop_int,ufx_loop_int,
     &ufv_loop_int,ufg_loop_int,vfx_loop_int
      integer rec_num_int,xyz_num_int,ufe_num_int,ufx_num_int,
     &ufv_num_int,ufg_num_int,vfx_num_int
      real*8 xc,xcp
      real*8 fm(q_max),ym(q_max)
      real*8 temper_step
C      call centre
C      call min_PES
      call BIMD_init_file
      rec_loop_int=dint(simulation_rec_loop)
      rec_num_int=dint(simulation_rec_num)
      xyz_loop_int=dint(simulation_xyz_loop)
      xyz_num_int=dint(simulation_xyz_num)
      ufe_loop_int=dint(simulation_ufe_loop)
      ufe_num_int=dint(simulation_ufe_num)
      ufx_loop_int=dint(simulation_ufx_loop)
      ufx_num_int=dint(simulation_ufx_num)
      ufv_loop_int=dint(simulation_ufv_loop)
      ufv_num_int=dint(simulation_ufv_num)
      ufg_loop_int=dint(simulation_ufg_loop)
      ufg_num_int=dint(simulation_ufg_num)
      vfx_loop_int=dint(simulation_vfx_loop)
      vfx_num_int=dint(simulation_vfx_num)
      init_loop_int=dint(init_loop)
      final_loop_int=dint(final_loop)
C=============Start BIMD================================================
      do I1=init_loop_int,final_loop_int
C-----------------------------------------------------------------------C
C THE FOLLOWING IS THE HAMMING SCHEME TO INTEGRATE A SET OF 1ST ORDER   C
C  DIFFERENTIAL EQUATIONS.                                              C
C-----------------------------------------------------------------------C
        do J3=1,q_dim
          q(J3)=yk3(J3)+a1*(fk0(J3)-0.5D0*fk1(J3)+fk2(J3))
          ym(J3)=q(J3)+a2*ycp(J3)
        enddo
        CALL TDIFF(ym,fm)
        do J1=1,q_dim
          xc=a3*(a4*yk0(J1)-yk2(J1))+a5*(fm(J1)+2.0D0*fk0(J1)-fk1(J1))
          xcp=xc-q(J1)
          xc=xc+a6*xcp
          q(J1)=xc                        !  fifth point as first point
          ycp(J1)=xcp                     ! Necessary elements for the iteration: yk0,yk2,fk0,fk1,
          fk2(J1)=fk1(J1)
          fk1(J1)=fk0(J1)
          yk3(J1)=yk2(J1)
          yk2(J1)=yk1(J1)
          yk1(J1)=yk0(J1)
          yk0(J1)=xc
        enddo
C=============send back x and v===========================
        do J1=1,top_frz_num
          J3=top_frz_id(J1)
          J2=J3*3
          x(J2-2)=q(J1)
          x(J2-1)=q(J1+N1)
          x(J2)=q(J1+N1*2)
          v(J2-2)=q(J1+N1*3)/mass(J3)
          v(J2-1)=q(J1+N1*4)/mass(J3)
          v(J2)=q(J1+N1*5)/mass(J3)
C          write(*,*) J1,x(J2-2),x(J2-1),x(J2)
        enddo
C        pause
C==============End of sending back x and v========================
        CALL TDIFF(q,fk0)
C====================End of Differential Equation,generate x and v =======
        do J1=1,atom_num
          do J3=1,atom_num
            if(J3.ne.J1) then   
              BIMD_ave_dist(J1,J3)=BIMD_ave_dist(J1,J3)+dist(J1,J3)
              BIMD_ave_dist2(J1,J3)=BIMD_ave_dist2(J1,J3)+dist(J1,J3)**2
            endif
          enddo
        enddo 
        kinetic=0.D0
        do J1=1,top_frz_num
          J3=top_frz_id(J1)
          kinetic=kinetic+(q(J1+N1*3)**2+
     &q(J1+N1*4)**2+q(J1+N1*5)**2)/(2D0*mass(J3))
        enddo
C        temper_step=0.D0
C        do J1=1,atom_num
C          J2=J1*3
C          temper_step=temper_step+
C     &mass(J1)*(v(J2-2)**2+v(J2-1)**2+v(J2)**2)
C        enddo
C        temper_step=temper_step/(dble(atom_num)*kb*3D0)  !Check
        temper_step=kinetic/(1.5D0*dble(N1)*kb)   !kinetic_energy=3/2*N*Kb*T

        BIMD_ave_energy=BIMD_ave_energy+pot+kinetic
C    &-mean_global_pot          !in unit of eV
        BIMD_ave_energy_square=BIMD_ave_energy_square+
     &(pot+kinetic)**2
        BIMD_ave_pot=BIMD_ave_pot+pot
        BIMD_ave_kinetic=BIMD_ave_kinetic+kinetic
        BIMD_ave_temper=BIMD_ave_temper+temper_step
C===========Start of Restoring Points or recording xyz==============================
        if(mod(I1,rec_loop_int).eq.0)then
          call BIMD_history(simulation_restore_history,I1,"app","min")
          call BIMD_backup(I1)
        endif
        if(mod(I1,xyz_loop_int).eq.0.and.I1.ge.simulation_init_loop)
     &call write_xyz_file(simulation_xyz_file,"app",I1,
     &xyz_num_int,4)
C     &call BIMD_history(simulation_restore_history,I1,"app","org")
        if(mod(I1,ufe_loop_int).eq.0.and.I1.ge.simulation_init_loop)
     &call write_e_unformatted(simulation_e_unformatted_file)
        if(mod(I1,ufx_loop_int).eq.0.and.I1.ge.simulation_init_loop)
     &call write_x_unformatted(simulation_x_unformatted_file)
        if(mod(I1,ufv_loop_int).eq.0.and.I1.ge.simulation_init_loop)
     &call write_v_unformatted(simulation_v_unformatted_file)
        if(mod(I1,ufg_loop_int).eq.0.and.I1.ge.simulation_init_loop)
     &call write_g_unformatted(simulation_g_unformatted_file)
        if(mod(I1,vfx_loop_int).eq.0.and.I1.ge.simulation_init_loop)
     &call write_v_fixed_unformatted(simulation_vfx_unformatted_file)
C     &call write_xyz_file
C     &("output/BIMD/xyz/BIMD_pes_gupta_pure_Ag0014_0100.xyz","app",I1,
C     &1000000,1)
C===========End of Restoring Points or recording xyz================================
      enddo  
      return
      end


      subroutine BIMD_init_file
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/BIMD/BIMD_restore.h"
      include "../../include/pes.h"
      include "../../include/simulation.h"
      integer total_record_xyz_num,x_dim,y_dim
C      write(*,*) simulation_init_loop,init_loop,final_loop
C=================formatted xyz=============================
      if(simulation_xyz_loop.le.final_loop.and.
     &simulation_init_loop.ge.dint(init_loop).and.
     &simulation_init_loop.le.dint(final_loop))then
        simulation_xyz_num=dble(dint(simulation_final_loop-
     &simulation_init_loop+1.D0)/dint(simulation_xyz_loop))
        call clean_file(simulation_xyz_file)
        if(wscreen)then
          write(*,"(I5,1x,A42)")
     &myid,"Erase the corresponding formatted xyz file"
          write(*,"(I5,1x,A23,1x,A80)")
     &myid,"Start to record xyz to ",simulation_xyz_file
        endif
      endif
C===============unformatted_ufx============================
      if(simulation_ufx_loop.le.final_loop.and.
     &simulation_init_loop.ge.dint(init_loop).and.
     &simulation_init_loop.le.dint(final_loop))then
        x_dim=dint(simulation_final_loop-
     &simulation_init_loop+1.D0)/dint(simulation_ufx_loop)
        simulation_ufx_num=dble(x_dim)
        y_dim=ndim
        call BIMD_unformatted_header("ufx",x_dim,y_dim)
        call creat_unformatted_header(simulation_x_unformatted_file)
        if(wscreen)then
          write(*,"(I5,1x,A42)")
     &myid,"Erase the corresponding unformatted x file"
          write(*,"(I5,1x,A23,1x,A80)")
     &myid,"Start to record ufx to ",simulation_x_unformatted_file
        endif
      endif
C=================unformatted_ufv==========================
      if(simulation_ufv_loop.le.final_loop.and.
     &simulation_init_loop.ge.dint(init_loop).and.
     &simulation_init_loop.le.dint(final_loop))then
        x_dim=dint(simulation_final_loop-
     &simulation_init_loop+1.D0)/dint(simulation_ufv_loop)
        simulation_ufv_num=dble(x_dim)
        y_dim=ndim
        call BIMD_unformatted_header("ufv",x_dim,y_dim)
        call creat_unformatted_header(simulation_v_unformatted_file)
        if(wscreen)then
          write(*,"(I5,1x,A42)")
     &myid,"Erase the corresponding unformatted v file"
          write(*,"(I5,1x,A23,1x,A80)")
     &myid,"Start to record ufv to ",simulation_v_unformatted_file
        endif
      endif
C=================unformatted_vfx==========================
      if(simulation_vfx_loop.le.final_loop.and.
     &simulation_init_loop.ge.dint(init_loop).and.
     &simulation_init_loop.le.dint(final_loop))then
        x_dim=dint(simulation_final_loop-
     &simulation_init_loop+1.D0)/dint(simulation_vfx_loop)
        simulation_vfx_num=dble(x_dim)
        y_dim=ndim   !include com_v and ang_v
        call BIMD_unformatted_header("vfx",x_dim,y_dim)
        call creat_unformatted_header(simulation_vfx_unformatted_file)
        if(wscreen)then
          write(*,"(I5,1x,A48)")
     &myid,"Erase the corresponding unformatted fixed v file"
          write(*,"(I5,1x,A23,1x,A80)")
     &myid,"Start to record vfx to ",simulation_vfx_unformatted_file
        endif
      endif
C================unformatted_ufg===============================
      if(simulation_ufg_loop.le.final_loop.and.
     &simulation_init_loop.ge.dint(init_loop).and.
     &simulation_init_loop.le.dint(final_loop))then
        x_dim=dint(simulation_final_loop-
     &simulation_init_loop+1.D0)/dint(simulation_ufg_loop)
        y_dim=ndim
        simulation_ufg_num=dble(x_dim)
        call BIMD_unformatted_header("ufg",x_dim,y_dim)
        call creat_unformatted_header(simulation_g_unformatted_file)
        if(wscreen)then
          write(*,"(I5,1x,A42)")
     &myid,"Erase the corresponding unformatted g file"
          write(*,"(I5,1x,A23,1x,A80)")
     &myid,"Start to record ufg to ",simulation_g_unformatted_file
         endif
      endif
C==================unformatted_ufe==========================
      if(simulation_ufe_loop.le.final_loop.and.
     &simulation_init_loop.ge.dint(init_loop).and.
     &simulation_init_loop.le.dint(final_loop))then
        x_dim=dint(simulation_final_loop-
     &simulation_init_loop+1.D0)/dint(simulation_ufe_loop)
        simulation_ufe_num=dble(x_dim)
        y_dim=atom_num+3
        call BIMD_unformatted_header("ufe",x_dim,y_dim)
        call creat_unformatted_header(simulation_e_unformatted_file)
        if(wscreen)then
          write(*,"(I5,1x,A44)")
     &myid,"Erase the corresponding unformatted ufe file"
          write(*,"(I5,1x,A23,1x,A80)") 
     &myid,"Start to record ufe to ",simulation_e_unformatted_file
        endif
      endif
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
