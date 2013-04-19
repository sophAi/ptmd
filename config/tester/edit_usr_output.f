*========================================================================
* File Name : edit_usr_output.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Tue 19 Jul 2011 10:51:47 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_usr_output
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/ensemble.h"
      include "../../include/job.h"
      include "../../include/tools/moment.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/tester/tester.h"
      include "../../include/tools/usr.h"
      include "../../include/tester/score.h"
      include "../../include/header.h"
      integer frame_num,cst_temp,fct_temp,ftf_temp,top_off(atom_num_max)
      integer bond_smaller_one_number
      character yn*7,read_another*1,target_usr_file_name*80,sel1*1
     &,target_top_file_name*80,usr_file_name*80,top_file_name*80
      real*8 temp_matrix(1000,100),read_rzero
      tester_read_file_flag=99
      total_moment_x_num=4  ! The length per each moment. For usr, it is 4. For simple moment, it is 1
      total_moment_z_num=16 ! Total number of moment descriptors
      do I0=1,atom_num_max
        top_off(I0)=1
      enddo
C      if(pes_type.eq."none ")then
C        write(*,*) "No PES, USR requare mass information"
C        write(*,*) "Please select a PES first" 
C        call pes_cycle
C      endif
      do while(tester_read_file_flag.ne.0)
        write(*,*) "0.Quit"
        write(*,*) "1.Manually input file_name"
        read(*,*) tester_read_file_flag
        if(tester_read_file_flag.eq.1)then
          write(*,*) "Please input the file name of the source file"
          read(*,*) source_file_name
          target_usr_file_name=output_path(:index(output_path," ")-1)//
     &bimd_path(:index(bimd_path," ")-1)//
     &mom_path(:index(usr_path," ")-1)//
     &"score_target.mom"
          usr_file_name=source_file_name
     &(:index(source_file_name,".")-1)//".mom"
304       write(*,*) "Would you like to..."
          write(*,*) "1.Generate usr data to ",target_usr_file_name
          write(*,*) "2.Generate usr data to ",usr_file_name 
          read(*,*)sel1
          if(sel1.eq."1")then
            usr_file_name=
     &target_usr_file_name(:index(target_usr_file_name," ")-1)
          else if(sel1.eq."2")then

          else
            write(*,*) "ERROR!! Please select again!"
            goto 304
          endif
          write(*,*) "Creating new files=",usr_file_name
          read_another="y"
          frame_num=0
          do while(read_another.eq."y")
            call check_file_exist(source_file_name)
            if(.not.file_exist)then
              write(*,*) "Can't find ",source_file_name
              source_file_name=
     &input_path(:index(input_path," ")-1)//source_file_name
     &(:index(source_file_name," ")-1)
              write(*,*) "Try find ",source_file_name
              call check_file_exist(source_file_name)
              if(file_exist)write(*,*)"Found it!!"
            endif
            call check_file_name(source_file_name,file_main,file_ext)
            if(file_ext.eq."xyz")then
              call read_xyz_file(source_file_name)
              call check_config_name
            elseif(file_ext.eq."pdb")then
              call read_pdb_file(source_file_name)
              call check_config_name
            endif
            call check_reduced_unit(bond_smaller_one_number)
            if(bond_smaller_one_number.gt.0)then
              write(*,*) 
     &"Please input the multiply factor to coordinates"
              write(*,*) "(1 would be the same)"
              read(*,*) read_rzero
              if(read_rzero.ne.1.D0)call fix_reduced_unit(read_rzero)
            endif
            call periodic_mass
            write(*,*) "Please check your topology information"
C            call initial_pes 
            call top_usr_init
            call usr_init
            call usr   !only for showing ctd.fct.ftf
            write(*,*) "Closest to ctd(cst)  ",cst_id
            write(*,*) "Farthest to ctd(fct) ",fct_id
            write(*,*) "Farthest to fct(ftf) ",ftf_id
            call edit_top_usr
            call usr
            frame_num=frame_num+1
C            write(21,"(I13,1x,F13.8,1x,I4,1x,I4,1x,(3(I3,1x)),
C     &(20(F20.16,1x)))")
C     &frame_num,pot,1,atom_num,cst_id,fct_id,ftf_id,
C     &(usr_moment(J2),J2=1,total_moment)  !1 means only one topological data
            I1=0
            do I0=1,total_moment_z_num
              I1=I1+1
              temp_matrix(frame_num,I0)=mom(I1)
            enddo
            temp_matrix(frame_num,total_moment_z_num+1)=
     &dble(top_usr_num)
            if(top_use_usr.eq.1)then 
              I1=0
              do I0=total_moment_z_num+1+1,total_moment_z_num+1+atom_num
                I1=I1+1
                temp_matrix(frame_num,I0)=top_usr_io(I1)
              enddo
            else
              I1=0
              do I0=total_moment_z_num+1+1,total_moment_z_num+1+atom_num
                I1=I1+1
                temp_matrix(frame_num,I0)=top_off(I1)
              enddo
            endif
            temp_matrix(frame_num,total_moment_z_num+atom_num+2)=pot
            temp_matrix(frame_num,total_moment_z_num+atom_num+3)=
     &dble(cst_id)
            temp_matrix(frame_num,total_moment_z_num+atom_num+4)=
     &dble(fct_id)
            temp_matrix(frame_num,total_moment_z_num+atom_num+5)=
     &dble(ftf_id)
            write(*,*) "Read another frame?(y/n)"
            read(*,*) read_another
          enddo
          header_source_type="mom"
          header_par_num=7
          header_par_name(1)="file_x_dim"
          header_par_real(1)=1.D0
          header_par_name(2)="file_y_dim"
          header_par_real(2)=dble(frame_num)
          header_par_name(3)="file_z_dim"
          header_par_real(3)=dble(2+total_moment_z_num+atom_num+5)
          header_par_name(4)="atom_num"
          header_par_real(4)=dble(atom_num)
          header_par_name(5)="total_moment_y_num"
          header_par_real(5)=dble(frame_num)
          header_par_name(6)="total_moment_z_num"
          header_par_real(6)=dble(total_moment_z_num)
          header_par_name(7)="total_moment_x_num"
          header_par_real(7)=dble(total_moment_x_num)
          header_annotation=
     &"'1 target_id mom(1~n) top_usr_num top_usr(1~n) pot cst fct,ftf'"
          call creat_formatted_header(usr_file_name)
          open(21,file=usr_file_name,access="append")
          I2=total_moment_z_num+atom_num+5
          write(21,"(A18)")"# name: mom_target"
          write(21,"(A14)")"# type: matrix"
          write(21,"(A8,1x,I13)")"# rows: ",frame_num
          write(21,"(A11,1x,I5)")"# columns: ",I2+2
          do I0=1,frame_num
            write(21,"(I1,1x,I13,3000(1x,F15.8))")
     &1,I0,(temp_matrix(I0,I1),I1=1,I2)
          enddo
          close(21)
        else if(tester_read_file_flag.eq.0)then
          stop
        endif
      enddo
      return
      end

      subroutine write_usr_output
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/ensemble.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/tester/tester.h"
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
