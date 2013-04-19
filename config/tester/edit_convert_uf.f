*========================================================================
* File Name : edit_usr_output.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Thu 24 Mar 2011 04:33:48 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_convert_uf
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/ensemble.h"
      include "../../include/job.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/PTMC/PTMC.h"
      include "../../include/tester/tester.h"
      integer frame_num,cst_temp,fct_temp,ftf_temp
      character yn*7,read_another*1,sel1*1,sel2*1,
     &file_name_old*80,access_method*3,convert_target_type*3
      logical append_old
      tester_read_file_flag=99
      append_old=.false.
      file_name_old="none"
      do while(tester_read_file_flag.ne.0)
        write(*,*) "0.Quit"
        write(*,*) "1.Manually input file_name"
        read(*,*) tester_read_file_flag
        if(tester_read_file_flag.eq.1)then
          write(*,*) "Please input the file name of the source file"
          read(*,*) source_file_name
304       write(*,*) "What is the format of your source file"
          write(*,*) "1.xyz format without potential"
          write(*,*) "2.pdb format without potential"
          read(*,*) sel1
          if(sel1.eq."1")then
            source_type="xyz"
          else if(sel1.eq."2")then
            source_type="pdb"
          else
            write(*,*) "Error! Please select again!"
            goto 304
          endif
          if(source_type.eq."xyz")then
            write(*,*) "Please select a target format."
            write(*,*) 
     &"Make sure your source file contain the data you want"
            write(*,*) "1.Convert to unformatted x data (*.ufx)"
            write(*,*) "2.Convert to unformatted v data (*.ufv)"
            write(*,*) "3.Convert to unformatted g data (*.ufg)"
            read(*,*) sel1
            if(sel1.eq."1")then
              convert_target_type="ufx"
            else if(sel1.eq."2")then
              convert_target_type="ufv"
            else if(sel1.eq."3")then
              convert_target_type="ufg" 
            endif
          else if(source_type.eq."pdb")then
            convert_target_type="ufx"
          endif
          file_name=source_file_name
     &(:index(source_file_name,".")-1)//"."//convert_target_type
          write(*,*) "Creating a new file=",file_name
          if(file_name.eq.file_name_old)then
            write(*,*) "Append to ",file_name_old
            write(*,*)"(y/n)"
            read(*,*)sel2
            if(sel2.eq."y")then
              write(*,*) "Append new data to ",file_name_old
              access_method="app"
            else
              write(*,*) "Warning! This will erase your previous output"
              access_method="seq"
            endif
          else
            access_method="seq"
          endif
          call write_convert_uf(file_name,access_method,
     &convert_target_type)
          file_name_old=file_name
        else if(tester_read_file_flag.eq.0)then
          stop
        endif
      enddo
      return
      end

      subroutine write_convert_uf(file_name,access_method,
     &convert_target_type)
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/ensemble.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/PTMC/PTMC.h"
      include "../../include/tester/corr.h"
      include "../../include/tester/fourier.h"
      include "../../include/tester/tester.h"
      include "../../include/header.h"
      integer frame_num,total_frame_num,init_frame,final_frame,
     &delta_frame,read_frame_num,pot_row_num
      integer skip_line_start,skip_line_end,read_x_col,
     &total_line_per_frame
      real*8 dummy_real,time_frame
      character access_method*3,convert_target_type*3,dummy(20)*1
      character dummy_char*3
      pot=0.D0
      pot_row_num=0
C      if(access_method.eq."app")then
C        open(21,file=file_name,form="unformatted",access="append")
C      else if(access_method.eq."seq")then
      if(access_method.eq."seq")call clean_file(file_name)
C        open(21,file=file_name,form="unformatted",status="replace")

C      endif
      call check_file_exist(source_file_name)
      call count_file_line(source_file_name)
      if(source_type.eq."xyz")then
        open(22,file=source_file_name,status="old")
        read(22,*)atom_num
        ndim=atom_num*3
        read(22,*) dummy_char,dummy_real
        if(dummy_char.eq."Pot".or.dummy_char.eq."POT".or.
     &dummy_char.eq."pot")pot_row_num=1
        close(22)
        skip_line_start=2
        skip_line_end=0
        if(convert_target_type.eq."ufx")then
          read_x_col=2
        else
          read_x_col=6
        endif
      else if(source_type.eq."pdb")then
        call check_pdb(source_file_name,skip_line_start,
     &skip_line_end)
        read_x_col=6
      endif
      total_line_per_frame=atom_num+skip_line_start+skip_line_end
      total_frame_num=dint(dble(file_line_num)/
     &dble(total_line_per_frame))  
      if(total_frame_num.eq.1)then
        frame_num=1
        init_frame=1
        final_frame=1
        delta_frame=1
        time_frame=0.D0
      else
        write(*,*) "There are ", total_frame_num, " frame(s)"
305     write(*,*) "Please input the initial frame(default=1)"
        read(*,*) init_frame
        if(init_frame.gt.total_frame_num)then
          write(*,*) "Error! Please input again!"
          goto 305
        endif
306     write(*,*) "Please input the final frame(default=",
     &total_frame_num,")"
        read(*,*) final_frame
        if(final_frame.gt.total_frame_num.or.
     &final_frame.lt.init_frame)then
          write(*,*) "Error! Please input again!"
          goto 306
        endif
307     write(*,*) "Please input the frame interval(default=1)"
        read(*,*) delta_frame
        if(delta_frame.gt.total_frame_num)then
          write(*,*) "Error! Please input again!"
          goto 307
        endif
308     write(*,*) 
     &"Please input the time scale for output frames(default=0.001)"
        read(*,*) time_frame        
      endif
      read_frame_num=(final_frame-init_frame+1)/delta_frame
C========Header information=====================
      header_source_type=convert_target_type
      header_par_num=9
      header_par_name(1)="file_x_dim"
      header_par_real(1)=dble(read_frame_num)
      header_par_name(2)="file_y_dim"
      header_par_real(2)=dble(ndim)
      header_par_name(3)="init_loop"
      header_par_real(3)=dble(init_frame)
      header_par_name(4)="final_loop"
      header_par_real(4)=dble(final_frame)
      header_par_name(5)="simulation_delta_time"
      header_par_real(5)=time_frame
      header_par_name(6)="ndim_fac"
      header_par_real(6)=3.D0
      header_par_name(7)="atom_num"
      header_par_real(7)=dble(atom_num)
      header_par_name(8)="ndim"
      header_par_real(8)=dble(ndim)
C===============================================
      open(23,file=source_file_name,status="old")
      do I0=1,init_frame-1
        do I1=1,total_line_per_frame
          read(23,*)
        enddo
      enddo
C     To speed up the algorithm, clone the codes
C==========Block for ufx=================================
      if(convert_target_type.eq."ufx")then
        if(access_method.eq."seq")then
          header_par_name(9)="simulation_ufx_loop"
          header_par_real(9)=dble(delta_frame)
          header_annotation="'x(i=1~3n)'"
          call creat_unformatted_header(file_name)
        endif
        do I0=1,read_frame_num
          do I1=1,skip_line_start
            read(23,*)
          enddo
          if(pot_row_num.eq.1)read(23,*)dummy_char,pot
          do I1=1,atom_num
            I2=I1*3
            read(23,*)
     &(dummy(I3),I3=1,read_x_col-1),x(I2-2),x(I2-1),x(I2)
          enddo
          call write_x_unformatted(file_name,I0,read_frame_num)
          do I1=1,skip_line_end
            read(23,*)
          enddo
          do I1=1,delta_frame-1
            do I2=1,total_line_per_frame
              read(23,*)
            enddo
          enddo
        enddo
      endif
C==========End of ufx=======================================
C==========Block for ufv====================================
      if(convert_target_type.eq."ufv")then
        if(access_method.eq."seq")then
          header_par_name(9)="simulation_ufv_loop"
          header_par_real(9)=dble(delta_frame)
          header_annotation="'v(i=1~3n)'"
          call creat_unformatted_header(file_name)
        endif
        do I0=1,read_frame_num
          do I1=1,skip_line_start-1
            read(23,*)
          enddo
          if(pot_row_num.eq.1)read(22,*)dummy_char,pot
          do I1=1,atom_num
            I2=I1*3
            read(23,*)
     &(dummy(I3),I3=1,read_x_col-1),v(I2-2),v(I2-1),v(I2)
          enddo
          call write_v_unformatted(file_name,I0,read_frame_num)
          do I1=1,skip_line_end
            read(23,*)
          enddo
          do I1=1,delta_frame-1
            do I2=1,total_line_per_frame
              read(23,*)
            enddo
          enddo
        enddo
      endif
C==========End of ufv=======================================
C==========Block for ufg====================================
      if(convert_target_type.eq."ufg")then
        if(access_method.eq."seq")then
          header_par_name(9)="simulation_ufg_loop"
          header_par_real(9)=dble(delta_frame)
          header_annotation="'g(i=1~3n)'"
          call creat_unformatted_header(file_name)
        endif
        do I0=1,read_frame_num
          do I1=1,skip_line_start-1
            read(23,*)
          enddo
          if(pot_row_num.eq.1)read(23,*)dummy_char,pot
          do I1=1,atom_num
            I2=I1*3
            read(23,*)
     &(dummy(I3),I3=1,read_x_col-1),grad(I2-2),grad(I2-1),grad(I2)
          enddo
          call write_g_unformatted(file_name,I0,read_frame_num)
          do I1=1,skip_line_end
            read(23,*)
          enddo
          do I1=1,delta_frame-1
            do I2=1,total_line_per_frame
              read(23,*)
            enddo
          enddo
        enddo
      endif
C==========End of ufg==========================================
      close(23)
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
