*========================================================================
* File Name : check_file.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Mon 21 Mar 2011 03:34:00 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine check_file
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      integer file_line
      character*30 dummy1,dummy2,read_file_path*60
      file_name="path"
      inquire(file=file_name,exist=file_exist)
      if(file_exist)then
        open(20,file="path",status="unknown")
        read(20,*)file_path
        close(20)
      else
        file_path="dat/"
      endif
      if(wscreen)then
        write(*,*) "Checking all necessary files and paths."
        write(*,*) "The default path is ",file_path
      endif
      file_name=file_path(:index(file_path," ")-1)//"file_path"
      inquire(file=file_name,exist=file_exist)
      if(.not.file_exist)then
        write(*,*) "No files in ",file_path
        write(*,*)"Please input the location of your file path"
        write(*,*)"Please indicate the relative directory,ex:dat"
        write(*,*)"The name of this dir is limited to 3 characters"
        read(*,*) read_file_path
        write(*,*)" and your file_path filename,ex:file_path"
        read(*,*)file_name
        file_path=read_file_path(:index(read_file_path, " ")-1)//"/"
        file_name=file_path(:index(file_path," ")-1)//file_name
        call check_file_exist(file_name)
      endif
      call count_file_line(file_name)
      open(22,file=file_name,status="old")
      do I0=1,file_line_num
        read(22,*)dummy1,dummy2
        call check_file_path(dummy1,dummy2)
      enddo
      close(22)
C      open(23,file="path",status="replace")
C      write(23,*)'"',file_path(:index(file_path," ")-1),'"'
C      close(23)
      if(wscreen)then
        write(*,*) "End of checking. No error occurs.."
      endif
      return
      end

      subroutine check_file_path(dummy1,dummy2)
C===========Define your files and paths here================
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      character*30 dummy1,dummy2
C===========Check path======================================
      if(dummy1.eq."file_path")then
        file_path=dummy2
      endif
      if(dummy1.eq."input_path")then
        input_path=dummy2
      endif
      if(dummy1.eq."output_path")then
        output_path=dummy2
      endif
      if(dummy1.eq."status_path")then
        status_path=dummy2
        program_status_file=
     &status_path(:index(status_path," ")-1)//"program_status"
        job_status_file=
     &status_path(:index(status_path," ")-1)//"job_status"
      endif
      if(dummy1.eq."representation_path")then
        representation_path=dummy2
      endif
      if(dummy1.eq."CN_network_path")then
        CN_network_path=dummy2
      endif
      if(dummy1.eq."action_path")then
        action_path=dummy2
      endif
      if(dummy1.eq."bimd_path")then
        bimd_path=dummy2
      endif
      if(dummy1.eq."ptmc_path")then
        ptmc_path=dummy2
      endif
      if(dummy1.eq."dat_path")then
        dat_path=dummy2
      endif 
      if(dummy1.eq."rec_path")then
        rec_path=dummy2
      endif
      if(dummy1.eq."xyz_path")then
        xyz_path=dummy2
      endif
      if(dummy1.eq."usr_path")then
        usr_path=dummy2
      endif
      if(dummy1.eq."cnl_path")then
        cnl_path=dummy2
      endif  
      if(dummy1.eq."anl_path")then
        anl_path=dummy2
      endif
      if(dummy1.eq."dtf_path")then
        dtf_path=dummy2
      endif
      if(dummy1.eq."cor_path")then
        cor_path=dummy2
      endif
      if(dummy1.eq."his_path")then
        his_path=dummy2
      endif
      if(dummy1.eq."fft_path")then
        fft_path=dummy2
      endif
      if(dummy1.eq."scr_path")then
        scr_path=dummy2
      endif
      if(dummy1.eq."mom_path")then
        mom_path=dummy2
      endif
     
C============Check file=======================
      if(dummy1.eq."debug_file")then
        debug_file=dummy2
        file_name=file_path(:index(file_path," ")-1)//
     &debug_file
        call check_file_exist(file_name)
        if(.not.file_exist)then
          write(*,*)"debug_file not found"
          stop
        endif
      endif
      if(dummy1.eq."wscreen_file")then
        wscreen_file=dummy2
        file_name=file_path(:index(file_path," ")-1)//
     &wscreen_file
        call check_file_exist(file_name)
         if(.not.file_exist)then
          write(*,*)"wscreen_file not found"
          stop
        endif
      endif
      if(dummy1.eq."unit_file")then
        unit_file=dummy2
        file_name=file_path(:index(file_path," ")-1)//
     &unit_file
        call check_file_exist(file_name)
        if(.not.file_exist)then
          write(*,*)"unit_file not found"
          stop
        endif
      endif
      if(dummy1.eq."periodic_file")then
        periodic_file=dummy2
        file_name=file_path(:index(file_path," ")-1)//
     &periodic_file
        call check_file_exist(file_name)
        if(.not.file_exist)then
          write(*,*)"periodic_file not found"
          stop
        endif
      endif
      if(dummy1.eq."version_file")then
        version_file=dummy2
        file_name=file_path(:index(file_path," ")-1)//
     &version_file
        call check_file_exist(file_name)
        if(.not.file_exist)then
          write(*,*)"version_file not found"
          stop
        endif

      endif
      if(dummy1.eq."list_changed_file")then
        list_changed_file=dummy2
        file_name=file_path(:index(file_path," ")-1)//
     &list_changed_file   !list file will be automatically generated and deleted!
C        call check_file_exist(file_name)
      endif
      if(dummy1.eq."life_file")then
        life_file=dummy2
        file_name=file_path(:index(file_path," ")-1)//
     &life_file
C        call check_file_exist(file_name)
C        if(.not.file_exist)then
C          write(*,*)"life_file not found"
C          stop
C        endif
      endif
      if(dummy1.eq."job_table_file")then
        job_table_file=dummy2
        file_name=file_path(:index(file_path," ")-1)//
     &job_table_file
        call check_file_exist(file_name)
        if(.not.file_exist)then
          write(*,*)"job_table_file not found"
          stop
        endif
      endif
      if(dummy1.eq."job_file")then
        job_file=dummy2
        file_name=file_path(:index(file_path," ")-1)//job_file
C        call check_file_exist(file_name)
C        if(.not.file_exist)then
C          write(*,*)"job_file not found"
C          stop
C        endif
      endif
      if(dummy1.eq."pes_file")then
        pes_file=dummy2
        file_name=file_path(:index(file_path," ")-1)//pes_file
        call check_file_exist(file_name)
        if(.not.file_exist)then
          write(*,*)"pes_file not found"
          stop
        endif
      endif
      if(dummy1.eq."parameter_file")then
        parameter_file=dummy2
        file_name=file_path(:index(file_path," ")-1)//parameter_file
        call check_file_exist(file_name)
        if(.not.file_exist)then
          write(*,*)"parameter_file not found"
          stop
        endif
      endif
      if(dummy1.eq."parameter_config_file")then
        parameter_config_file=dummy2
      endif
      if(dummy1.eq."parameter_hist_file")then
        parameter_hist_file=dummy2
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
