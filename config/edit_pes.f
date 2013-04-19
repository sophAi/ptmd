*========================================================================
* File Name : edit_pes.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Fri 25 Mar 2011 11:51:30 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_pes_type
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      integer pes_sel
      character pes_type_matrix(1000)*15
      common/pes_type_matrix0/pes_type_matrix
      call show_pes
      write(*,*) "Please select a pes for this job"
      read(*,*)pes_sel
      if(pes_sel.eq.0)then
        call initial_source
        return
      endif
      pes_type=pes_type_matrix(pes_sel)
102   write(*,*) "Please indicate a material for this job"
      read(*,*) pes_content
      pes_id=0
      call locate_current_pes_id
      if(error_input)goto 102
      return
      end

      subroutine edit_pes
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      character yn*1,yn2*1,yn3*1
99    format(I5,1x,A12,A3)
100   format(A10,1x,I5,1x,A3,1x,A5,1x,I5,1x,A3)
C      call locate_current_pes_id
C      write(*,*) "Locate pes id"
C      call locate_current_pes_type
C      write(*,*) "Locate pes type"
!If you want to read the pes data from correct line, 
!you have to do both locate_pes_id and locate_pes_type
      if(pes_type.eq."sophai")then

      else if(pes_type.eq."none ")then
        return
      else
        write(*,*) "1.Read the initial state from file"
        write(*,*) "2.Generate Radom number from 0 to 1"
        write(*,*) "3.Read from a set of coordinates"
        read(*,*) yn
        if(yn.eq."1")then
200       write(*,*) "Please input the global file name for reference"
          write(*,*) "(*.xyz or *.pdb)"
          read(*,*) file_name
          init_coord_file_name=file_name(:index(file_name," ")-1)
          call check_file_exist(file_name)
          if(.not.file_exist)then
            file_name=input_path(:index(input_path," ")-1)//file_name
            write(*,*) "Check ",file_name
            call check_file_exist(file_name)
            if(file_exist)then
              write(*,*) "Correctly locate the input file"
            else
              write(*,*) "File doesn't exist. Please try again"
              goto 200 
            endif
          endif
          call check_file_name(file_name,file_main,file_ext)
          if(file_ext.eq."xyz")then
            call read_xyz_file(file_name)
            call check_config_name
            write(*,*) "You are going to calculate:"
            if(atom_num.eq.atom_num_a)then
              write(*,99) atom_num_a," Pure metal ",
     &atom_name_a(:index(atom_name_a," ")-1)
            else
              write(*,100) "Alloy for ",atom_num_a,
     &atom_name_a(:index(atom_name_a," ")-1)," and ",
     &atom_num_b,atom_name_b(:index(atom_name_b," ")-1)
            endif
          elseif(file_ext.eq."pdb")then
            call read_pdb_file(file_name)
            call check_config_name
            write(*,*) "You are going to calculate:",file_name
            write(*,*) "With ",atom_num," atoms"
            config_name=file_main(:index(file_main," ")-1)
          endif
        else if(yn.eq."2".or.yn.eq."3")then
40        write(*,*) "Please input the total dimensions"
          write(*,*) "(ex:13 atoms=39 in 3D or 26 in 2D)"
          read(*,*) ndim
          write(*,*) "Please input the dimension(ex:2 for 2D,3 for 3D)"
          read(*,*)ndim_fac
          atom_num=ndim/ndim_fac
          write(*,*) "Please input the name of component A"
          read(*,*) atom_name_a
          write(*,*) "Please input the dimensions of A"
          read(*,*) ndim_a
          write(*,*) "Please input the name of component B"
          read(*,*) atom_name_b
          write(*,*) "Please input the dimensions of B"
          read(*,*) ndim_b
          write(*,*) "Please input the name of component C"
          read(*,*) atom_name_c
          write(*,*) "Please input the dimensions of C"
          read(*,*) ndim_c
          atom_num_a=ndim_a/ndim_fac
          atom_num_b=ndim_b/ndim_fac
          atom_num_c=ndim_c/ndim_fac
          if((ndim_a+ndim_b+ndim_c).ne.ndim)then
            write(*,*) "Your dimensions are incorrect!!"
            write(*,*) "Please input again"
            goto 40
          endif
          if(yn.eq."2")then
            do I0=1,ndim
              x(I0)=rnd()
            enddo
          endif
          if(yn.eq."3")then
254         write(*,*) "Please input the full name of the source file"
            read(*,*) file_name
            call check_file_exist(file_name)
            if(.not.file_exist)then
              write(*,*) "Please try again! Cannot locate ",file_name
              goto 254
            endif
            open(30,file=file_name,status="old")
            I1=1
301         read(30,*,end=300) x(I1*3-2),x(I1*3-1),x(I1*3)
            I1=I1+1
            goto 301
300         close(30)
            I1=I1-1
            write(*,*) "Detect atom number=",I1
            if(I1.lt.atom_num)then
              write(*,*) "Warning! The atom number is not correct!"
              atom_num=I1
              ndim=atom_num*3
            endif
          endif
          write(*,*) "Please type in the output file name(*.xyz)"
          read(*,*) file_name
          call check_file_exist(file_name)
          yn2="y"
          if(file_exist)then
            write(*,*) "Overwrite the old file? ",file_name
            write(*,*) "(y/n)"
            read(*,*) yn2
          endif
          if(yn2.eq."y")then
            open(20,file=file_name,status="replace")
            write(20,*) ndim/ndim_fac
            write(20,*) 
            do I1=1,ndim_a/ndim_fac
              I2=I1*3
              write(20,*)atom_name_a,x(I2-2),x(I2-1),x(I2)
            enddo
            do I1=ndim_a/ndim_fac+1,ndim_a/ndim_fac+ndim_b/ndim_fac
              I2=I1*3
              write(20,*)atom_name_b,x(I2-2),x(I2-1),x(I2)
            enddo
            do I1=ndim_b/ndim_fac+1,ndim_b/ndim_fac+ndim_c/ndim_fac
              I2=I1*3
              write(20,*)atom_name_c,x(I2-2),x(I2-1),x(I2)
            enddo
          endif
        endif
      endif
      return
      end

      subroutine write_pes
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      character yn*1
      if(pes_type.eq."sophai")then
      else if(pes_type.eq."sophai_sleep")then
      else
        file_name=file_path(:index(file_path," ")-1)//"tmp."//job_file
        open(20,file=file_name,access="append")
        if(use_top.eq.1)then
          do I0=1,atom_num
            I1=I0*3
            write(20,"(A4,1x,(3(F14.7)),1x,I3,100(1x,A3,1x,F5.3))") 
     &atom_name(I0),x(I1-2),x(I1-1),x(I1),top_num,
     &(top_name(I2),top_value(I2,I0),I2=1,top_num)
          enddo
        else
          do I0=1,atom_num
            I1=I0*3
            write(20,"(A4,1x,(3(F14.7)),1x,A1)")
     &atom_name(I0),x(I1-2),x(I1-1),x(I1),"0"
          enddo
        endif
        close(20)
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
