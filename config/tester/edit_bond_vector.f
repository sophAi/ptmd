*========================================================================
* File Name : edit_bond_vector.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2011年04月13日 (週三) 12時57分10秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine edit_bond_vector
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/ensemble.h"
      include "../../include/job.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/bond_vector.h"
      integer id_select
      character dummy1*12,yn*1,bond_id_file_name*80,sel*2,sel2*2,sel3*2
      call edit_source
      atom_num=0
      sel="99"
      do while(sel.ne."0")
        write(*,*) "0.Quit"
        write(*,*) "1.Read bond vectors from a file"
        write(*,*) "2.Manually edit bond vectors"
        write(*,*) "3.Generate a bond vector template"
        write(*,*) "4.Loop condition"
        read(*,*) sel
        if(sel.eq."1")then
100       write(*,*) "Please input the file name of the source file"
          read(*,*) bond_id_file_name
          call check_file_exist(bond_id_file_name)
          if(.not.file_exist)then
            write(*,*) "File:",bond_id_file_name
            write(*,*) "doesn't exist. Please try again"
            write(*,*) 
     &"Note this file should be in the current directory"
            goto 100
          endif
          open(21,file=bond_id_file_name,status="old")
          read(21,*)dummy1,atom_num  !We use atom_num as the bond number for the input in GVACF
          do I0=1,atom_num
            read(21,*) bond_id(I0,1),bond_id(I0,2)
          enddo
          close(21)
          call show_bond_vector
        else if(sel.eq."2")then
          sel3="99"    
          do while(sel3.ne."0")
            call show_bond_vector
            write(*,*) "0.Quit"
            write(*,*) "1.Input a new bond vector"
            if(atom_num.ne.0)then
              write(*,*) "2.Edit a presented bond vector"
              write(*,*) "3.Remove a presented bond vector" 
              write(*,*) "4.Save the bond vectors to file"
            endif
            read(*,*) sel3
            if(sel3.eq."1")then
200           atom_num=atom_num+1
              write(*,*) 
     &"Input the k-1'th atomic id for new bond ",atom_num     
              read(*,*) bond_id(atom_num,1)
              write(*,*) 
     &"Input the k'th atomic id for new bond ",atom_num
              read(*,*) bond_id(atom_num,2)       
              call show_bond_vector
              write(*,*) "Creat another bond?(y/n)"
              read(*,*) yn
              if(yn.eq."y")goto 200
            else if(sel3.eq."2".and.atom_num.ne.0)then
300           write(*,*) "Edit: input a bond id"
              read(*,*) id_select
              if(id_select.gt.atom_num)then
                write(*,*) "This bond vector does not  exist"
                write(*,*) "You have to creat a new one first"
                call show_bond_vector
                goto 300
              endif
              write(*,*)
     &"Input the k-1'th atomic id for new bond ",id_select
              read(*,*) bond_id(id_select,1)
              write(*,*)
     &"Input the k'th atomic id for new bond ",id_select
              read(*,*) bond_id(id_select,2)
              call show_bond_vector
              write(*,*) "Edit another bond?(y/n)"
              read(*,*) yn
              if(yn.eq."y")goto 300
            else if(sel3.eq."3".and.atom_num.ne.0)then
400           write(*,*) "Remove: input a bond id"
              read(*,*) id_select
              do I1=id_select+1,atom_num
                bond_id(I1-1,1)=bond_id(I1,1)
                bond_id(I1-1,2)=bond_id(I1,2)
              enddo
              atom_num=atom_num-1
              call show_bond_vector
              write(*,*) "Remove another bond?(y/n)"
              read(*,*) yn
              if(yn.eq."y")goto 400
            endif
            if(sel3.eq."4".and.atom_num.ne.0)then
              write(*,*) "Please input the output file name"
              read(*,*) file_name
              write(*,*) "Save bond vector information to file?(y/n)"
              read(*,*) yn
              if(yn.eq."y")call save_bond_vector(file_name)
            endif
          enddo
        else if(sel.eq."3")then
          write(*,*) 
     &"Please input the file name for the bond vector template"
          read(*,*)source_file_name
          write(*,*) source_file_name
          open(22,file=source_file_name,status="replace")
          write(22,"(A13)") "total_bond= 2" 
          write(22,"(A5)") " 3  4"
          write(22,"(A5)") "10 11"
          close(22)
          write(*,*) "<File content>"
          write(*,"(A13)") "total_bond= 2"
          write(*,"(A42)") " 3  4  (Note! former_index < latter_index)"
          write(*,"(A31)") "10 11   (11 10 is unacceptable)"
          write(*,*) 
     &"This file has been written in the current directory"
          write(*,*) "Please modify the file and execute config again"
C          pause
          stop
        else if(sel.eq."4")then
          sel2="99"
          do while(sel2.ne."0")
            call show_parameter(0,"quit ")
            call show_parameter(1,"init_loop ")
            call show_parameter(2,"final_loop ")
            call show_parameter(3,"delta_loop ")
            write(*,*)"------------------------------------------"
            read(*,*) sel2
            if(sel2.eq."1")call edit_parameter("init_loop ")
            if(sel2.eq."2")call edit_parameter("final_loop ")
            if(sel2.eq."3")call edit_parameter("delta_loop ")
          enddo
        endif
      enddo
      return
      end

      subroutine show_bond_vector
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/pes.h"
      include "../../include/ensemble.h"
      include "../../include/file.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/bond_vector.h"
      integer bond_id_temp
      write(*,*) "Current bond vector information:"
      if(atom_num.eq.0)then
        write(*,*) "No bond vector yet!"
      else
        write(*,*) "Total bonds=",atom_num
        write(*,*) " Bond  k-1'th  k'th"
        do I0=1,atom_num
          if(bond_id(I0,1).gt.bond_id(I0,2))then
            bond_id_temp=bond_id(I0,1)
            bond_id(I0,1)=bond_id(I0,2)
            bond_id(I0,2)=bond_id_temp
          endif
          if(bond_id(I0,1).eq.bond_id(I0,2))then
            write(*,*) "Warning!!",I0,bond_id(I0,1),bond_id(I0,2)
            write(*,*) "There is no bond because two atom are the same!"
            write(*,*) "Must check again"
          else
            write(*,"(3(1x,I5))")I0,bond_id(I0,1),bond_id(I0,2)
          endif
        enddo
      endif
      do I0=1,atom_num
        I1=I0*3
        x(I1-2)=dble(bond_id(I0,1))
        x(I1-1)=dble(bond_id(I0,2))
        x(I1)  =0.D0
        atom_name(I0)="bv"
      enddo
      return
      end

      subroutine save_bond_vector(file_name)
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/pes.h"
      include "../../include/ensemble.h"
      include "../../include/file.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/bond_vector.h"
      open(21,file=file_name,status="replace")
      write(21,"(A11,1x,I5)") "total_bond=",atom_num
      do I0=1,atom_num
        write(21,"(I5,1x,I5)")bond_id(I0,1),bond_id(I0,2)
      enddo
      close(21)
      return
      end 

      subroutine write_bond_vector
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/life.h"
      include "../../include/job.h"
      include "../../include/pes.h"
      include "../../include/ensemble.h"
      include "../../include/file.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/bond_vector.h"
      file_name=file_path(:index(file_path," ")-1)//"tmp."//job_file
      open(20,file=file_name,access="append")
      write(20,"(I3,1x,I3,1x,
     &A10,1x,I3,1x,
     &A9,1x,I5,1x,
     &A10,1x,F6.1,1x,
     &A11,1x,F6.1,1x,
     &A11,1x,F7.2,1x,
     &A10,1x,F15.1,1x,
     &A11,1x,F15.1,1x,
     &A11,1x,F10.1,1x,
     &A17,1x,I2,1x,
     &A17,1x,I2,1x,
     &A12,1x,A20,1x,
     &A12,1x,A3,1x,
     &A17,1x,A3,1x,
     &A19,A80,A1,1x,
     &A24,1x,A3,1x,
     &A17,1x,A3,1x,
     &A19,A80,A1)")

     &10,7,
     &"tester_id=",tester_id,
     &"ndim_fac=",ndim_fac,
     &"init_temp=",init_temp,
     &"final_temp=",final_temp,
     &"delta_temp=",delta_temp,
     &"init_loop=",init_loop,
     &"final_loop=",final_loop,
     &"delta_loop=",delta_loop,
     &"source_file_flag=",source_file_flag,
     &"header_file_flag=",header_file_flag,
     &"tester_flag=",tester_flag,
     &"source_type=",source_type,
     &"source_file_type=",source_file_type,
     &"source_file_name= '",source_file_name,"'",
     &"header_file_source_type=",header_file_source_type,
     &"header_file_type=",header_file_type,
     &"header_file_name= '",header_file_name,"'"

      pes_content="none " 
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
