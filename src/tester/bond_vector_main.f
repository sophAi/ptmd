*========================================================================
* File Name : bond_vector_main.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2013-04-16 21:35:54
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine bond_vector_main
      implicit none
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/job.h"
      include "../../include/pes.h"
      include "../../include/simulation.h"
      include "../../include/tester/tester.h"
      include "../../include/tester/bond_vector.h"
      integer x_dim,y_dim,report_step,
     &skip_line_start,skip_line_end,read_atom_num   !skip_line_start and skip_line_end is applicable for both pdb and xyz format
      integer read_x_col,total_line_per_frame,read_bond_total_loop
      real*8 bond_norm
      character dummy(20)*1
      if(job_skip)return
C=======Read atom_num====================
      report_step=total_loop_int/10
      if(source_file_type.eq."xyz")then
        open(21,file=source_file_name,status="old")
        read(21,*)read_atom_num
        rewind(21)
        skip_line_start=2 !Atom_num ,Description_line
        skip_line_end=0  !None
        read_x_col=2 !Start from the 2nd column
        total_line_per_frame=read_atom_num+skip_line_start+skip_line_end
        ndim_fac=3
        file_x_dim=total_loop_int
        file_y_dim=bond_ndim
        atom_num=file_y_dim/ndim_fac
        ndim=file_y_dim
        simulation_delta_time=1
        simulation_ufv_loop=delta_loop
        call bond_vector_header("xyz")
      else if(source_file_type.eq."pdb")then
        call check_pdb(source_file_name,skip_line_start,
     &skip_line_end)  
        read_x_col=6     !Start from the 6th column
        read_atom_num=atom_num
        total_line_per_frame=read_atom_num+skip_line_start+skip_line_end
C        write(*,*) "skip=",skip_line_start,skip_line_end,atom_num
        ndim_fac=3
        file_x_dim=total_loop_int
        file_y_dim=bond_ndim
        atom_num=file_y_dim/ndim_fac
        ndim=file_y_dim
        simulation_delta_time=1
        simulation_ufv_loop=delta_loop
        call bond_vector_header("pdb") 
        open(21,file=source_file_name,status="old")
      else if(source_file_type.eq."ufx")then
        skip_line_start=0
        skip_line_end=0
        read_x_col=0
        total_line_per_frame=1
        call read_unformatted_header(header_file_name,"ndim ")
        call 
     &read_unformatted_header(header_file_name,"simulation_delta_time ")
        call 
     &read_unformatted_header(header_file_name,"simulation_ufx_loop ")
        read_atom_num=ndim/3
        if(read_bond_total_loop.lt.total_loop_int)then
          total_loop_int=read_bond_total_loop
          if(wscreen)write(*,"(I5,1x,A34,1x,I12)")
     &myid,"Warning!!Total loop exceed,fix to ",total_loop_int
        endif
        ndim_fac=3
        file_x_dim=total_loop_int
        file_y_dim=bond_ndim*3
        atom_num=file_y_dim/ndim_fac
        ndim=file_y_dim
        simulation_ufv_loop=delta_loop
        call bond_vector_header("ufx")
        open(21,file=source_file_name,form="unformatted",
     &status="old")
        read(21)  !skip the header line
      endif
      call creat_unformatted_header(bond_vector_file_name)
C======Start converting the bond vectors==
C======Jump to init_loop(can be >1)===
      do I0=1,init_loop_int-1
        do I1=1,total_line_per_frame
          if(source_file_type.eq."ufx")then
            read(21)
          else
            read(21,*)
          endif
        enddo 
      enddo
C=======Start from init_loop==========
      do I0=1,total_loop_int
        do I1=1,skip_line_start  ! Skip the header lines
          if(source_file_type.eq."ufx")then
            read(21)
          else
            read(21,*)
          endif
        enddo
C=======retrive Coordinates===========
        if(source_file_type.eq."ufx")then
          read(21) (x(I1),I1=1,read_atom_num*ndim_fac)
        else
          do I1=1,read_atom_num
            I2=I1*3
            read(21,*)
     &(dummy(I3),I3=1,read_x_col-1),x(I2-2),x(I2-1),x(I2)
          enddo
          if (mod(I0,report_step).eq.0.and.wscreen)
     &write(*,"(I5,1x,I13,1x,A6)") myid,I0," done!"
        endif
C=======construct bond_vector
        do I1=1,total_bond_num
          I3=bond_id(I1,2)*3
          I2=bond_id(I1,1)*3
          I4=I1*3
          v(I4-2)=x(I3-2)-x(I2-2)
          v(I4-1)=x(I3-1)-x(I2-1)
          v(I4)=x(I3)  -x(I2)
          bond_norm=
     &dsqrt(v(I4-2)**2+v(I4-1)**2+v(I4)**2)
          v(I4-2)=v(I4-2)/bond_norm
          v(I4-1)=v(I4-1)/bond_norm
          v(I4)=v(I4)/bond_norm
C          write(22,"(I10,3(1x,F13.7),2(1x,I4))")
C     &I1,bond_vector(1),bond_vector(2),bond_vector(3),bond_id(I1,1),
c     &bond_id(I1,2)
C          write(*,*) bond_id(I1,1),bond_id(I1,2),bond_norm
        enddo
        call write_v_unformatted(bond_vector_file_name)
C=====================================
        do I1=1,skip_line_end   ! Skip the end lines
          if(source_file_type.eq."ufx")then
            read(21)
          else
            read(21,*)
          endif
        enddo
        do I1=1,delta_loop_int-1 !Delta loop skip frames
          do I2=1,total_line_per_frame
            if(source_file_type.eq."ufx")then
              read(21)
            else
              read(21,*)
            endif
          enddo
        enddo
      enddo
      close(21)
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
