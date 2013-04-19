*========================================================================
* File Name : header.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 14-09-2010
* Last Modified : Tue 05 Apr 2011 03:20:10 PM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description : header information for ouput files
*========================================================================
      subroutine overwrite_header(file_name)  !will ignore header in the file and overwrite the headers
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/header.h"
      open(21,file=file_name,status="old")
      close(21)
      return
      end

      subroutine creat_basic_octave_header(file_name,access_method)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/header.h"
      character access_method*3
      if(access_method.eq."new")then
        open(21,file=file_name,status="replace")
      elseif(access_method.eq."app")then
        open(21,file=file_name,access="append")
      endif
      write(21,"(A14)")"# name: pes_id"
      write(21,"(A14)")"# type: scalar"
      write(21,"(I6)") pes_id
      write(21,"(A18)")"# name: time_label"
      write(21,"(A14)")"# type: scalar"
      write(21,"(F15.6)") time_label
      close(21)
      return
      end

      subroutine creat_octave_header(file_name,access_method)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/header.h"
      character access_method*3
      character par_type*25
      if(access_method.eq."new")then
        open(21,file=file_name,status="replace")
      else
        open(21,file=file_name,access="append")
      endif
      write(21,"(A8,A25)")"# name: ",
     &header_name(:index(header_name," ")-1)
      par_type=header_type(:index(header_type," ")-1)
      write(21,"(A8,A25)")"# type: ",par_type 
      if(par_type.eq."scalar".or.par_type.eq."complex scalar")then
        close(21)
        return
      elseif (par_type.eq."matrix".or.par_type.eq."complex matrix")then
        write(21,"(A8,I13)") "# rows: ",header_rows
        write(21,"(A11,I13)") "# columns: ",header_columns
      elseif (par_type.eq."string")then
        write(21,"(A12,I13)") "# elements: ",header_elements
        write(21,"(A10,I13)") "# length: ",header_length  !temporary support only one element
      endif
      close(21)
      return
      end

      subroutine octave_clear_header
      implicit none
      include "../include/global_common.h"
      include "../include/octave.h"
      octave_par_num=0
      do I0=1,octave_par_num_max     
        octave_par_line_num(I0)=0
        octave_size_rank(I0)=0
        do I1=1,octave_size_rank_max
          octave_size(I0,I1)=0
        enddo
      enddo
      end

      subroutine read_octave_header(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/header.h"
      include "../include/octave.h"
      integer octave_read_line,octave_read_value
      character read_header*25,header*25,dummy1*1,dummy2*4,
     &read_octave_name*25,read_octave_name2*25
      call octave_clear_header
      octave_read_line=0
      open(21,file=file_name,status="old")
101   read(21,*,end=100)
      read(21,*)dummy1
      octave_read_line=octave_read_line+1
      if(dummy1.eq."#")then
        backspace(21)
        read(21,*)dummy1,dummy2
        if(dummy2.eq."name")then
          backspace(21)
          read_octave_name="                         "
          read(21,*)dummy1,dummy2,read_octave_name
          octave_par_num=octave_par_num+1
          octave_par_name(octave_par_num)=
     &read_octave_name(:index(read_octave_name," ")-1)
          read_octave_name="                         "
          read(21,*)dummy1,dummy2,read_octave_name
          octave_par_type(octave_par_num)=
     &read_octave_name(:index(read_octave_name," ")-1)
          if(octave_par_type(octave_par_num).eq."scalar")then
            octave_read_line=octave_read_line+1
            octave_par_line_num(octave_par_num)=octave_read_line+1
            octave_size_rank(octave_par_num)=1
            octave_size(octave_par_num,1)=1
          else if(octave_par_type(octave_par_num).eq."matrix")then
            read(21,*)dummy1,dummy2,octave_read_value
            if(dummy2.eq."rows")then
              octave_read_line=octave_read_line+2
              octave_par_line_num(octave_par_num)=octave_read_line+1
              octave_size_rank(octave_par_num)=2
              octave_size(octave_par_num,1)=octave_read_value
              read(21,*)dummy1,dummy2,octave_read_value
              octave_size(octave_par_num,2)=octave_read_value
            else if(dummy2.eq."ndim")then
              octave_read_line=octave_read_line+1
              octave_par_line_num(octave_par_num)=octave_read_line+1
              octave_size_rank(octave_par_num)=octave_read_value
              read(21,*)(octave_size(octave_par_num,J0),
     &J0=1,octave_size_rank(octave_par_num))
            endif
          else if(octave_par_type(octave_par_num).eq."string".or.
     &octave_par_type(octave_par_num).eq."sq_string")then
            read(21,*)dummy1,dummy2,octave_read_value
            octave_read_line=octave_read_line+1  !But need to read the length description
            octave_par_line_num(octave_par_num)=octave_read_line+1
            octave_size_rank(octave_par_num)=1
            octave_size(octave_par_num,1)=octave_read_value
          else if(octave_par_type(octave_par_num).eq."complex")then
            backspace(21)
            read_octave_name="                        "
            read_octave_name2="                        "
            read(21,*)dummy1,dummy2,read_octave_name,read_octave_name2
            octave_par_type(octave_par_num)="complex_"//
     &read_octave_name2(:index(read_octave_name2," ")-1)
            if(read_octave_name2(:index(read_octave_name2," ")-1).
     &eq."scalar")then
              octave_read_line=octave_read_line+1
              octave_par_line_num(octave_par_num)=octave_read_line+1
              octave_size_rank(octave_par_num)=1
              octave_size(octave_par_num,1)=1
            else if(read_octave_name2(:index(read_octave_name2," ")-1).
     &eq."matrix")then
              read(21,*)dummy1,dummy2,octave_read_value
              if(dummy2.eq."rows")then
                octave_read_line=octave_read_line+2
                octave_par_line_num(octave_par_num)=octave_read_line+1
                octave_size_rank(octave_par_num)=2
                octave_size(octave_par_num,1)=octave_read_value
                read(21,*)dummy1,dummy2,octave_read_value
                octave_size(octave_par_num,2)=octave_read_value
              else if(dummy2.eq."ndim")then
                octave_read_line=octave_read_line+1
                octave_par_line_num(octave_par_num)=octave_read_line+1
                octave_size_rank(octave_par_num)=octave_read_value
                read(21,*)(octave_size(octave_par_num,J0),
     &J0=1,octave_size_rank(octave_par_num))
              endif
            endif
          endif
        endif
      endif
      goto 101
100   close(21)
      end


      subroutine creat_formatted_header(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/header.h"
      open(21,file=file_name,status="replace")
      write(21,
     &*)
     &"# ",header_source_type," ",header_par_num,
     &(" ",header_par_name(I0)," ",header_par_real(I0),
     &I0=1,header_par_num)," ",header_annotation
      close(21)
      return
      end


      subroutine show_formatted_header(file_name) !read header must leave a space
!     Read a parameter one at a time. The parameter will be covered only if the read_header for this parameter is assigned.
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/header.h"
      character dummy1*1
      header_found=.false.
      open(21,file=file_name,status="old")
      read(21,*)dummy1,header_source_type,header_par_num,
     &(header_par_name(I0),header_par_real(I0),I0=1,header_par_num),
     &header_annotation
      write(*,*)"File_type= ",header_source_type," ,par_num=",
     &header_par_num
      do I0=1,header_par_num
        write(*,*)I0,header_par_name(I0)," = ",header_par_real(I0)
      enddo
      write(*,*)"Annotation=  ",header_annotation
      close(21)
      return
      end

      subroutine read_formatted_header(file_name,read_header) !read header must leave a space
!     Read a parameter one at a time. The parameter will be covered only if the read_header for this parameter is assigned.
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/header.h"
      include "../include/tools/moment.h"
      include "../include/tester/corr.h"
      include "../include/tester/fourier.h"
      include "../include/parameter.h"
      character read_header*25,header*25
      character dummy1*1
      real*8 header_real8
      header_found=.false.
      open(21,file=file_name,status="old")
      read(21,*)dummy1,header_source_type,header_par_num,
     &(header_par_name(I0),header_par_real(I0),I0=1,header_par_num),
     &header_annotation
      close(21)
      header=read_header(:index(read_header," ")-1)
      if(header.eq."source_type")then
        source_type=header_source_type
        goto 99
      endif
      if(header.eq."annotation")then
       file_annotation=header_annotation
       goto 99
      endif
      do I1=1,header_par_num
        if(header_par_name(I1).eq.header.and.
     &header.eq."time_label")then
          time_label=header_par_real(I1)
          goto 99
        endif
        if(header_par_name(I1).eq.header.and.
     &header.eq."pes_id")then
          pes_id=dint(header_par_real(I1))
          goto 99
        endif
        if(header_par_name(I1).eq.header.and.
     &header.eq."file_x_dim")then
          file_x_dim=dint(header_par_real(I1))
          goto 99
        endif
        if(header_par_name(I1).eq.header.and.
     &header.eq."file_y_dim")then
          file_y_dim=dint(header_par_real(I1))
          goto 99
        endif
        if(header_par_name(I1).eq.header.and.
     &header.eq."file_z_dim")then
          file_z_dim=dint(header_par_real(I1))
          goto 99
        endif
        if(header_par_name(I1).eq.header.and.
     &header.eq."total_moment_x_num")then
          total_moment_x_num=dint(header_par_real(I1))
          goto 99
        endif
        if(header_par_name(I1).eq.header.and.
     &header.eq."total_moment_y_num")then
          total_moment_y_num=dint(header_par_real(I1))
          goto 99
        endif
        if(header_par_name(I1).eq.header.and.
     &header.eq."total_moment_z_num")then
          total_moment_z_num=dint(header_par_real(I1))
          goto 99
        endif
        if(header_par_name(I1).eq.header)then
          header_real8=header_par_real(I1)
          call update_parameter(header, header_real8)
          if(par_found)goto 99
        endif
      enddo
      write(*,"(I5,1x,A35,1x,A25)") 
     &myid,"Warning!! Cannot find this header= ",header
      stop
99    header_found=.true.
      header_output=header_par_real(I1)
      return
      end

      subroutine creat_unformatted_header(file_name)  !Write the information in the first line of unformatted file.
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/header.h"
      integer total_frame_num
      open(121,file=file_name,form="unformatted",status="replace")
      write(121)
     &"#",header_source_type,header_par_num,
     &(header_par_name(I0),header_par_real(I0),
     &I0=1,header_par_num),header_annotation
      close(121)
      return
      end

      subroutine show_unformatted_header(file_name) 
C     Read the information in the first line of unformatted file and update the value assigned by read_parameter.
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/BIMD/BIMD.h"
      include "../include/PTMC/PTMC.h"
      include "../include/header.h"
      integer total_frame_num
      character dummy1*1
      header_found=.false.
      open(20,file=file_name,form="unformatted",status="old")
      read(20)dummy1,header_source_type,header_par_num,
     &(header_par_name(I0),header_par_real(I0),I0=1,header_par_num)
     &,header_annotation
      write(*,*)"File type= ",header_source_type," ,par_num=",
     &header_par_num
      do I0=1,header_par_num
        write(*,*)I0,header_par_name(I0)," = ",header_par_real(I0)
      enddo
      write(*,*)"Annotation=  ",header_annotation
      close(20)
      return
      end

      subroutine read_unformatted_header(file_name,
     &read_header)  !Read the information in the first line of unformatted file and update the value assigned by read_parameter.
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/simulation.h"
      include "../include/BIMD/BIMD.h"
      include "../include/PTMC/PTMC.h"
      include "../include/header.h"
      include "../include/tools/moment.h"
      include "../include/tester/corr.h"
      include "../include/tester/fourier.h"
      integer total_frame_num
      character header*25,read_header*25
      character dummy1*1
      real*8 header_real8
      header_found=.false.
      open(20,file=file_name,form="unformatted",status="old")
      read(20)dummy1,header_source_type,header_par_num,
     &(header_par_name(I0),header_par_real(I0),I0=1,header_par_num)
     &,header_annotation
      close(20)
      header=read_header(:index(read_header," ")-1)
      if(header.eq."source_type")then
        source_type=header_source_type
        goto 99
      endif
      do I1=1,header_par_num
        if(header_par_name(I1).eq.header.and.
     &header.eq."time_label")then
          time_label=header_par_real(I1)
          goto 99
        endif
        if(header_par_name(I1).eq.header.and.
     &header.eq."pes_id")then
          pes_id=dint(header_par_real(I1))
          goto 99
        endif
        if(header_par_name(I1).eq.header.and.
     &header.eq."file_x_dim")then
          file_x_dim=dint(header_par_real(I1))
          goto 99
        endif
        if(header_par_name(I1).eq.header.and.
     &header.eq."file_y_dim")then
          file_y_dim=dint(header_par_real(I1))
          goto 99
        endif
       if(header_par_name(I1).eq.header.and.
     &header.eq."total_moment_y_num")then
          total_moment_y_num=dint(header_par_real(I1))
          goto 99
        endif
        if(header_par_name(I1).eq.header.and.
     &header.eq."total_moment_z_num")then
          total_moment_z_num=dint(header_par_real(I1))
          goto 99
        endif
        if(header_par_name(I1).eq.header)then
          header_real8=header_par_real(I1)
          call update_parameter(header,header_real8)
          goto 99
        endif
      enddo
      return
99    header_found=.true.
      header_output=header_par_real(I1)
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
