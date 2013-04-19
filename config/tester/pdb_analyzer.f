*========================================================================
* File Name : edit_xyz_analyzer.f
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 23-09-2010
* Last Modified : Thu 21 Apr 2011 11:27:26 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : 
* Description : 
*========================================================================

      program pdb_water_analyzer
      implicit none
      integer atom_num,I0,I1,I2,I3,J0,J1,J2,J3,crt_id(6000),
     &skip_line_start,skip_line_end,index_temp(2000,4),dist_crt(5,2)
      real*8 dist_temp(2000),x(6000),ctd_x,ctd_y,ctd_z,total_mass
      character sel*1,sel2*1,dummy(10)*3,file_name*80
98    write(*,*) "1.Read formatted file"
      write(*,*) "2.Read unformatted file"
      read(*,*)sel
      write(*,*) "Please input the full file name:"
      read(*,*)file_name
      if(sel.eq."1")then   
        write(*,*) "1. xyz"
        write(*,*) "2. pdb"
        read(*,*) sel2
        write(*,*) "Please input number of residue"
        read(*,*) atom_num
        if(sel2.eq."1")then
C          call read_xyz_file(file_name)
        else if (sel2.eq."2")then
C          call read_pdb_file(file_name)
          open(20,file=file_name,status="old")
          skip_line_start=186
          do I1=1,skip_line_start
            read(20,*)
          enddo
          I3=181
          do I1=1,(atom_num-181)/4
            I3=I3+1
            I2=I1*3
            read(20,*) (dummy(J3),J3=1,5),x(I2-2),x(I2-1),x(I2)
            write(*,*) I3
            index_temp(I1,1)=I3
            do J1=2,4
              I3=I3+1
              read(20,*)
              index_temp(I1,J1)=I3
            enddo
          enddo
          close(20)
        endif
      endif
      ctd_x=0.D0
      ctd_y=0.D0
      ctd_z=0.D0
      total_mass=0.D0
      do J0=1,(atom_num-181)/4
        J1=J0*3
        ctd_x=ctd_x+x(J1-2)
        ctd_y=ctd_y+x(J1-1)
        ctd_z=ctd_z+x(J1)
        total_mass=total_mass+1
      enddo
      ctd_x=ctd_x/total_mass
      ctd_y=ctd_y/total_mass
      ctd_z=ctd_z/total_mass
      write(*,*) "COM:",ctd_x,ctd_y,ctd_z
      write(*,*) "Will output the atomic distance for each atom"
      open(30,file="output.dat",status="replace")
      do I0=1,(atom_num-181)/4
        I1=I0*3
        dist_temp(I0)=dsqrt((x(I1-2)-ctd_x)**2+(x(I1-1)-ctd_y)**2+
     &(x(I1)-ctd_z)**2)
        write(30,*) I0,dist_temp(I0),(index_temp(I0,I2),I2=1,4)
      enddo
      close(30)
      open(31,file="index.ndx",status="replace")
      dist_crt(1,1)=2
      dist_crt(1,2)=13
      dist_crt(2,1)=13
      dist_crt(2,2)=16
      dist_crt(3,1)=16
      dist_crt(3,2)=19
      dist_crt(4,1)=19
      dist_crt(4,2)=22
      dist_crt(5,1)=22
      dist_crt(5,2)=30
      do I0=1,5
        J3=0
        write(31,"(A2,I2,A1,I2,A2)")
     &"[ ",dist_crt(I0,1),"-",dist_crt(I0,2)," ]"
        do I1=1,(atom_num-181)/4
          if(dist_temp(I1).ge.dble(dist_crt(I0,1)).and.
     &dist_temp(I1).lt.dble(dist_crt(I0,2)))then
            J3=J3+1
            crt_id(J3)=index_temp(I1,1)
            J3=J3+1
            crt_id(J3)=index_temp(I1,2)
            J3=J3+1
            crt_id(J3)=index_temp(I1,3)
            J3=J3+1
            crt_id(J3)=index_temp(I1,4)       
          endif   
        enddo
        I2=(J3)/15
        J2=mod((J3),15)
        do I1=1,I2-1
          write(31,"(15(I4,1x))")(crt_id(J1),J1=(I1-1)*15+1,I1*15)
        enddo
        write(31,"(15(I4,1x))")
     &(crt_id(J1),J1=(I2-1)*15+1,(I2-1)*15+1+J2)
      enddo
      close(31)
      stop
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
