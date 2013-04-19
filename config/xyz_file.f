*========================================================================
* File Name : xyz_file.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : Tue 12 Apr 2011 09:57:18 AM CST
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine write_pot_unformatted(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/BIMD/BIMD.h"
      include "../include/PTMC/PTMC.h"
      open(20,file=file_name,form="unformatted",access="append")
      write(20) pot/dble(atom_num),(pot_per_atom(I0),I0=1,atom_num)
      close(20)
      return
      end

      subroutine write_e_unformatted(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/BIMD/BIMD.h"
      include "../include/PTMC/PTMC.h"
      open(20,file=file_name,form="unformatted",access="append")
      write(20) pot/dble(atom_num),(pot_per_atom(I0),I0=1,atom_num),
     &(kinetic)/dble(atom_num),(pot+kinetic)/dble(atom_num)
      close(20)
      return
      end


      subroutine write_x_unformatted(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/BIMD/BIMD.h"
      include "../include/PTMC/PTMC.h"
      open(20,file=file_name,form="unformatted",access="append")
      write(20) (x(I1),I1=1,ndim)
      close(20)
      return
      end

      subroutine write_g_unformatted(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/BIMD/BIMD.h"
      include "../include/PTMC/PTMC.h"
      open(20,file=file_name,form="unformatted",access="append")
      write(20) (grad(I1),I1=1,ndim)
      close(20)
      return
      end

      subroutine write_v_unformatted(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/BIMD/BIMD.h"
      include "../include/PTMC/PTMC.h"
      open(20,file=file_name,form="unformatted",access="append")
      write(20)(v(I1),I1=1,ndim)
      close(20)
      return
      end
 
      subroutine write_v_fixed_unformatted(file_name)
      !Fix center of mass velocity and angular velocity
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/tools/usr.h"
      include "../include/BIMD/BIMD.h"
      include "../include/PTMC/PTMC.h"
      real*8 v_fixed(ndim_max),x_fixed(ndim_max)
      real*8 ax,ay,az,total_angular,total_angular_moment
      real*8 angular_temp1,angular_temp2,angular_temp3
      real*8 rx,ry,rz,d(atom_num_max)
C==========Fix COM to x_fixed==============
      call centroid
      do I1=1,atom_num
        I2=I1*3
        x_fixed(I2-2)=x(I2-2)-com_x
        x_fixed(I2-1)=x(I2-1)-com_y
        x_fixed(I2)=x(I2)-com_z
      enddo
C==========End of COM x_fixed
C==========Fix COM velocity================
      call centroid_velocity
      do I1=1,atom_num
        I2=I1*3
        v_fixed(I2-2)=v(I2-2)-com_vx
        v_fixed(I2-1)=v(I2-1)-com_vy
        v_fixed(I2)=v(I2)-com_vz
      enddo
C===========End of fixing COM velocity
C===========Fix angular velocity============
      ax=0.D0
      ay=0.D0
      az=0.D0
      total_angular_moment=0.D0
      do I1=1,atom_num
        I2=I1*3
        ax=ax+mass(I1)*
     &(x_fixed(I2-1)*v_fixed(I2)-x_fixed(I2)*v_fixed(I2-1))
        ay=ay+mass(I1)*
     &(x_fixed(I2)*v_fixed(I2-2)-x_fixed(I2-2)*v_fixed(I2))
        az=az+mass(I1)*
     &(x_fixed(I2-2)*v_fixed(I2-1)-x_fixed(I2-1)*v_fixed(I2-2))
      enddo
      total_angular=dsqrt(ax**2+ay**2+az**2)
      ax=ax/total_angular
      ay=ay/total_angular
      az=az/total_angular
      do I1=1,atom_num
        I2=I1*3
        d(I1)=dsqrt((x_fixed(I2-1)*az-x_fixed(I2)*ay)**2
     &+(x_fixed(I2)*ax-x_fixed(I2-2)*az)**2
     &+(x_fixed(I2-2)*ay-x_fixed(I2-1)*ax)**2)
        total_angular_moment=
     &total_angular_moment+mass(I1)*d(I1)**2
      enddo
      total_angular_moment=total_angular/total_angular_moment
      do I1=1,atom_num
        I2=I1*3
        angular_temp1=
     &(x_fixed(I2-2)*ax+x_fixed(I2-1)*ay+x_fixed(I2)*az)/
     &total_angular**2
        rx=(x_fixed(I2-2)-ax*angular_temp1)/d(I1)
        ry=(x_fixed(I2-1)-ay*angular_temp1)/d(I1)
        rz=(x_fixed(I2)-az*angular_temp1)/d(I1)
C        ang_v(I2-2)=d(I1)*total_angular_moment*
C     &(rz*ay-ry*az)
C        ang_v(I2-1)=d(I1)*total_angular_moment*
C     &(rx*az-rz*ax)
C        ang_v(I2)=d(I1)*total_angular_moment*
C     &(ry*ax-rx*ay)
C        v_fixed(I2-2)=v_fixed(I2-2)-ang_v(I2-2)
C        v_fixed(I2-1)=v_fixed(I2-1)-ang_v(I2-1)
C        v_fixed(I2)=v_fixed(I2)-ang_v(I2)
         v_fixed(I2-2)=v_fixed(I2-2)+d(I1)*total_angular_moment*
     &(ry*az-rz*ay)
         v_fixed(I2-1)=v_fixed(I2-1)+d(I1)*total_angular_moment*
     &(rz*ax-rx*az)
         v_fixed(I2)=v_fixed(I2)+d(I1)*total_angular_moment*
     &(rx*ay-ry*ax)

      enddo
C=========End of fixing angular velocity==========
C=========Output v_fixed==========================
      open(20,file=file_name,form="unformatted",access="append")
      write(20)(v_fixed(I1),I1=1,ndim)
C     &,com_vx,com_vy,com_vz,
C     &(ang_v(I2),I2=1,ndim)
C      write(*,*)(v(I1),I1=1,ndim)
C      pause
C      write(*,*)(v_fixed(I1),I1=1,ndim)
C      pause
      close(20)
      return
      end

      

      subroutine write_xyz_file(file_name,access_method,frame_num,
     &total_frame_num,write_method)  !access_method=new -->clean the file,=app-->append to the existed file.
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/BIMD/BIMD.h"
      include "../include/PTMC/PTMC.h"
      include "../include/cn/cn.h"
      integer frame_num,total_frame_num,total_bonds,write_method !method=1 write velocity only,=2 write gradient only =0 simply writing =3 write velocity with CN_name
      character*3 access_method
      logical write_pes,write_grad,write_velocity
      if(access_method.eq."new")then
12      inquire(file=file_name
     &(:index(file_name," ")-1),opened=file_opened)
        if(file_opened) goto 12      !make sure no one is opening it
        open(20,file=file_name,status="replace")
      else if(access_method.eq."app")then
13      inquire(file=file_name
     &(:index(file_name," ")-1),opened=file_opened)
        if(file_opened) goto 13      !make sure no one is opening it
        open(20,file=file_name,access="append")
      endif
      total_bonds=0
      do I1=1,atom_num
        total_bonds=total_bonds+bond_num(I1)
      enddo
      if(write_method.eq.1)then
        write
     &(20,"(I4,1x,A6,1x,I13,1x,A12,1x,I13,1x,A4,
     &1x,A15,1x,A15)") 
     &atom_num,"frame=",frame_num,"total_frame=",total_frame_num,
     &"pes=",pes_type,pes_content
        write(20,"(A4,1x,F13.7,1x,A5,1x,F13.7,1x,A12,1x,
     &I5,1x,A4,F13.7,1x,A8,F13.7,1x,A2,1x,F10.5)") 
     &"Pot=",pot,"Bind=",pot/dble(atom_num),
     &"Total_bonds=",total_bonds,"RMS=",RMS,"Kinetic=",kinetic,"T=",
     &(2.D0*kinetic)/(3.D0*kb*dble(atom_num))
      else if(write_method.eq.2)then
        write
     &(20,"(I4,1x,A6,1x,I13,1x,A12,1x,I13,1x,A4,1x,A15,1x,A15)")
     &atom_num,"frame=",frame_num,"total_frame=",total_frame_num,
     &"pes=",pes_type,pes_content
        write(20,"(A4,1x,F13.7,1x,A5,1x,F13.7,1x
     &,A12,1x,I5,1x,A4,F13.7)")
     &"Pot=",pot,"Bind=",pot/dble(atom_num),
     &"Total_bonds=",total_bonds,"RMS=",RMS    
      else if(write_method.eq.3)then        ! write CN_name
        call cn
        call cn2name
        write
     &(20,"(I4,1x,A6,1x,I13,1x,A12,1x,I13,1x,A4,1x,
     &A15,1x,A15)")
     &atom_num,"frame=",frame_num,"total_frame=",total_frame_num,
     &"pes=",pes_type,pes_content
        write(20,"(A4,1x,F13.7,1x,A5,1x,F13.7,1x,A12,1x,I5,1x,A4,F13.7,
     &1x,A3,1x,A200)")
     &"pot=",pot,"Bind=",pot/dble(atom_num),
     &"Total_bonds=",total_bonds,"RMS=",RMS,"CN=",CN_name     
      else if(write_method.eq.4)then
        write(20,"(I4,1x,A6,1x,I13)")
     &atom_num,"frame=",frame_num
        write(20,"(A4,1x,F13.7)")"Pot=",pot
      else if(write_method.eq.0)then
        write(20,"(I4,1x,A6,1x,I13)")
     &atom_num,"frame=",frame_num
        write(20,"(A4,1x,F13.7)")"Pot=",pot
      endif
      do I1=1,atom_num
        I2=I1*3
        if(write_method.eq.1)then    !write_method=1, output x, and v
          write(20,"(A4,(3(1x,F13.7)),1x,A11,3(1x,F13.7))") 
     &atom_name(I1),x(I2-2),x(I2-1),x(I2),
     &"atom_vector",v(I2-2),v(I2-1),v(I2)
        else if(write_method.eq.2)then  !write_method=2, output x, g, pot(i), bond_num, and #
          write(20,"(A4,(3(1x,F10.4)),1x,A11,3(1x,F10.4),1x,
     &F12.7,1x,F5.0,1x,I4)")
     &atom_name(I1),x(I2-2),x(I2-1),x(I2),
     &"atom_vector",grad(I2-2),grad(I2-1),grad(I2),pot_per_atom(I1),
     &dint(bond_num(I1)),I1
        else if(write_method.eq.3)then  !write_method=3, output x, v, pot(i), bond_num, and #
          write(20,"(A4,(3(1x,F13.7)),1x,A11,3(1x,F13.7),1x,
     &F12.7,1x,F5.0,1x,I4)")
     &atom_name(I1),x(I2-2),x(I2-1),x(I2),
     &"atom_vector",v(I2-2),v(I2-1),v(I2),pot_per_atom(I1),
     &dint(bond_num(I1)),I1
        else if(write_method.eq.4)then   !write_method=4, output x, v, g, pot(i)
          write(20,"(A4,(3(1x,F13.6)),1x,A11,6(1x,F13.6),1x,F12.6)")
     &atom_name(I1),x(I2-2),x(I2-1),x(I2),
     &"atom_vector",v(I2-2),v(I2-1),v(I2),
     &grad(I2-2),grad(I2-1),grad(I2),
     &pot_per_atom(I1)
        else if(write_method.eq.0)then   !write_method=0, output x, v
          write(20,"(A4,(3(1x,F13.6)))")
     &atom_name(I1),x(I2-2),x(I2-1),x(I2)
        endif
      enddo 
      close(20)
      return
      end

      subroutine write_xyz_file_simple
     &(file_name,access_method,frame_num)  !access_method=new -->clean the file,=app-->append to the existed file.
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      include "../include/BIMD/BIMD.h"
      include "../include/PTMC/PTMC.h"
      integer frame_num,total_bonds
      character*3 access_method
      logical write_pes,write_grad,write_velocity
      if(access_method.eq."new")then
12      inquire(file=file_name
     &(:index(file_name," ")-1),opened=file_opened)
        if(file_opened) goto 12      !make sure no one is opening it
        open(20,file=file_name,status="replace")
      else if(access_method.eq."app")then
13      inquire(file=file_name
     &(:index(file_name," ")-1),opened=file_opened)
        if(file_opened) goto 13      !make sure no one is opening it
        open(20,file=file_name,access="append")
      endif
      total_bonds=0
      do I1=1,atom_num
        total_bonds=total_bonds+bond_num(I1)
      enddo
      write(20,"(I4,1x,A6,1x,I13)")
     &atom_num,"frame=",frame_num
      write(20,"(A4,1x,F13.7)")
     &"Pot=",pot
      do I1=1,atom_num
        I2=I1*3
        write(20,"(A4,(3(1x,F16.8)))")
     &atom_name(I1),x(I2-2),x(I2-1),x(I2)
      enddo
      close(20)
      return
      end



      subroutine write_xyz_ref_file(file_name,access_method,frame_num)  !access_method=new -->clean the file,=app-->append to the existed file.
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/BIMD/BIMD.h"
      include "../include/PTMC/PTMC.h"
      include "../include/pes.h"
      integer frame_num,total_bonds
      character*3 access_method
      logical write_pes,write_grad,write_velocity
      if(access_method.eq."new")then
12      inquire(file=file_name
     &(:index(file_name," ")-1),opened=file_opened)
        if(file_opened) goto 12      !make sure no one is opening it
        open(20,file=file_name,status="replace")
      else if(access_method.eq."app")then
13      inquire(file=file_name
     &(:index(file_name," ")-1),opened=file_opened)
        if(file_opened) goto 13      !make sure no one is opening it
        open(20,file=file_name,access="append")
      endif
      write(20,"(I5,1x,A6,1x,I10,1x,A17,1x,F10.0,1x,A11,1x,F12.7)") 
     &atom_num,"frame=",frame_num,"total_steps=",
     &(final_loop-init_loop+1.D0),"delta_time=",delta_time
      
      close(20)
      return
      end

      subroutine read_xyz_top_file(file_name,start_file_line)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      integer start_file_line,dummy_int,atom_flag
      real*8 dummy_real(3),second_real8
      character dummy_name,second_dummy*4
      call wait_till_file_close(file_name)
      open(20,file=file_name,status="old")
      do I0=1,start_file_line-1  !count in first two lines
        read(20,*)
      enddo
      read(20,*) atom_num
      read(20,*) second_dummy,second_real8
      if(second_dummy.eq."pot=".or.second_dummy.eq."Pot=".or.
     &second_dummy.eq."POT=")pot=second_real8
      read(20,*) dummy_name,dummy_real(1),dummy_real(2),dummy_real(3),
     &top_num
      backspace(20)
      if(top_num.gt.0)then
        do I1=1,atom_num
          I2=I1*3
          read(20,*) atom_name(I1),x(I2-2),x(I2-1),x(I2),dummy_int,
     &(top_name(I3),top_value(I3,I1),I3=1,top_num)
        enddo
        if(wscreen)then
          do I4=1,top_num
             write(*,"(I5,1x,A3,1x,I3,1x,A1,1x,A3)")
     &myid,"Top",I4,"=",top_name(I4)
          enddo
        endif
      else
        do I1=1,atom_num
          I2=I1*3
          read(20,*) atom_name(I1),x(I2-2),x(I2-1),x(I2)
        enddo
        if(wscreen)write(*,"(I5,1x,A24)")myid,"No topology restrictions"
      endif
      ndim_fac=3
      ndim=atom_num*ndim_fac
      call int2char4(atom_num,atom_num_name)
      if(wscreen)then
        write(*,"(I5,1x,A18,1x,I4)")
     &myid,"Total atom_number=",atom_num
      endif
      call top   !rearrange topology file
      close(20)
      return
      end

      subroutine read_xyz_file(file_name)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      integer frame_num,total_frame_num,read_frame,atom_flag
      real*8 second_real8
      character second_dummy*4
      logical manual
      file_line_num=0
      call check_file_exist(file_name)
      call count_file_line(file_name)
      open(20,file=file_name,status="old")
      read(20,*) atom_num
      rewind(20)
      total_frame_num=dint(dble(file_line_num)/dble(atom_num+2))
      if(total_frame_num.eq.1)then
        frame_num=1
      else
        write(*,*) "There are ", total_frame_num, " frame(s)"
        write(*,*) "Please select a frame"
        read(*,*) frame_num
        read_frame=0
        do while(read_frame.ne.frame_num-1)
          read_frame=read_frame+1
          do I1=1,atom_num+2
            read(20,*)        
          enddo  
        enddo
      endif
      read(20,*) atom_num
      read(20,*)second_dummy,second_real8
      if(second_dummy.eq."pot=".or.second_dummy.eq."Pot=".or.
     &second_dummy.eq."POT=")pot=second_real8
      do I1=1,atom_num
        I2=I1*3
        read(20,*) atom_name(I1),x(I2-2),x(I2-1),x(I2)
      enddo
      ndim_fac=3
      ndim=atom_num*ndim_fac
      close(20)
      return
      end
 
      subroutine check_config_name
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      integer config_name_flag
      integer hetroatom_num,hetroatom_id(atom_num_max)
      integer hetroatom_num2,hetroatom_id2(atom_num_max)
      config_name_flag=1   !1 for pure; 2 for alloy; 3 for more than 2 types of atoms
      atom_name_a=atom_name(1)
      atom_num_a=1
      hetroatom_num=0
      do I0=2,atom_num
        if(atom_name(I0).eq.atom_name_a)then
          atom_num_a=atom_num_a+1
        else
          hetroatom_num=hetroatom_num+1
          hetroatom_id(hetroatom_num)=I0
        endif
      enddo
      if(hetroatom_num.eq.0)then
        atom_num_b=0
        atom_name_b=atom_name(1)
        atom_num_c=0
        atom_name_c=atom_name(1)
      else
        atom_num_b=1
        atom_name_b=atom_name(hetroatom_id(1))
        config_name_flag=2
      endif
      hetroatom_num2=0
      do I0=2,hetroatom_num
        if(atom_name(hetroatom_id(I0)).eq.atom_name_b)then
          atom_num_b=atom_num_b+1
        else
          hetroatom_num2=hetroatom_num2+1
          hetroatom_id2(hetroatom_num2)=hetroatom_id(I0)
        endif
      enddo
      if(hetroatom_num2.eq.0)then
        atom_num_c=0
        atom_name_c=atom_name_b
      else
        atom_num_c=atom_num-atom_num_a-atom_num_b
        atom_name_c=atom_name(hetroatom_id2(1))
        config_name_flag=3
      endif
      call int2char4(atom_num,atom_num_name)
      call int2char4(atom_num_a,atom_num_name_a)
      call int2char4(atom_num_b,atom_num_name_b)
      call int2char4(atom_num_c,atom_num_name_c)
      if(config_name_flag.eq.2)then
        config_name=atom_name_a(:index(atom_name_a," ")-1)//
     &atom_num_name_a//atom_name_b(:index(atom_name_b," ")-1)//
     &atom_num_name_b
      else
        config_name=pes_content(:index(pes_content," ")-1)//
     &atom_num_name
      endif
      return
      end

      subroutine read_xyz_velocity_frame(file_name,frame_num)
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/pes.h"
      integer frame_num,total_frame_num,read_frame
      real*8 second_real8
      character atom_vector_char*11,second_dummy*4
      logical manual
      file_line_num=0
      call check_file_exist(file_name)
      call count_file_line(file_name)
      open(20,file=file_name,status="old")
      atom_num=file_line_num-2
      total_frame_num=dint(dble(file_line_num)/dble(atom_num+2))
      if(total_frame_num.eq.1)then
        frame_num=1
      else
        write(*,*) "There are ", total_frame_num, " frame(s)"
        read_frame=0
        do while(read_frame.ne.frame_num-1)
          read_frame=read_frame+1
          do I1=1,atom_num+2
            read(20,*)
          enddo
        enddo
      endif
      read(20,*) atom_num
      read(20,*) second_dummy,second_real8
      if(second_dummy.eq."pot=".or.second_dummy.eq."Pot=".or.
     &second_dummy.eq."POT=")pot=second_real8
      do I1=1,atom_num
        I2=I1*3
        read(20,*) atom_name(I1),x(I2-2),x(I2-1),x(I2),
     &atom_vector_char,v(I2-2),v(I2-1),v(I2)
      enddo
      close(20)
      ndim_fac=3
      ndim=atom_num*ndim_fac
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
