*========================================================================
* File Name : analysis_init.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 09時45分05秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine analysis_init
      implicit none
      include "../include/global_common.h"
      include "../include/common.h"
      include "../include/file.h"
      include "../include/life.h"
      include "../include/job.h"
      include "../include/ensemble.h"
      include "../include/pes.h"
      include "../include/BIMD/BIMD.h"
      include "../include/VACF/VACF.h"
      integer total_num1,total_num2
      character read_atom_name*4,dim_dummy1*10,dummy2*10,dummy3*17
      file_name=file_path(:index(file_path," ")-1)//job_file
      total_num1=6
      total_num2=6
C=================locate parameters======================
      call wait_till_file_close(file_name)
      open(20,file=file_name,status="old")
      do I0=1,job_line_num-1
        read(20,*)
      enddo
      read(20,*) atom_num,dim_dummy1,
     &((read_flag(I0),read_int(I0)),I0=1,total_num1),dummy3,
     &VACF_read_file_flag
      read(20,*) ((read_flag(total_num1+I0),
     &read_real8(total_num1+I0)),
     &I0=1,total_num2),dummy2,VACF_file_name
      do I0=1,total_num1+total_num2
        if(read_flag(I0).eq."job_ensemble=")ensemble_num=read_int(I0)
        if(read_flag(I0).eq."loop=")loop_num=read_int(I0)
        if(read_flag(I0).eq."pes_id=")pes_id=read_int(I0)
        if(read_flag(I0).eq."init_temp=")init_temp=read_real8(I0)
        if(read_flag(I0).eq."final_temp=")final_temp=read_real8(I0)
        if(read_flag(I0).eq."dTemp=")delta_temp=read_real8(I0)
        if(read_flag(I0).eq."observe_time_step=")
     &observe_time_step=dint(read_real8(I0))
        if(read_flag(I0).eq."window_time_step=")
     &window_time_step=dint(read_real8(I0))
        if(read_flag(I0).eq."delta_time=")delta_time=read_real8(I0)
      enddo
      do I1=1,atom_num
        I2=I1*3
        read(20,*) read_atom_name,x(I2-2),x(I2-1),x(I2)
        atom_name(I1)=read_atom_name
        if(I1.eq.1)then
          atom_name_a=read_atom_name
          atom_name_b=read_atom_name
          atom_num_a=1
          atom_num_b=0
        else if(I1.ge.2.and.read_atom_name.eq.atom_name_a)then
          atom_num_a=atom_num_a+1
        else if(I1.ge.2.and.read_atom_name.ne.atom_name_a)then
          atom_name_b=read_atom_name
          atom_num_b=atom_num_b+1
        endif
      enddo
      dim_fac=3
      ndim=atom_num*dim_fac
      ndim_a=atom_num_a*dim_fac
      ndim_b=atom_num_b*dim_fac
      if(atom_num.ne.(atom_num_a+atom_num_b))then
        write(*,*)
     &"Error, atom A=",atom_num_a,",and atom B=",atom_num_b
        write(*,*) "Not equal to total atom number=",atom_num
        close(20)
        stop
      endif
      if(wscreen)then
        write(*,"I5,1x,A18,1x,I4,A4,A1,1x,I4,1x,A4,A1,1x,I4")
     &myid,"Total atom number=",atom_num
     &,atom_name_a(:index(atom_name_a," ")-1),"=",
     &atom_num_a
     &,atom_name_b(:index(atom_name_b," ")-1),"=",
     &atom_num_b
        write(*,"I5,1x,A10,1x,F6.1,1x,A11,1x,F6.1,1x,A6,1x,F7.2,
     &1x,1x,A18,1x,I8,1x,A17,1x,I8,1x,A11,1x,F15.7")
     &myid,"init_temp=",init_temp,"final_temp=",final_temp,
     &"dTemp=",delta_temp,
     &"observe_time_step=",observe_time_step,
     &"window_time_step=",window_time_step,
     &"delta_time=",delta_time
      endif
      close(20)
      call init_parameter 
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
