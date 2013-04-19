*========================================================================
* File Name : VACF_read_xyz.f 
* Copyright (C) 2008-2011 Po-Jen Hsu <xanadu8850@pchome.com.tw>
* Creation Date : 19-04-2010
* Last Modified : 2010年04月20日 (週二) 10時02分45秒
* License : GPL (see bottom)
* Encoding : utf-8
* Project : sophAi
* Description :
* ========================================================================
      subroutine VACF_read_xyz_all
      implicit none 
      include "../../include/global_common.h"
      include "../../include/common.h"
      include "../../include/file.h"
      include "../../include/pes.h"
      include "../../include/BIMD/BIMD.h"
      include "../../include/VACF/VACF.h"
      character read_atom_name*2,dummy0*11
      file_name=VACF_input_name(:index(VACF_input_name," ")-1)      
      open(20,file=file_name,status="old")
      I6=0
      do I1=1,VACF_total_time_step
        read(*,*)
        read(*,*)
        do I2=1,atom_num
          I3=I2*3
          read(*,*) read_atom_name,x(I3-2),x(I3-1),x(I3),dummy0,
     &v(I3-2),v(I3-1),v(I3)
        enddo
        if(wscreen)write(*,*) "Read xyz file complete"
        call centre
        if(wscreen)write(*,*) "Fixed center of mass"
        call centre_velocity
        if(wscreen)write(*,*) "Fixed moment initia"
        call fix_angular
        if(wscreen)write(*,*) "Fixed angular momentum"
        do I4=1,atom_num
          I5=I4*3
          I6=I6+1
          xx(I6)=x(I5-2)
          xy(I6)=x(I5-1)
          xz(I6)=x(I5)
          vx(I6)=v(I5-2)
          vy(I6)=v(I5-1)
          vz(I6)=v(I5)
        enddo
      enddo
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
